{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Wasm.Interpreter
  ( Value (..),
    Store,
    ModuleInstance (..),
    ExternalValue (..),
    ExportInstance (..),
    GlobalInstance (..),
    Imports,
    HostItem (..),
    instantiate,
    invoke,
    invokeExport,
    getGlobalValueByName,
    emptyStore,
    emptyImports,
    makeHostModule,
    makeMutGlobal,
  )
where

import qualified Control.Monad as Monad
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bits
  ( Bits,
    countLeadingZeros,
    countTrailingZeros,
    popCount,
    rotateL,
    rotateR,
    shiftL,
    shiftR,
    xor,
    (.&.),
    (.|.),
  )
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as Vector
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as IOVector
import Data.Word (Word16, Word32, Word64, Word8)
import Language.Wasm.FloatUtils
  ( doubleToWord,
    floatToWord,
    wordToDouble,
    wordToFloat,
  )
import Language.Wasm.Structure as Struct
import Language.Wasm.Validate as Valid
import Numeric.IEEE (IEEE, copySign, identicalIEEE, maxNum, minNum)
import Numeric.Natural (Natural)

data Value
  = VI32 Word32
  | VI64 Word64
  | VF32 Float
  | VF64 Double
  | VHandle Word32 Word32 Word32 Bool
  deriving (Eq, Show, Ord)

data Type
  = TI32
  | TI64
  | TF32
  | TF64
  | THandle
  deriving (Eq, Show, Ord)

-- MS-Wasm segment memory handling
data SegmentType = SegData | SegHandle
  deriving (Eq, Show)

data SegmentMemory = SegmentMemory
  { segments :: Map.Map Value (SegmentType, Value),
    size :: Int32
  }
  deriving (Eq, Show)

loadFromSegment :: SegmentMemory -> Value -> Maybe Value
loadFromSegment mem key =
  case key of
    VHandle x y z b ->
      case Map.lookup key (segments mem) of
        Just (segType, val) -> case segType of
          SegData -> case val of
            VHandle x y z b -> Just $ VHandle x y z True
            _ -> Just val
          _ -> Just val
        Nothing -> Nothing
    _ -> Nothing

storeToSegment :: SegmentMemory -> Value -> Value -> Maybe SegmentMemory
storeToSegment mem key val =
  case key of
    VHandle x y z b -> case val of
      VHandle t u v w ->
        Just
          SegmentMemory
            { segments = (Map.insert key (SegHandle, val) (segments mem)),
              size = size mem
            }
      _ ->
        Just
          SegmentMemory
            { segments = (Map.insert key (SegData, val) (segments mem)),
              size = size mem
            }
    _ -> Nothing

freeSegment :: SegmentMemory -> Value -> Maybe SegmentMemory
freeSegment mem key =
  case key of
    VHandle x y z b ->
      if Map.member key (segments mem)
        then
          Just
            SegmentMemory
              { segments = Map.delete key (segments mem),
                size = (size mem)
              }
        else Nothing
    _ -> Nothing

newSegment :: SegmentMemory -> Value -> Value -> Maybe SegmentMemory
newSegment mem key val =
  case key of
    VHandle x y z b ->
      Just
        SegmentMemory
          { segments =
              ( Map.insertWith
                  (\new old -> old)
                  key
                  (SegData, val)
                  (segments mem)
              ),
            size = (size mem) + asInt32 (z - x)
          }
    _ -> Nothing

sliceSegment :: Value -> Int32 -> Int32 -> Maybe Value
sliceSegment (VHandle x y z b) base bound =
  Just $ (VHandle (asWord32 $ (asInt32 x + base)) y (asWord32 $ (asInt32 z - bound)) b)
sliceSegment _ _ _ = Nothing

isInvalidHandle :: Value -> Bool
isInvalidHandle (VHandle x y z b) = (x + y > z) || b
isInvalidHandle _ = False

evalSegmentLoad :: EvalCtx -> IO EvalResult
evalSegmentLoad ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem}
  = if isInvalidHandle (VHandle w x y z)
      then return Trap
      else
        let result = loadFromSegment segmem (VHandle w x y z)
         in case result of
              Just val -> return $ Done ctx {stack = val : rest}
              Nothing -> return Trap
evalSegmentLoad _
  = return Trap

evalSegmentStore :: EvalCtx -> IO EvalResult
evalSegmentStore ctx@EvalCtx {stack = (VI32 v : VHandle w x y z : rest), segmem}
  = if isInvalidHandle (VHandle w x y z)
      then return Trap
      else
        let result = storeToSegment segmem (VHandle w x y z) (VI32 v)
         in case result of
              Just mem -> return $ Done ctx {stack = rest, segmem = mem}
              Nothing -> return Trap
evalSegmentStore ctx@EvalCtx {stack = (VI64 v : VHandle w x y z : rest), segmem}
  = if isInvalidHandle (VHandle w x y z)
      then return Trap
      else
        let result = storeToSegment segmem (VHandle w x y z) (VI64 v)
         in case result of
              Just mem -> return $ Done ctx {stack = rest, segmem = mem}
              Nothing -> return Trap
evalSegmentStore ctx@EvalCtx {stack = (VF32 v : VHandle w x y z : rest), segmem}
  = if isInvalidHandle (VHandle w x y z)
      then return Trap
      else
        let result = storeToSegment segmem (VHandle w x y z) (VF32 v)
         in case result of
              Just mem -> return $ Done ctx {stack = rest, segmem = mem}
              Nothing -> return Trap
evalSegmentStore ctx@EvalCtx {stack = (VF64 v : VHandle w x y z : rest), segmem}
  = if isInvalidHandle (VHandle w x y z)
      then return Trap
      else
        let result = storeToSegment segmem (VHandle w x y z) (VF64 v)
         in case result of
              Just mem -> return $ Done ctx {stack = rest, segmem = mem}
              Nothing -> return Trap
evalSegmentStore _
  = return Trap

-- end MS-Wasm interpreter functions

asInt32 :: Word32 -> Int32
asInt32 w =
  if w < 0x80000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFF - w + 1)

asInt64 :: Word64 -> Int64
asInt64 w =
  if w < 0x8000000000000000
    then fromIntegral w
    else -1 * fromIntegral (0xFFFFFFFFFFFFFFFF - w + 1)

asWord32 :: Int32 -> Word32
asWord32 i
  | i >= 0 = fromIntegral i
  | otherwise = 0xFFFFFFFF - (fromIntegral (abs i)) + 1

asWord64 :: Int64 -> Word64
asWord64 i
  | i >= 0 = fromIntegral i
  | otherwise = 0xFFFFFFFFFFFFFFFF - (fromIntegral (abs i)) + 1

nearest :: (IEEE a) => a -> a
nearest f
  | isNaN f = f
  | f >= 0 && f <= 0.5 = copySign 0 f
  | f < 0 && f >= -0.5 = -0
  | otherwise =
    let i = floor f :: Integer
     in let fi = fromIntegral i
         in let r = abs f - abs fi
             in flip copySign f $
                  ( if r == 0.5
                      then
                        ( case (even i, f < 0) of
                            (True, _) -> fi
                            (_, True) -> fi - 1.0
                            (_, False) -> fi + 1.0
                        )
                      else fromIntegral (round f :: Integer)
                  )

zeroAwareMin :: IEEE a => a -> a -> a
zeroAwareMin a b
  | identicalIEEE a 0 && identicalIEEE b (-0) = b
  | isNaN a = a
  | isNaN b = b
  | otherwise = minNum a b

zeroAwareMax :: IEEE a => a -> a -> a
zeroAwareMax a b
  | identicalIEEE a (-0) && identicalIEEE b 0 = b
  | isNaN a = a
  | isNaN b = b
  | otherwise = maxNum a b

floatFloor :: Float -> Float
floatFloor a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (floor a :: Integer)) a

doubleFloor :: Double -> Double
doubleFloor a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (floor a :: Integer)) a

floatCeil :: Float -> Float
floatCeil a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (ceiling a :: Integer)) a

doubleCeil :: Double -> Double
doubleCeil a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (ceiling a :: Integer)) a

floatTrunc :: Float -> Float
floatTrunc a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (truncate a :: Integer)) a

doubleTrunc :: Double -> Double
doubleTrunc a
  | isNaN a = a
  | otherwise = copySign (fromIntegral (truncate a :: Integer)) a

data Label = Label ResultType deriving (Show, Eq)

type Address = Int

data TableInstance = TableInstance
  { lim :: Limit,
    elements :: Vector (Maybe Address)
  }

data MemoryInstance = MemoryInstance
  { lim :: Limit,
    memory :: IORef (IOVector Word8)
  }

data GlobalInstance = GIConst ValueType Value | GIMut ValueType (IORef Value)

makeMutGlobal :: Value -> IO GlobalInstance
makeMutGlobal val = GIMut (getValueType val) <$> newIORef val

getValueType :: Value -> ValueType
getValueType (VI32 _) = I32
getValueType (VI64 _) = I64
getValueType (VF32 _) = F32
getValueType (VF64 _) = F64
getValueType (VHandle _ _ _ _) = Handle

data ExportInstance = ExportInstance TL.Text ExternalValue deriving (Eq, Show)

data ExternalValue
  = ExternFunction Address
  | ExternTable Address
  | ExternMemory Address
  | ExternGlobal Address
  deriving (Eq, Show)

data FunctionInstance
  = FunctionInstance
      { funcType :: FuncType,
        moduleInstance :: ModuleInstance,
        code :: Function
      }
  | HostInstance
      { funcType :: FuncType,
        hostCode :: HostFunction
      }

data Store = Store
  { funcInstances :: Vector FunctionInstance,
    tableInstances :: Vector TableInstance,
    memInstances :: Vector MemoryInstance,
    globalInstances :: Vector GlobalInstance
  }

emptyStore :: Store
emptyStore =
  Store
    { funcInstances = Vector.empty,
      tableInstances = Vector.empty,
      memInstances = Vector.empty,
      globalInstances = Vector.empty
    }

type HostFunction = [Value] -> IO [Value]

data HostItem
  = HostFunction FuncType HostFunction
  | HostGlobal GlobalInstance
  | HostMemory Limit
  | HostTable Limit

makeHostModule :: Store -> [(TL.Text, HostItem)] -> IO (Store, ModuleInstance)
makeHostModule st items = do
  (st, emptyModInstance)
    |> makeHostFunctions
    |> makeHostGlobals
    |> makeHostMems
    >>= makeHostTables
  where
    (|>) = flip ($)

    makeHostFunctions :: (Store, ModuleInstance) -> (Store, ModuleInstance)
    makeHostFunctions (st, inst) =
      let funcLen = Vector.length $ funcInstances st
       in let (names, types, instances) = unzip3 [(name, t, HostInstance t c) | (name, (HostFunction t c)) <- items]
           in let exps = Vector.fromList $ zipWith (\name i -> ExportInstance name (ExternFunction i)) names [funcLen ..]
               in let inst' =
                        inst
                          { funcTypes = Vector.fromList types,
                            funcaddrs = Vector.fromList [funcLen .. funcLen + length instances - 1],
                            exports = Language.Wasm.Interpreter.exports inst <> exps
                          }
                   in let st' = st {funcInstances = funcInstances st <> Vector.fromList instances}
                       in (st', inst')

    makeHostGlobals :: (Store, ModuleInstance) -> (Store, ModuleInstance)
    makeHostGlobals (st, inst) =
      let globLen = Vector.length $ globalInstances st
       in let (names, instances) = unzip [(name, g) | (name, (HostGlobal g)) <- items]
           in let exps = Vector.fromList $ zipWith (\name i -> ExportInstance name (ExternGlobal i)) names [globLen ..]
               in let inst' =
                        inst
                          { globaladdrs = Vector.fromList [globLen .. globLen + length instances - 1],
                            exports = Language.Wasm.Interpreter.exports inst <> exps
                          }
                   in let st' = st {globalInstances = globalInstances st <> Vector.fromList instances}
                       in (st', inst')

    makeHostMems :: (Store, ModuleInstance) -> IO (Store, ModuleInstance)
    makeHostMems (st, inst) = do
      let memLen = Vector.length $ memInstances st
      let (names, limits) = unzip [(name, Memory lim) | (name, (HostMemory lim)) <- items]
      instances <- allocMems limits
      let exps = Vector.fromList $ zipWith (\name i -> ExportInstance name (ExternMemory i)) names [memLen ..]
      let inst' =
            inst
              { memaddrs = Vector.fromList [memLen .. memLen + length instances - 1],
                exports = Language.Wasm.Interpreter.exports inst <> exps
              }
      let st' = st {memInstances = memInstances st <> instances}
      return (st', inst')

    makeHostTables :: (Store, ModuleInstance) -> IO (Store, ModuleInstance)
    makeHostTables (st, inst) = do
      let tableLen = Vector.length $ tableInstances st
      let (names, tables) = unzip [(name, Table (TableType lim AnyFunc)) | (name, (HostTable lim)) <- items]
      let instances = allocTables tables
      let exps = Vector.fromList $ zipWith (\name i -> ExportInstance name (ExternTable i)) names [tableLen ..]
      let inst' =
            inst
              { tableaddrs = Vector.fromList [tableLen .. tableLen + length instances - 1],
                exports = Language.Wasm.Interpreter.exports inst <> exps
              }
      let st' = st {tableInstances = tableInstances st <> instances}
      return (st', inst')

data ModuleInstance = ModuleInstance
  { funcTypes :: Vector FuncType,
    funcaddrs :: Vector Address,
    tableaddrs :: Vector Address,
    memaddrs :: Vector Address,
    globaladdrs :: Vector Address,
    exports :: Vector ExportInstance
  }
  deriving (Eq, Show)

emptyModInstance :: ModuleInstance
emptyModInstance =
  ModuleInstance
    { funcTypes = Vector.empty,
      funcaddrs = Vector.empty,
      tableaddrs = Vector.empty,
      memaddrs = Vector.empty,
      globaladdrs = Vector.empty,
      exports = Vector.empty
    }

calcInstance :: Store -> Imports -> Module -> Initialize ModuleInstance
calcInstance (Store fs ts ms gs) imps Module {functions, types, tables, mems, globals, exports, imports} = do
  let funLen = length fs
  let tableLen = length ts
  let memLen = length ms
  let globalLen = length gs
  funImps <- mapM checkImportType $ filter isFuncImport imports
  tableImps <- mapM checkImportType $ filter isTableImport imports
  memImps <- mapM checkImportType $ filter isMemImport imports
  globalImps <- mapM checkImportType $ filter isGlobalImport imports
  let funs = Vector.fromList $ map (\(ExternFunction i) -> i) funImps ++ [funLen .. funLen + length functions - 1]
  let tbls = Vector.fromList $ map (\(ExternTable i) -> i) tableImps ++ [tableLen .. tableLen + length tables - 1]
  let memories = Vector.fromList $ map (\(ExternMemory i) -> i) memImps ++ [memLen .. memLen + length mems - 1]
  let globs = Vector.fromList $ map (\(ExternGlobal i) -> i) globalImps ++ [globalLen .. globalLen + length globals - 1]
  let refExport (Export name (ExportFunc idx)) =
        ExportInstance name $ ExternFunction $ funs ! fromIntegral idx
      refExport (Export name (ExportTable idx)) =
        ExportInstance name $ ExternTable $ tbls ! fromIntegral idx
      refExport (Export name (ExportMemory idx)) =
        ExportInstance name $ ExternMemory $ memories ! fromIntegral idx
      refExport (Export name (ExportGlobal idx)) =
        ExportInstance name $ ExternGlobal $ globs ! fromIntegral idx
  return $
    ModuleInstance
      { funcTypes = Vector.fromList types,
        funcaddrs = funs,
        tableaddrs = tbls,
        memaddrs = memories,
        globaladdrs = globs,
        exports = Vector.fromList $ map refExport exports
      }
  where
    getImpIdx :: Import -> Initialize ExternalValue
    getImpIdx (Import m n _) =
      case Map.lookup (m, n) imps of
        Just idx -> return idx
        Nothing -> throwError $ "Cannot find import from module " ++ show m ++ " with name " ++ show n

    checkImportType :: Import -> Initialize ExternalValue
    checkImportType imp@(Import _ _ (ImportFunc typeIdx)) = do
      idx <- getImpIdx imp
      funcAddr <- case idx of
        ExternFunction funcAddr -> return funcAddr
        other -> throwError "incompatible import type"
      let expectedType = types !! fromIntegral typeIdx
      let actualType = Language.Wasm.Interpreter.funcType $ fs ! funcAddr
      if expectedType == actualType
        then return idx
        else throwError "incompatible import type"
    checkImportType imp@(Import _ _ (ImportGlobal globalType)) = do
      let err = throwError "incompatible import type"
      idx <- getImpIdx imp
      globalAddr <- case idx of
        ExternGlobal globalAddr -> return globalAddr
        _ -> err
      let globalInst = gs ! globalAddr
      let vt = case globalType of
            Const vt -> vt
            Mut vt -> vt
      let vt' = case globalInst of
            GIConst vt _ -> vt
            GIMut vt _ -> vt
      if vt == vt' then return idx else err
    checkImportType imp@(Import _ _ (ImportMemory limit)) = do
      idx <- getImpIdx imp
      memAddr <- case idx of
        ExternMemory memAddr -> return memAddr
        _ -> throwError "incompatible import type"
      let MemoryInstance {lim} = ms ! memAddr
      if limitMatch lim limit
        then return idx
        else throwError "incompatible import type"
    checkImportType imp@(Import _ _ (ImportTable (TableType limit _))) = do
      idx <- getImpIdx imp
      tableAddr <- case idx of
        ExternTable tableAddr -> return tableAddr
        _ -> throwError "incompatible import type"
      let TableInstance {lim} = ts ! tableAddr
      if limitMatch lim limit
        then return idx
        else throwError "incompatible import type"

    limitMatch :: Limit -> Limit -> Bool
    limitMatch (Limit n1 m1) (Limit n2 m2) = n1 >= n2 && (isNothing m2 || fromMaybe False ((<=) <$> m1 <*> m2))

type Imports = Map.Map (TL.Text, TL.Text) ExternalValue

emptyImports :: Imports
emptyImports = Map.empty

allocFunctions :: ModuleInstance -> [Function] -> Vector FunctionInstance
allocFunctions inst@ModuleInstance {funcTypes} funs =
  let mkFuncInst f@Function {funcType} = FunctionInstance (funcTypes ! (fromIntegral funcType)) inst f
   in Vector.fromList $ map mkFuncInst funs

getGlobalValue :: ModuleInstance -> Store -> Natural -> IO Value
getGlobalValue inst store idx =
  let addr = case globaladdrs inst !? fromIntegral idx of
        Just a -> a
        Nothing -> error "Global index is out of range. It can happen if initializer refs non-import global."
   in case globalInstances store ! addr of
        GIConst _ v -> return v
        GIMut _ ref -> readIORef ref

-- due the validation there can be only these instructions

evalConstExpr :: ModuleInstance -> Store -> Expression -> IO Value
evalConstExpr _ _ [I32Const v] = return $ VI32 v
evalConstExpr _ _ [I64Const v] = return $ VI64 v
evalConstExpr _ _ [F32Const v] = return $ VF32 v
evalConstExpr _ _ [F64Const v] = return $ VF64 v
evalConstExpr inst store [GetGlobal i] = getGlobalValue inst store i
evalConstExpr _ _ instrs = error $ "Global initializer contains unsupported instructions: " ++ show instrs

allocAndInitGlobals :: ModuleInstance -> Store -> [Global] -> IO (Vector GlobalInstance)
allocAndInitGlobals inst store globs = Vector.fromList <$> mapM allocGlob globs
  where
    runIniter :: Expression -> IO Value
    -- the spec says get global can ref only imported globals
    -- only they are in store for this moment

    runIniter = evalConstExpr inst store

    allocGlob :: Global -> IO GlobalInstance
    allocGlob (Global (Const vt) initer) = GIConst vt <$> runIniter initer
    allocGlob (Global (Mut vt) initer) = do
      val <- runIniter initer
      GIMut vt <$> newIORef val

allocTables :: [Table] -> Vector TableInstance
allocTables tables = Vector.fromList $ map allocTable tables
  where
    allocTable :: Table -> TableInstance
    allocTable (Table (TableType lim@(Limit from to) _)) =
      TableInstance
        { lim,
          elements = Vector.fromList $ replicate (fromIntegral from) Nothing
        }

defaultBudget :: Natural
defaultBudget = 300

pageSize :: Int
pageSize = 64 * 1024

allocMems :: [Memory] -> IO (Vector MemoryInstance)
allocMems mems = Vector.fromList <$> mapM allocMem mems
  where
    allocMem :: Memory -> IO MemoryInstance
    allocMem (Memory lim@(Limit from to)) = do
      mem <- IOVector.replicate (fromIntegral from * pageSize) 0
      memory <- newIORef mem
      return
        MemoryInstance
          { lim,
            memory
          }

type Initialize = ExceptT String IO

initialize :: ModuleInstance -> Module -> Store -> Initialize Store
initialize inst Module {elems, datas, start} store = do
  checkedMems <- mapM (checkData store) datas
  checkedTables <- mapM (checkElem store) elems
  mapM_ initData checkedMems
  st <- Monad.foldM initElem store checkedTables
  case start of
    Just (StartFunction idx) -> do
      let funInst = funcInstances store ! (funcaddrs inst ! fromIntegral idx)
      mainRes <- liftIO $ eval defaultBudget st funInst []
      case mainRes of
        Just [] -> return st
        _ -> throwError "Start function terminated with trap"
    Nothing -> return st
  where
    checkElem :: Store -> ElemSegment -> Initialize (Address, Int, [Address])
    checkElem st ElemSegment {tableIndex, offset, funcIndexes} = do
      VI32 val <- liftIO $ evalConstExpr inst st offset
      let from = fromIntegral val
      let funcs = map ((funcaddrs inst !) . fromIntegral) funcIndexes
      let idx = tableaddrs inst ! fromIntegral tableIndex
      let last = from + length funcs
      let TableInstance lim elems = tableInstances st ! idx
      let len = Vector.length elems
      Monad.when (last > len) $ throwError "elements segment does not fit"
      return (idx, from, funcs)

    initElem :: Store -> (Address, Int, [Address]) -> Initialize Store
    initElem st (idx, from, funcs) = do
      let TableInstance lim elems = tableInstances st ! idx
      let table = TableInstance lim (elems // zip [from ..] (map Just funcs))
      return st {tableInstances = tableInstances st Vector.// [(idx, table)]}

    checkData :: Store -> DataSegment -> Initialize (Int, IOVector Word8, LBS.ByteString)
    checkData st DataSegment {memIndex, offset, chunk} = do
      VI32 val <- liftIO $ evalConstExpr inst st offset
      let from = fromIntegral val
      let idx = memaddrs inst ! fromIntegral memIndex
      let last = from + (fromIntegral $ LBS.length chunk)
      let MemoryInstance _ memory = memInstances st ! idx
      mem <- liftIO $ readIORef memory
      let len = IOVector.length mem
      Monad.when (last > len) $ throwError "data segment does not fit"
      return (from, mem, chunk)

    initData :: (Int, IOVector Word8, LBS.ByteString) -> Initialize ()
    initData (from, mem, chunk) =
      mapM_ (\(i, b) -> IOVector.write mem i b) $ zip [from ..] $ LBS.unpack chunk

instantiate :: Store -> Imports -> Valid.ValidModule -> IO (Either String (ModuleInstance, Store))
instantiate st imps mod = runExceptT $ do
  let m = Valid.getModule mod
  inst <- calcInstance st imps m
  let functions = funcInstances st <> (allocFunctions inst $ Struct.functions m)
  globals <- liftIO $ (globalInstances st <>) <$> (allocAndInitGlobals inst st $ Struct.globals m)
  let tables = tableInstances st <> (allocTables $ Struct.tables m)
  mems <- liftIO $ (memInstances st <>) <$> (allocMems $ Struct.mems m)
  st' <-
    initialize inst m $
      st
        { funcInstances = functions,
          tableInstances = tables,
          memInstances = mems,
          globalInstances = globals
        }
  return $ (inst, st')

type Stack = [Value]

data EvalCtx = EvalCtx
  { locals :: Vector Value,
    labels :: [Label],
    stack :: Stack,
    segmem :: SegmentMemory
  }
  deriving (Show, Eq)

data EvalResult
  = Done EvalCtx
  | Break Int [Value] EvalCtx
  | Trap
  | ReturnFn [Value]
  deriving (Show, Eq)

eval :: Natural -> Store -> FunctionInstance -> [Value] -> IO (Maybe [Value])
eval 0 _ _ _ = return Nothing
eval budget store FunctionInstance {funcType, moduleInstance, code = Function {localTypes, body}} args = do
  case sequence $ zipWith checkValType (params funcType) args of
    Just checkedArgs -> do
      let initialContext =
            EvalCtx
              { locals = Vector.fromList $ checkedArgs ++ map initLocal localTypes,
                labels = [Label $ results funcType],
                stack = [],
                segmem = SegmentMemory {segments = Map.empty, size = 0}
              }
      res <- go initialContext body
      case res of
        Done ctx -> return $ Just $ reverse $ stack ctx
        ReturnFn r -> return $ Just r
        Break 0 r _ -> return $ Just $ reverse r
        Break _ _ _ -> error "Break is out of range"
        Trap -> return Nothing
    Nothing -> return Nothing
  where
    checkValType :: ValueType -> Value -> Maybe Value
    checkValType I32 (VI32 v) = Just $ VI32 v
    checkValType I64 (VI64 v) = Just $ VI64 v
    checkValType F32 (VF32 v) = Just $ VF32 v
    checkValType F64 (VF64 v) = Just $ VF64 v
    checkValType Handle (VHandle x y z b) = Just $ VHandle x y z b
    checkValType _ _ = Nothing

    initLocal :: ValueType -> Value
    initLocal I32 = VI32 0
    initLocal I64 = VI64 0
    initLocal F32 = VF32 0
    initLocal F64 = VF64 0
    initLocal Handle = VHandle 0 0 0 False

    go :: EvalCtx -> Expression -> IO EvalResult
    go ctx [] = return $ Done ctx
    go ctx (instr : rest) = do
      res <- step ctx instr
      case res of
        Done ctx' -> go ctx' rest
        command -> return command

    makeLoadInstr :: (Bits i, Integral i) => EvalCtx -> Natural -> Int -> ([Value] -> i -> EvalResult) -> IO EvalResult
    makeLoadInstr ctx@EvalCtx {stack = (VI32 v : rest)} offset byteWidth cont = do
      let MemoryInstance {memory = memoryRef} = memInstances store ! (memaddrs moduleInstance ! 0)
      memory <- readIORef memoryRef
      let addr = fromIntegral v + fromIntegral offset
      let readByte idx = do
            byte <- IOVector.read memory $ addr + idx
            return $ fromIntegral byte `shiftL` (idx * 8)
      if addr + byteWidth > IOVector.length memory
        then return Trap
        else cont rest . sum <$> mapM readByte [0 .. byteWidth -1]
    makeLoadInstr _ _ _ _ = error "Incorrect value on top of stack for memory instruction"

    makeStoreInstr :: (Bits i, Integral i) => EvalCtx -> Natural -> Int -> i -> IO EvalResult
    makeStoreInstr ctx@EvalCtx {stack = (VI32 va : rest)} offset byteWidth v = do
      let MemoryInstance {memory = memoryRef} = memInstances store ! (memaddrs moduleInstance ! 0)
      memory <- readIORef memoryRef
      let addr = fromIntegral $ va + fromIntegral offset
      let writeByte idx = do
            let byte = fromIntegral $ v `shiftR` (idx * 8) .&. 0xFF
            IOVector.write memory (addr + idx) byte
      if addr + byteWidth > IOVector.length memory
        then return Trap
        else do
          mapM_ writeByte [0 .. byteWidth -1]
          return $ Done ctx {stack = rest}
    makeStoreInstr _ _ _ _ = error "Incorrect value on top of stack for memory instruction"

    step :: EvalCtx -> Instruction Natural -> IO EvalResult
    step _ Unreachable = return Trap
    step ctx Nop = return $ Done ctx
    step ctx (Block resType expr) = do
      res <- go ctx {labels = Label resType : labels ctx} expr
      case res of
        Break 0 r EvalCtx {locals = ls} -> return $ Done ctx {locals = ls, stack = r ++ stack ctx}
        Break n r ctx' -> return $ Break (n - 1) r ctx'
        Done ctx'@EvalCtx {labels = (_ : rest)} -> return $ Done ctx' {labels = rest}
        command -> return command
    step ctx loop@(Loop resType expr) = do
      res <- go ctx {labels = Label resType : labels ctx} expr
      case res of
        Break 0 r EvalCtx {locals = ls} -> step ctx {locals = ls, stack = r ++ stack ctx} loop
        Break n r ctx' -> return $ Break (n - 1) r ctx'
        Done ctx'@EvalCtx {labels = (_ : rest)} -> return $ Done ctx' {labels = rest}
        command -> return command
    step ctx@EvalCtx {stack = (VI32 v) : rest} (If resType true false) = do
      let expr = if v /= 0 then true else false
      res <- go ctx {labels = Label resType : labels ctx, stack = rest} expr
      case res of
        Break 0 r EvalCtx {locals = ls} -> return $ Done ctx {locals = ls, stack = r ++ rest}
        Break n r ctx' -> return $ Break (n - 1) r ctx'
        Done ctx'@EvalCtx {labels = (_ : rest)} -> return $ Done ctx' {labels = rest}
        command -> return command
    step ctx@EvalCtx {stack, labels} (Br label) = do
      let idx = fromIntegral label
      let Label resType = labels !! idx
      case sequence $ zipWith checkValType resType $ take (length resType) stack of
        Just result -> return $ Break idx result ctx
        Nothing -> return Trap
    step ctx@EvalCtx {stack = (VI32 v) : rest} (BrIf label) =
      if v == 0
        then return $ Done ctx {stack = rest}
        else step ctx {stack = rest} (Br label)
    step ctx@EvalCtx {stack = (VI32 v) : rest} (BrTable labels label) =
      let idx = fromIntegral v
       in let lbl = fromIntegral $ if idx < length labels then labels !! idx else label
           in step ctx {stack = rest} (Br lbl)
    step EvalCtx {stack} Return =
      let resType = results funcType
       in case sequence $ zipWith checkValType resType $ take (length resType) stack of
            Just result -> return $ ReturnFn $ reverse result
            Nothing -> return Trap
    step ctx (Call fun) = do
      let funInst = funcInstances store ! (funcaddrs moduleInstance ! fromIntegral fun)
      let ft = Language.Wasm.Interpreter.funcType funInst
      let args = params ft
      case sequence $ zipWith checkValType args $ reverse $ take (length args) $ stack ctx of
        Just params -> do
          res <- eval (budget - 1) store funInst params
          case res of
            Just res -> return $ Done ctx {stack = reverse res ++ (drop (length args) $ stack ctx)}
            Nothing -> return Trap
        Nothing -> return Trap
    step ctx@EvalCtx {stack = (VI32 v) : rest} (CallIndirect typeIdx) = do
      let funcType = funcTypes moduleInstance ! fromIntegral typeIdx
      let TableInstance {elements} = tableInstances store ! (tableaddrs moduleInstance ! 0)
      let checks = do
            addr <- Monad.join $ elements !? fromIntegral v
            let funcInst = funcInstances store ! addr
            let targetType = Language.Wasm.Interpreter.funcType funcInst
            Monad.guard $ targetType == funcType
            let args = params targetType
            Monad.guard $ length args <= length rest
            params <- sequence $ zipWith checkValType args $ reverse $ take (length args) rest
            return (funcInst, params)
      case checks of
        Just (funcInst, params) -> do
          res <- eval (budget - 1) store funcInst params
          case res of
            Just res -> return $ Done ctx {stack = reverse res ++ (drop (length params) rest)}
            Nothing -> return Trap
        Nothing -> return Trap
    step ctx@EvalCtx {stack = (_ : rest)} Drop = return $ Done ctx {stack = rest}
    step ctx@EvalCtx {stack = (VI32 test : val2 : val1 : rest)} Select =
      if test == 0
        then return $ Done ctx {stack = val2 : rest}
        else return $ Done ctx {stack = val1 : rest}
    step ctx (GetLocal i) = return $ Done ctx {stack = (locals ctx ! fromIntegral i) : stack ctx}
    step ctx@EvalCtx {stack = (v : rest)} (SetLocal i) =
      return $ Done ctx {stack = rest, locals = locals ctx // [(fromIntegral i, v)]}
    step ctx@EvalCtx {locals = ls, stack = (v : rest)} (TeeLocal i) =
      return $
        Done
          ctx
            { stack = v : rest,
              locals = locals ctx // [(fromIntegral i, v)]
            }
    step ctx (GetGlobal i) = do
      let globalInst = globalInstances store ! (globaladdrs moduleInstance ! fromIntegral i)
      val <- case globalInst of
        GIConst _ v -> return v
        GIMut _ ref -> readIORef ref
      return $ Done ctx {stack = val : stack ctx}
    step ctx@EvalCtx {stack = (v : rest)} (SetGlobal i) = do
      let globalInst = globalInstances store ! (globaladdrs moduleInstance ! fromIntegral i)
      case globalInst of
        GIConst _ v -> error "Attempt of mutation of constant global"
        GIMut _ ref -> writeIORef ref v
      return $ Done ctx {stack = rest}
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I32SegmentLoad =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I64SegmentLoad =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} F32SegmentLoad =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} F64SegmentLoad =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I32SegmentLoad8U =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I32SegmentLoad8S =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I32SegmentLoad16U =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I32SegmentLoad16S =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I64SegmentLoad8U =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I64SegmentLoad8S =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I64SegmentLoad16U =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I64SegmentLoad16S =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I64SegmentLoad32U =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I64SegmentLoad32S =
      evalSegmentLoad ctx
    step ctx@EvalCtx {stack = (VI32 v : VHandle w x y z : rest), segmem} I32SegmentStore =
      evalSegmentStore ctx
    step ctx@EvalCtx {stack = (VI64 v : VHandle w x y z : rest), segmem} I64SegmentStore =
      evalSegmentStore ctx
    step ctx@EvalCtx {stack = (VF32 v : VHandle w x y z : rest), segmem} F32SegmentStore =
      evalSegmentStore ctx
    step ctx@EvalCtx {stack = (VF64 v : VHandle w x y z : rest), segmem} F64SegmentStore =
      evalSegmentStore ctx
    step ctx@EvalCtx {stack = (VI32 v : VHandle w x y z : rest), segmem} I32SegmentStore8 =
      evalSegmentStore ctx
    step ctx@EvalCtx {stack = (VI32 v : VHandle w x y z : rest), segmem} I32SegmentStore16 =
      evalSegmentStore ctx
    step ctx@EvalCtx {stack = (VI64 v : VHandle w x y z : rest), segmem} I64SegmentStore8 =
      evalSegmentStore ctx 
    step ctx@EvalCtx {stack = (VI64 v : VHandle w x y z : rest), segmem} I64SegmentStore16 =
      evalSegmentStore ctx 
    step ctx@EvalCtx {stack = (VI64 v : VHandle w x y z : rest), segmem} I64SegmentStore32 =
      evalSegmentStore ctx 
    step ctx@EvalCtx {stack = st} CurrentMemory = do
      let MemoryInstance {memory = memoryRef} = memInstances store ! (memaddrs moduleInstance ! 0)
      memory <- readIORef memoryRef
      let size = fromIntegral $ IOVector.length memory `div` pageSize
      return $ Done ctx {stack = VI32 size : st}
    step ctx@EvalCtx {stack = (VI32 n : rest)} GrowMemory = do
      let MemoryInstance {lim = limit@(Limit _ maxLen), memory = memoryRef} = memInstances store ! (memaddrs moduleInstance ! 0)
      memory <- readIORef memoryRef
      let size = fromIntegral $ IOVector.length memory `quot` pageSize
      let growTo = size + fromIntegral n
      result <-
        ( if fromMaybe True ((growTo <=) . fromIntegral <$> maxLen) && growTo <= 0xFFFF
            then do
              mem' <- IOVector.grow memory $ fromIntegral n * pageSize
              writeIORef memoryRef mem'
              return size
            else return $ -1
          )
      return $ Done ctx {stack = VI32 (asWord32 $ fromIntegral result) : rest}
    step ctx (I32Const v) = return $ Done ctx {stack = VI32 v : stack ctx}
    step ctx (I64Const v) = return $ Done ctx {stack = VI64 v : stack ctx}
    step ctx (F32Const v) = return $ Done ctx {stack = VF32 v : stack ctx}
    step ctx (F64Const v) = return $ Done ctx {stack = VF64 v : stack ctx}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IAdd) =
      return $ Done ctx {stack = VI32 (v1 + v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 ISub) =
      return $ Done ctx {stack = VI32 (v1 - v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IMul) =
      return $ Done ctx {stack = VI32 (v1 * v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IDivU) =
      if v2 == 0
        then return Trap
        else return $ Done ctx {stack = VI32 (v1 `quot` v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IDivS) =
      if v2 == 0 || (v1 == 0x80000000 && v2 == 0xFFFFFFFF)
        then return Trap
        else return $ Done ctx {stack = VI32 (asWord32 $ asInt32 v1 `quot` asInt32 v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IRemU) =
      if v2 == 0
        then return Trap
        else return $ Done ctx {stack = VI32 (v1 `rem` v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IRemS) =
      if v2 == 0
        then return Trap
        else return $ Done ctx {stack = VI32 (asWord32 $ asInt32 v1 `rem` asInt32 v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IAnd) =
      return $ Done ctx {stack = VI32 (v1 .&. v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IOr) =
      return $ Done ctx {stack = VI32 (v1 .|. v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IXor) =
      return $ Done ctx {stack = VI32 (v1 `xor` v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IShl) =
      return $ Done ctx {stack = VI32 (v1 `shiftL` (fromIntegral v2 `rem` 32)) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IShrU) =
      return $ Done ctx {stack = VI32 (v1 `shiftR` (fromIntegral v2 `rem` 32)) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IShrS) =
      return $ Done ctx {stack = VI32 (asWord32 $ asInt32 v1 `shiftR` (fromIntegral v2 `rem` 32)) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IRotl) =
      return $ Done ctx {stack = VI32 (v1 `rotateL` fromIntegral v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IBinOp BS32 IRotr) =
      return $ Done ctx {stack = VI32 (v1 `rotateR` fromIntegral v2) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 IEq) =
      return $ Done ctx {stack = VI32 (if v1 == v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 INe) =
      return $ Done ctx {stack = VI32 (if v1 /= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 ILtU) =
      return $ Done ctx {stack = VI32 (if v1 < v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 ILtS) =
      return $ Done ctx {stack = VI32 (if asInt32 v1 < asInt32 v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 IGtU) =
      return $ Done ctx {stack = VI32 (if v1 > v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 IGtS) =
      return $ Done ctx {stack = VI32 (if asInt32 v1 > asInt32 v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 ILeU) =
      return $ Done ctx {stack = VI32 (if v1 <= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 ILeS) =
      return $ Done ctx {stack = VI32 (if asInt32 v1 <= asInt32 v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 IGeU) =
      return $ Done ctx {stack = VI32 (if v1 >= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v2 : VI32 v1 : rest)} (IRelOp BS32 IGeS) =
      return $ Done ctx {stack = VI32 (if asInt32 v1 >= asInt32 v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} I32Eqz =
      return $ Done ctx {stack = VI32 (if v == 0 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} (IUnOp BS32 IClz) =
      return $ Done ctx {stack = VI32 (fromIntegral $ countLeadingZeros v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} (IUnOp BS32 ICtz) =
      return $ Done ctx {stack = VI32 (fromIntegral $ countTrailingZeros v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} (IUnOp BS32 IPopcnt) =
      return $ Done ctx {stack = VI32 (fromIntegral $ popCount v) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IAdd) =
      return $ Done ctx {stack = VI64 (v1 + v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 ISub) =
      return $ Done ctx {stack = VI64 (v1 - v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IMul) =
      return $ Done ctx {stack = VI64 (v1 * v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IDivU) =
      if v2 == 0
        then return Trap
        else return $ Done ctx {stack = VI64 (v1 `quot` v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IDivS) =
      if v2 == 0 || (v1 == 0x8000000000000000 && v2 == 0xFFFFFFFFFFFFFFFF)
        then return Trap
        else return $ Done ctx {stack = VI64 (asWord64 $ asInt64 v1 `quot` asInt64 v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IRemU) =
      if v2 == 0
        then return Trap
        else return $ Done ctx {stack = VI64 (v1 `rem` v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IRemS) =
      if v2 == 0
        then return Trap
        else return $ Done ctx {stack = VI64 (asWord64 $ asInt64 v1 `rem` asInt64 v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IAnd) =
      return $ Done ctx {stack = VI64 (v1 .&. v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IOr) =
      return $ Done ctx {stack = VI64 (v1 .|. v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IXor) =
      return $ Done ctx {stack = VI64 (v1 `xor` v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IShl) =
      return $ Done ctx {stack = VI64 (v1 `shiftL` (fromIntegral (v2 `rem` 64))) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IShrU) =
      return $ Done ctx {stack = VI64 (v1 `shiftR` (fromIntegral (v2 `rem` 64))) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IShrS) =
      return $ Done ctx {stack = VI64 (asWord64 $ asInt64 v1 `shiftR` (fromIntegral (v2 `rem` 64))) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IRotl) =
      return $ Done ctx {stack = VI64 (v1 `rotateL` fromIntegral v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IBinOp BS64 IRotr) =
      return $ Done ctx {stack = VI64 (v1 `rotateR` fromIntegral v2) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 IEq) =
      return $ Done ctx {stack = VI32 (if v1 == v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 INe) =
      return $ Done ctx {stack = VI32 (if v1 /= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 ILtU) =
      return $ Done ctx {stack = VI32 (if v1 < v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 ILtS) =
      return $ Done ctx {stack = VI32 (if asInt64 v1 < asInt64 v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 IGtU) =
      return $ Done ctx {stack = VI32 (if v1 > v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 IGtS) =
      return $ Done ctx {stack = VI32 (if asInt64 v1 > asInt64 v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 ILeU) =
      return $ Done ctx {stack = VI32 (if v1 <= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 ILeS) =
      return $ Done ctx {stack = VI32 (if asInt64 v1 <= asInt64 v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 IGeU) =
      return $ Done ctx {stack = VI32 (if v1 >= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v2 : VI64 v1 : rest)} (IRelOp BS64 IGeS) =
      return $ Done ctx {stack = VI32 (if asInt64 v1 >= asInt64 v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} I64Eqz =
      return $ Done ctx {stack = VI32 (if v == 0 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} (IUnOp BS64 IClz) =
      return $ Done ctx {stack = VI64 (fromIntegral $ countLeadingZeros v) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} (IUnOp BS64 ICtz) =
      return $ Done ctx {stack = VI64 (fromIntegral $ countTrailingZeros v) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} (IUnOp BS64 IPopcnt) =
      return $ Done ctx {stack = VI64 (fromIntegral $ popCount v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (FUnOp BS32 FAbs) =
      return $ Done ctx {stack = VF32 (abs v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (FUnOp BS32 FNeg) =
      return $ Done ctx {stack = VF32 (negate v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (FUnOp BS32 FCeil) =
      return $ Done ctx {stack = VF32 (floatCeil v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (FUnOp BS32 FFloor) =
      return $ Done ctx {stack = VF32 (floatFloor v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (FUnOp BS32 FTrunc) =
      return $ Done ctx {stack = VF32 (floatTrunc v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (FUnOp BS32 FNearest) =
      return $ Done ctx {stack = VF32 (nearest v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (FUnOp BS32 FSqrt) =
      return $ Done ctx {stack = VF32 (sqrt v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (FUnOp BS64 FAbs) =
      return $ Done ctx {stack = VF64 (abs v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (FUnOp BS64 FNeg) =
      return $ Done ctx {stack = VF64 (negate v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (FUnOp BS64 FCeil) =
      return $ Done ctx {stack = VF64 (doubleCeil v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (FUnOp BS64 FFloor) =
      return $ Done ctx {stack = VF64 (doubleFloor v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (FUnOp BS64 FTrunc) =
      return $ Done ctx {stack = VF64 (doubleTrunc v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (FUnOp BS64 FNearest) =
      return $ Done ctx {stack = VF64 (nearest v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (FUnOp BS64 FSqrt) =
      return $ Done ctx {stack = VF64 (sqrt v) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FBinOp BS32 FAdd) =
      return $ Done ctx {stack = VF32 (v1 + v2) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FBinOp BS32 FSub) =
      return $ Done ctx {stack = VF32 (v1 - v2) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FBinOp BS32 FMul) =
      return $ Done ctx {stack = VF32 (v1 * v2) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FBinOp BS32 FDiv) =
      return $ Done ctx {stack = VF32 (v1 / v2) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FBinOp BS32 FMin) =
      return $ Done ctx {stack = VF32 (zeroAwareMin v1 v2) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FBinOp BS32 FMax) =
      return $ Done ctx {stack = VF32 (zeroAwareMax v1 v2) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FBinOp BS32 FCopySign) =
      return $ Done ctx {stack = VF32 (copySign v1 v2) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FBinOp BS64 FAdd) =
      return $ Done ctx {stack = VF64 (v1 + v2) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FBinOp BS64 FSub) =
      return $ Done ctx {stack = VF64 (v1 - v2) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FBinOp BS64 FMul) =
      return $ Done ctx {stack = VF64 (v1 * v2) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FBinOp BS64 FDiv) =
      return $ Done ctx {stack = VF64 (v1 / v2) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FBinOp BS64 FMin) =
      return $ Done ctx {stack = VF64 (zeroAwareMin v1 v2) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FBinOp BS64 FMax) =
      return $ Done ctx {stack = VF64 (zeroAwareMax v1 v2) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FBinOp BS64 FCopySign) =
      return $ Done ctx {stack = VF64 (copySign v1 v2) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FRelOp BS32 FEq) =
      return $ Done ctx {stack = VI32 (if v1 == v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FRelOp BS32 FNe) =
      return $ Done ctx {stack = VI32 (if v1 /= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FRelOp BS32 FLt) =
      return $ Done ctx {stack = VI32 (if v1 < v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FRelOp BS32 FGt) =
      return $ Done ctx {stack = VI32 (if v1 > v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FRelOp BS32 FLe) =
      return $ Done ctx {stack = VI32 (if v1 <= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF32 v2 : VF32 v1 : rest)} (FRelOp BS32 FGe) =
      return $ Done ctx {stack = VI32 (if v1 >= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FRelOp BS64 FEq) =
      return $ Done ctx {stack = VI32 (if v1 == v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FRelOp BS64 FNe) =
      return $ Done ctx {stack = VI32 (if v1 /= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FRelOp BS64 FLt) =
      return $ Done ctx {stack = VI32 (if v1 < v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FRelOp BS64 FGt) =
      return $ Done ctx {stack = VI32 (if v1 > v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FRelOp BS64 FLe) =
      return $ Done ctx {stack = VI32 (if v1 <= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VF64 v2 : VF64 v1 : rest)} (FRelOp BS64 FGe) =
      return $ Done ctx {stack = VI32 (if v1 >= v2 then 1 else 0) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} I32WrapI64 =
      return $ Done ctx {stack = VI32 (fromIntegral $ v .&. 0xFFFFFFFF) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (ITruncFU BS32 BS32) =
      if isNaN v || isInfinite v || v >= 2 ^ 32 || v <= -1
        then return Trap
        else return $ Done ctx {stack = VI32 (truncate v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (ITruncFU BS32 BS64) =
      if isNaN v || isInfinite v || v >= 2 ^ 32 || v <= -1
        then return Trap
        else return $ Done ctx {stack = VI32 (truncate v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (ITruncFU BS64 BS32) =
      if isNaN v || isInfinite v || v >= 2 ^ 64 || v <= -1
        then return Trap
        else return $ Done ctx {stack = VI64 (truncate v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (ITruncFU BS64 BS64) =
      if isNaN v || isInfinite v || v >= 2 ^ 64 || v <= -1
        then return Trap
        else return $ Done ctx {stack = VI64 (truncate v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (ITruncFS BS32 BS32) =
      if isNaN v || isInfinite v || v >= 2 ^ 31 || v < -2 ^ 31
        then return Trap
        else return $ Done ctx {stack = VI32 (asWord32 $ truncate v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (ITruncFS BS32 BS64) =
      if isNaN v || isInfinite v || v >= 2 ^ 31 || v < -2 ^ 31
        then return Trap
        else return $ Done ctx {stack = VI32 (asWord32 $ truncate v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (ITruncFS BS64 BS32) =
      if isNaN v || isInfinite v || v >= 2 ^ 63 || v < -2 ^ 63
        then return Trap
        else return $ Done ctx {stack = VI64 (asWord64 $ truncate v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (ITruncFS BS64 BS64) =
      if isNaN v || isInfinite v || v >= 2 ^ 63 || v < -2 ^ 63
        then return Trap
        else return $ Done ctx {stack = VI64 (asWord64 $ truncate v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} I64ExtendUI32 =
      return $ Done ctx {stack = VI64 (fromIntegral v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} I64ExtendSI32 =
      return $ Done ctx {stack = VI64 (asWord64 $ fromIntegral $ asInt32 v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} (FConvertIU BS32 BS32) =
      return $ Done ctx {stack = VF32 (realToFrac v) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} (FConvertIU BS32 BS64) =
      return $ Done ctx {stack = VF32 (realToFrac v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} (FConvertIU BS64 BS32) =
      return $ Done ctx {stack = VF64 (realToFrac v) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} (FConvertIU BS64 BS64) =
      return $ Done ctx {stack = VF64 (realToFrac v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} (FConvertIS BS32 BS32) =
      return $ Done ctx {stack = VF32 (realToFrac $ asInt32 v) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} (FConvertIS BS32 BS64) =
      return $ Done ctx {stack = VF32 (realToFrac $ asInt64 v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} (FConvertIS BS64 BS32) =
      return $ Done ctx {stack = VF64 (realToFrac $ asInt32 v) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} (FConvertIS BS64 BS64) =
      return $ Done ctx {stack = VF64 (realToFrac $ asInt64 v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} F32DemoteF64 =
      return $ Done ctx {stack = VF32 (realToFrac v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} F64PromoteF32 =
      return $ Done ctx {stack = VF64 (realToFrac v) : rest}
    step ctx@EvalCtx {stack = (VF32 v : rest)} (IReinterpretF BS32) =
      return $ Done ctx {stack = VI32 (floatToWord v) : rest}
    step ctx@EvalCtx {stack = (VF64 v : rest)} (IReinterpretF BS64) =
      return $ Done ctx {stack = VI64 (doubleToWord v) : rest}
    step ctx@EvalCtx {stack = (VI32 v : rest)} (FReinterpretI BS32) =
      return $ Done ctx {stack = VF32 (wordToFloat v) : rest}
    step ctx@EvalCtx {stack = (VI64 v : rest)} (FReinterpretI BS64) =
      return $ Done ctx {stack = VF64 (wordToDouble v) : rest}
    -- MS-Wasm step instructions
    -- TODO: loadFromSegment doesn't specify what's IN the handle, so same
    -- code for I32SegmentLoad and I64SegmentLoad
    -- step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I32SegmentLoad =
    --   evalSegmentLoad ctx stack segmem
    -- step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} I64SegmentLoad =
    --   evalSgmentLoad sctx stack segmem
    -- step ctx@EvalCtx {stack = (VI32 v : VHandle w x y z : rest), segmem} I32SegmentStore =
    --   evalSegmentStore ctx stack segmem
    -- step ctx@EvalCtx {stack = (VI64 v : VHandle w x y z : rest), segmem} I64SegmentStore =
    --   evalSegmentStore ctx stack segmem
    step ctx@EvalCtx {stack = (VI32 v : rest), segmem} NewSegment =
      let result =
            VHandle
              (asWord32 $ size segmem)
              (asWord32 0)
              (asWord32 $ size segmem + asInt32 v)
              False
       in let newseg = newSegment segmem result (VI32 (asWord32 0))
           in case newseg of
                Just mem -> return $ Done ctx {stack = result : rest, segmem = mem}
                Nothing -> return Trap
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} FreeSegment =
      let result = freeSegment segmem (VHandle w x y z)
       in case result of
            Just mem -> return $ Done ctx {stack = rest, segmem = mem}
            Nothing -> return Trap
    step
      ctx@EvalCtx
        { stack = VI32 bound : VI32 base : VHandle w x y z : rest,
          segmem
        }
      SegmentSlice =
        let result = sliceSegment (VHandle w x y z) (asInt32 base) (asInt32 bound)
         in case result of
              Just val ->
                let newseg = newSegment segmem val (VI32 (asWord32 0))
                 in case newseg of
                      Just mem -> return $ Done ctx {stack = val : rest, segmem = mem}
                      Nothing -> return Trap
              Nothing -> return Trap
    step ctx@EvalCtx {stack = (VHandle w x y z : rest), segmem} HandleSegmentLoad =
      if isInvalidHandle (VHandle w x y z)
        then return Trap
        else
          let result = loadFromSegment segmem (VHandle w x y z)
           in case result of
                Just (VHandle t u v False) ->
                  return $
                    Done
                      ctx
                        { stack = (VHandle t u v False) : rest
                        }
                _ -> return Trap
    step
      ctx@EvalCtx
        { stack = (VHandle a b c d : VHandle w x y z : rest),
          segmem
        }
      HandleSegmentStore =
        if isInvalidHandle (VHandle w x y z)
          then return Trap
          else
            let result = storeToSegment segmem (VHandle w x y z) (VHandle a b c d)
             in case result of
                  Just mem -> return $ Done ctx {stack = rest, segmem = mem}
                  Nothing -> return Trap
    step ctx@EvalCtx {stack = (VHandle w x y z : VI32 i : rest), segmem} HandleAdd =
      let result = VHandle w (x + i) y z
       in let newseg = newSegment segmem result (VI32 (asWord32 0))
           in case newseg of
                Just mem -> return $ Done ctx {stack = result : rest, segmem = mem}
                Nothing -> return Trap
    step ctx@EvalCtx {stack = (VHandle w x y z : VI32 i : rest), segmem} HandleSub =
      let result = VHandle w (x - i) y z
       in let newseg = newSegment segmem result (VI32 (asWord32 0))
           in case newseg of
                Just mem -> return $ Done ctx {stack = result : rest, segmem = mem}
                Nothing -> return Trap
    step ctx@EvalCtx {stack = (VHandle w x y z : VI32 i : rest), segmem} HandleGetOffset =
      return $ Done ctx {stack = VI32 x : rest}
    step ctx@EvalCtx {stack = (VI32 i : VHandle w x y z : rest), segmem} HandleSetOffset =
      let result = VHandle w i y z
       in let newseg = newSegment segmem result (VI32 (asWord32 0))
           in case newseg of
                Just mem -> return $ Done ctx {stack = result : rest, segmem = mem}
                Nothing -> return Trap
    -- End MS-Wasm instructions
    step EvalCtx {stack} instr = error $ "Error during evaluation of instruction: " ++ show instr ++ ". Stack " ++ show stack
eval _ _ HostInstance {funcType, hostCode} args = Just <$> hostCode args

invoke :: Store -> Address -> [Value] -> IO (Maybe [Value])
invoke st funcIdx = eval defaultBudget st $ funcInstances st ! funcIdx

invokeExport :: Store -> ModuleInstance -> TL.Text -> [Value] -> IO (Maybe [Value])
invokeExport st ModuleInstance {exports} name args =
  case Vector.find (\(ExportInstance n _) -> n == name) exports of
    Just (ExportInstance _ (ExternFunction addr)) -> invoke st addr args
    _ -> error $ "Function with name " ++ show name ++ " was not found in module's exports"

getGlobalValueByName :: Store -> ModuleInstance -> TL.Text -> IO Value
getGlobalValueByName store ModuleInstance {exports} name =
  case Vector.find (\(ExportInstance n _) -> n == name) exports of
    Just (ExportInstance _ (ExternGlobal addr)) ->
      let globalInst = globalInstances store ! addr
       in case globalInst of
            GIConst _ v -> return v
            GIMut _ ref -> readIORef ref
    _ -> error $ "Function with name " ++ show name ++ " was not found in module's exports"