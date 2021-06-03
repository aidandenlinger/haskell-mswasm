{-# LANGUAGE DuplicateRecordFields #-}

module Language.Wasm (
    Module,
    ValidModule,
    ValidationError(..),
    parse,
    validate,
    Language.Wasm.parseScript,
    encode,
    encodeLazy,
    decode,
    decodeLazy,
    showModuleFromFile,
    Script,
    runScript
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Language.Wasm.Structure as Struct
import Language.Wasm.Script as Script
import Language.Wasm.Lexer as Lexer
import Language.Wasm.Parser as Parser
import Language.Wasm.Validate as Valid
import Language.Wasm.Binary as Binary
import Numeric.Natural (Natural)

-- | Parse WebAssembly text representation to `Module`
parse :: LBS.ByteString -> Either String Module
parse content = Lexer.scanner content >>= Parser.parseModule

-- | Parse WebAssembly extended scipt grammar
parseScript :: LBS.ByteString -> Either String Script
parseScript content = Lexer.scanner content >>= Parser.parseScript

-- | Dump `Module` to binary representation
encode :: Module -> BS.ByteString
encode = dumpModule

-- | Dump `Module` to binary representation lazily
encodeLazy :: Module -> LBS.ByteString
encodeLazy = dumpModuleLazy

-- | Decode `Module` from binary representation
decode :: BS.ByteString -> Either String Module
decode = decodeModule

-- | Decode `Module` from binary representation lazily
decodeLazy :: LBS.ByteString -> Either String Module
decodeLazy = decodeModuleLazy

showModuleFromFile :: FilePath -> IO String
showModuleFromFile input = do
  binary <- LBS.readFile input
  case decodeLazy binary of
    Right mod -> return $ showModule mod
    Left reason -> return reason


showModule :: Module -> IO String
showModule Module { functions = t}  = concatMap ((++ "\n\nnew function:\n\n") . functionPrint) t
  where
    functionPrint :: Struct.Function -> String
    functionPrint Struct.Function {Struct.body = b} = unlines (map (instrPrint 0) b)

    instrPrint :: Int -> Struct.Instruction Natural -> String
    instrPrint i (Block {Struct.body = b})
      = indent i ++ "Block:\n" ++ unlines (map (instrPrint (i+1)) b)
    instrPrint i (Loop {Struct.body = b})
      = indent i ++ "Loop:\n" ++ unlines (map (instrPrint (i+1)) b)
    instrPrint i (If {Struct.true = t, Struct.false = f})
      = indent i ++ "If True:\n"  ++ unlines (map (instrPrint (i+1)) t) ++ 
        indent i ++ "If False:\n" ++ unlines (map (instrPrint (i+1)) f)
    instrPrint i e
      = indent i ++ show e
    
    indent :: Int -> String
    indent 0 = ""
    indent n = "| " ++ (indent $ n - 1)



data Arrow = Arrow End End deriving (Show, Eq)

(==>) :: (ToEnd a, ToEnd b) => a -> b -> Arrow
(==>) a b = Arrow (toEnd a) (toEnd b)
    

convertModuleFromFile :: FilePath -> IO String
convertModuleFromFile input = do
  binary <- LBS.readFile input
  case decodeLazy binary of
    Right mod    -> return $ showModule (convertModule mod) -- HERE
    Left  reason -> return reason -- HERE

convertModule :: Module -> Module
convertModule mod@Module {functions = t} = mod {functions = concatMap convertFun t}
  where
    convertFun :: Struct.Function -> Struct.Function
    convertFun Struct.Function {Struct.localTypes = ls, Struct.body = b}
      = Struct.Function { Struct.localTypes = locals
                        , Struct.body = funHeader mem ++ 
                                        convertInstrs memidx [] [] b ++ 
                                        funFooter mem }
      where
        (locals, memidx) = convertLocals ls
    
    convertLocals :: [LocalsType] -> ([LocalsType], Int)
    convertLocals locals = (locals ++ [HandleType], length locals)

    funHeader :: Int -> [Struct.Instruction Natural]
    funHeader mem = [ I32Const <large const here> -- 3
                    , NewSegment
                    , SetLocal mem ] -- TODO figure out memidx/alloc local handle
    
    funFooter :: [Struct.Instruction Natural]
    funFooter mem = [ GetLocal mem
                    , FreeSegment ]

    --; convert list of instructions and return result
    convertInstrs :: Int -> [ValueType] -> [Struct.Instruction Natural]
                  -> [Struct.Instruction Natural]
    convertInstrs mem args []     = []
    convertInstrs mem args (i:is) = i' ++ convertInstrs mem args' is
      where
        (i', args') = convertInstr mem args i
    

    convertInstrHelper :: Int -> ValueType -> ValueType -> Struct.Instruction Natural
                       -> [Struct.Instruction Natural]
    convertInstrHelper mem expTy ty instr
      | expTy == ty = [ instr ]
      | otherwise   = [ instr
                      , GetLocal mem
                      , HandleSetOffset ]

    pushArgs :: Struct.Instruction Natural -> [ValueType] -> ([ValueType], Maybe ValueType)
    pushArgs instr as (as', res) = undefined -- define this!
      where
        Arrow args res = getInstrType instr
        as' = args ++ as

    getInstrType :: Ctx -> Instruction Natural -> Arrow
    getInstrType mem args Unreachable = Any ==> Any
    getInstrType mem args Nop = empty ==> empty
    getInstrType mem args Block {resultType, body} = empty ==> resultType
    getInstrType mem args Loop {resultType, body} = empty ==> resultTYpe
    getInstrType mem args If {resultType, true, false} = I32 ==> resultType
    getInstrType mem args (Br lbl) = TODO
      let r = map Val . maybeToList <$> getLabel lbl
      in (Any : r) ==> Any
    getInstrType mem args (BrIf lbl) = TODO
      let r = map Val . maybeToList <$> getLabel lbl
      in (Any : r) ==> Any
    getInstrType mem args (BrTable lbls lbl) = TODO
      let r = map Val . maybeToList <$> getLabel lbl
      in ([Any] ++ r ++ [Val I32]) ==> Any
    getInstrType mem args Return = TODO
    getInstrType mem args (Call fun) = TODO
    getInstrType mem args (CallIndirect sign) = TODO
    getInstrType mem args Drop = Var ==> empty
    getInstrType mem args Select = [Var, Var, Val I32] ==> Var
    getInstrType mem args (GetLocal local) = TODO
    getInstrType mem args (SetLocal local) = TODO
    getInstrType mem args (TeeLocal local) = TODO
    getInstrType mem args (GetGlobal global) = TODO
    getInstrType mem args (SetGlobal global) = TODO
    getInstrType mem args (I32SegmentLoad ) = Handle ==> I32
    getInstrType mem args (I64SegmentLoad ) = Handle ==> I64
    getInstrType mem args (F32SegmentLoad) = Handle ==> F32
    getInstrType mem args (F64SegmentLoad) = Handle ==> F64
    getInstrType mem args (I32SegmentLoad8S ) = Handle ==> I32
    getInstrType mem args (I32SegmentLoad8U ) = Handle ==> I32
    getInstrType mem args (I32SegmentLoad16S ) = Handle ==> I32
    getInstrType mem args (I32SegmentLoad16U ) = Handle ==> I32
    getInstrType mem args (I64SegmentLoad8S ) = Handle ==> I64
    getInstrType mem args (I64SegmentLoad8U ) = Handle ==> I64
    getInstrType mem args (I64SegmentLoad16S ) = Handle ==> I64
    getInstrType mem args (I64SegmentLoad16U ) = Handle ==> I64
    getInstrType mem args (I64SegmentLoad32S ) = Handle ==> I64
    getInstrType mem args (I64SegmentLoad32U ) = Handle ==> I64
    getInstrType mem args (I32SegmentStore ) = [Handle, I32] ==> empty
    getInstrType mem args (I64SegmentStore ) = [Handle, I64] ==> empty
    getInstrType mem args (F32SegmentStore) = [Handle, F32] ==> empty
    getInstrType mem args (F64SegmentStore) = [Handle, F64] ==> empty
    getInstrType mem args (I32SegmentStore8 ) = [Handle, I32] ==> empty
    getInstrType mem args (I32SegmentStore16 ) = [Handle, I32] ==> empty
    getInstrType mem args (I64SegmentStore8 ) = [Handle, I64] ==> empty
    getInstrType mem args (I64SegmentStore16 ) = [Handle, I64] ==> empty
    getInstrType mem args (I64SegmentStore32 ) = [Handle, I64] ==> empty
    getInstrType mem args CurrentMemory = empty ==> I32
    getInstrType mem args GrowMemory = I32 ==> I32
    getInstrType mem args NewSegment = I32 ==> Handle
    getInstrType mem args FreeSegment = Handle ==> empty
    getInstrType mem args SegmentSlice = [Handle, I32, I32] ==> Handle
    getInstrType mem args HandleSegmentLoad = Handle ==> Handle
    getInstrType mem args HandleSegmentStore = [Handle, Handle] ==> empty
    getInstrType mem args HandleAdd = [I32, Handle] ==> Handle
    getInstrType mem args HandleSub = [I32, Handle] ==> Handle
    getInstrType mem args HandleGetOffset = Handle ==> I32
    getInstrType mem args HandleSetOffset = [I32, Handle] ==> Handle
    getInstrType mem args (I32Const _) = empty ==> I32
    getInstrType mem args (I64Const _) = empty ==> I64
    getInstrType mem args (F32Const _) = empty ==> F32
    getInstrType mem args (F64Const _) = empty ==> F64
    getInstrType mem args (IUnOp BS32 _) = I32 ==> I32
    getInstrType mem args (IUnOp BS64 _) = I64 ==> I64
    getInstrType mem args (IBinOp BS32 _) = [I32, I32] ==> I32
    getInstrType mem args (IBinOp BS64 _) = [I64, I64] ==> I64
    getInstrType mem args I32Eqz = I32 ==> I32
    getInstrType mem args I64Eqz = I64 ==> I32
    getInstrType mem args (IRelOp BS32 _) = [I32, I32] ==> I32
    getInstrType mem args (IRelOp BS64 _) = [I64, I64] ==> I32
    getInstrType mem args (FUnOp BS32 _) = F32 ==> F32
    getInstrType mem args (FUnOp BS64 _) = F64 ==> F64
    getInstrType mem args (FBinOp BS32 _) = [F32, F32] ==> F32
    getInstrType mem args (FBinOp BS64 _) = [F64, F64] ==> F64
    getInstrType mem args (FRelOp BS32 _) = [F32, F32] ==> I32
    getInstrType mem args (FRelOp BS64 _) = [F64, F64] ==> I32
    getInstrType mem args I32WrapI64 = I64 ==> I32
    getInstrType mem args (ITruncFU BS32 BS32) = F32 ==> I32
    getInstrType mem args (ITruncFU BS32 BS64) = F64 ==> I32
    getInstrType mem args (ITruncFU BS64 BS32) = F32 ==> I64
    getInstrType mem args (ITruncFU BS64 BS64) = F64 ==> I64
    getInstrType mem args (ITruncFS BS32 BS32) = F32 ==> I32
    getInstrType mem args (ITruncFS BS32 BS64) = F64 ==> I32
    getInstrType mem args (ITruncFS BS64 BS32) = F32 ==> I64
    getInstrType mem args (ITruncFS BS64 BS64) = F64 ==> I64
    getInstrType mem args I64ExtendSI32 = I32 ==> I64
    getInstrType mem args I64ExtendUI32 = I32 ==> I64
    getInstrType mem args (FConvertIU BS32 BS32) = I32 ==> F32
    getInstrType mem args (FConvertIU BS32 BS64) = I64 ==> F32
    getInstrType mem args (FConvertIU BS64 BS32) = I32 ==> F64
    getInstrType mem args (FConvertIU BS64 BS64) = I64 ==> F64
    getInstrType mem args (FConvertIS BS32 BS32) = I32 ==> F32
    getInstrType mem args (FConvertIS BS32 BS64) = I64 ==> F32
    getInstrType mem args (FConvertIS BS64 BS32) = I32 ==> F64
    getInstrType mem args (FConvertIS BS64 BS64) = I64 ==> F64
    getInstrType mem args F32DemoteF64 = F64 ==> F32
    getInstrType mem args F64PromoteF32 = F32 ==> F64
    getInstrType mem args (IReinterpretF BS32) = F32 ==> I32
    getInstrType mem args (IReinterpretF BS64) = F64 ==> I64
    getInstrType mem args (FReinterpretI BS32) = I32 ==> F32
    getInstrType mem args (FReinterpretI BS64) = I64 ==> F64
    getInstrType mem args _ = error "Wrong instr or wrong type"