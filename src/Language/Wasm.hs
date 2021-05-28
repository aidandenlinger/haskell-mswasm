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
    

convertModuleFromFile :: FilePath -> IO String
convertModuleFromFile input = do
  binary <- LBS.readFile input
  case decodeLazy binary of
    Right mod    -> return $ showModule (convertModule mod)
    Left  reason -> return reason

convertModule :: Module -> Module
convertModule mod@Module {functions = t} = mod {functions = concatMap convertFun t}
  where
    convertFun :: Struct.Function -> Struct.Function
    convertFun Struct.Function {Struct.localTypes = ls, Struct.body = b}
      = Struct.Function { Struct.localTypes = locals
                        , Struct.body = funHeader mem ++ 
                                        convertInstr memidx [] [] b ++ 
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
    pushArgs instr as (as', res)
      where
        Arrow args res = getInstrType instr
        as' = args ++ as


    convertInstr :: Int -> [ValueType] -> Struct.Instruction Natural
                 -> ([Struct.Instruction Natural], [ValueType])
    convertInstr mem args   I32SegmentStore = ([I32SegmentStore], I32 : Handle : args)
    convertInstr mem a:args I32SegmentLoad  = (instrs, Handle : args)
      where
        instrs = convertInstrHelper mem I32 a (I32SegmentLoad)
    convertInstr mem a:args (I32Const n)    = (instrs, args)
      where
        instrs = convertInstrHelper mem I32 a (I32Const n)

    convertInstr mem args Unreachable = return $ Any ==> Any
    convertInstr mem args Nop = return $ empty ==> empty
    convertInstr mem args Block {resultType, body} = 
    convertInstr mem args Loop {resultType, body} = 
    convertInstr mem args If {resultType, true, false} = 
    convertInstr mem args (Br lbl) = 
    convertInstr mem args (BrIf lbl) = 
    convertInstr mem args (BrTable lbls lbl) = 
    convertInstr mem args Return = 
    convertInstr mem args (Call fun) = 
    convertInstr mem args (CallIndirect sign) = 
    convertInstr mem args Drop = 
    convertInstr mem args Select = 
    convertInstr mem args (GetLocal local) = 
    convertInstr mem args (SetLocal local) = 
    convertInstr mem args (TeeLocal local) = 
    convertInstr mem args (GetGlobal global) = 
    convertInstr mem args (SetGlobal global) = 
    convertInstr mem args (I32SegmentLoad ) = return $ Handle ==> I32
    convertInstr mem args (I64SegmentLoad ) = return $ Handle ==> I64
    convertInstr mem args (F32SegmentLoad) = return $ Handle ==> F32
    convertInstr mem args (F64SegmentLoad) = return $ Handle ==> F64
    convertInstr mem args (I32SegmentLoad8S ) = return $ Handle ==> I32
    convertInstr mem args (I32SegmentLoad8U ) = return $ Handle ==> I32
    convertInstr mem args (I32SegmentLoad16S ) = return $ Handle ==> I32
    convertInstr mem args (I32SegmentLoad16U ) = return $ Handle ==> I32
    convertInstr mem args (I64SegmentLoad8S ) = return $ Handle ==> I64
    convertInstr mem args (I64SegmentLoad8U ) = return $ Handle ==> I64
    convertInstr mem args (I64SegmentLoad16S ) = return $ Handle ==> I64
    convertInstr mem args (I64SegmentLoad16U ) = return $ Handle ==> I64
    convertInstr mem args (I64SegmentLoad32S ) = return $ Handle ==> I64
    convertInstr mem args (I64SegmentLoad32U ) = return $ Handle ==> I64
    convertInstr mem args (I32SegmentStore ) = return $ [Handle, I32] ==> empty
    convertInstr mem args (I64SegmentStore ) = return $ [Handle, I64] ==> empty
    convertInstr mem args (F32SegmentStore) = return $ [Handle, F32] ==> empty
    convertInstr mem args (F64SegmentStore) = return $ [Handle, F64] ==> empty
    convertInstr mem args (I32SegmentStore8 ) = return $ [Handle, I32] ==> empty
    convertInstr mem args (I32SegmentStore16 ) = return $ [Handle, I32] ==> empty
    convertInstr mem args (I64SegmentStore8 ) = return $ [Handle, I64] ==> empty
    convertInstr mem args (I64SegmentStore16 ) = return $ [Handle, I64] ==> empty
    convertInstr mem args (I64SegmentStore32 ) = return $ [Handle, I64] ==> empty
    convertInstr mem args CurrentMemory = 
    convertInstr mem args NewSegment = return $ I32 ==> Handle
    convertInstr mem args FreeSegment = return $ Handle ==> empty
    convertInstr mem args SegmentSlice = return $ [Handle, I32, I32] ==> Handle
    convertInstr mem args HandleSegmentLoad = return $ Handle ==> Handle
    convertInstr mem args HandleSegmentStore = return $ [Handle, Handle] ==> empty
    convertInstr mem args HandleAdd = return $ [I32, Handle] ==> Handle
    convertInstr mem args HandleSub = return $ [I32, Handle] ==> Handle
    convertInstr mem args HandleGetOffset = return $ Handle ==> I32
    convertInstr mem args HandleSetOffset = return $ [I32, Handle] ==> Handle
    convertInstr mem args (I32Const _) = return $ empty ==> I32
    convertInstr mem args (I64Const _) = return $ empty ==> I64
    convertInstr mem args (F32Const _) = return $ empty ==> F32
    convertInstr mem args (F64Const _) = return $ empty ==> F64
    convertInstr mem args (IUnOp BS32 _) = return $ I32 ==> I32
    convertInstr mem args (IUnOp BS64 _) = return $ I64 ==> I64
    convertInstr mem args (IBinOp BS32 _) = return $ [I32, I32] ==> I32
    convertInstr mem args (IBinOp BS64 _) = return $ [I64, I64] ==> I64
    convertInstr mem args I32Eqz = return $ I32 ==> I32
    convertInstr mem args I64Eqz = return $ I64 ==> I32
    convertInstr mem args (IRelOp BS32 _) = return $ [I32, I32] ==> I32
    convertInstr mem args (IRelOp BS64 _) = return $ [I64, I64] ==> I32
    convertInstr mem args (FUnOp BS32 _) = return $ F32 ==> F32
    convertInstr mem args (FUnOp BS64 _) = return $ F64 ==> F64
    convertInstr mem args (FBinOp BS32 _) = return $ [F32, F32] ==> F32
    convertInstr mem args (FBinOp BS64 _) = return $ [F64, F64] ==> F64
    convertInstr mem args (FRelOp BS32 _) = return $ [F32, F32] ==> I32
    convertInstr mem args (FRelOp BS64 _) = return $ [F64, F64] ==> I32
    convertInstr mem args I32WrapI64 = return $ I64 ==> I32
    convertInstr mem args (ITruncFU BS32 BS32) = return $ F32 ==> I32
    convertInstr mem args (ITruncFU BS32 BS64) = return $ F64 ==> I32
    convertInstr mem args (ITruncFU BS64 BS32) = return $ F32 ==> I64
    convertInstr mem args (ITruncFU BS64 BS64) = return $ F64 ==> I64
    convertInstr mem args (ITruncFS BS32 BS32) = return $ F32 ==> I32
    convertInstr mem args (ITruncFS BS32 BS64) = return $ F64 ==> I32
    convertInstr mem args (ITruncFS BS64 BS32) = return $ F32 ==> I64
    convertInstr mem args (ITruncFS BS64 BS64) = return $ F64 ==> I64
    convertInstr mem args I64ExtendSI32 = return $ I32 ==> I64
    convertInstr mem args I64ExtendUI32 = return $ I32 ==> I64
    convertInstr mem args (FConvertIU BS32 BS32) = return $ I32 ==> F32
    convertInstr mem args (FConvertIU BS32 BS64) = return $ I64 ==> F32
    convertInstr mem args (FConvertIU BS64 BS32) = return $ I32 ==> F64
    convertInstr mem args (FConvertIU BS64 BS64) = return $ I64 ==> F64
    convertInstr mem args (FConvertIS BS32 BS32) = return $ I32 ==> F32
    convertInstr mem args (FConvertIS BS32 BS64) = return $ I64 ==> F32
    convertInstr mem args (FConvertIS BS64 BS32) = return $ I32 ==> F64
    convertInstr mem args (FConvertIS BS64 BS64) = return $ I64 ==> F64
    convertInstr mem args F32DemoteF64 = return $ F64 ==> F32
    convertInstr mem args F64PromoteF32 = return $ F32 ==> F64
    convertInstr mem args (IReinterpretF BS32) = return $ F32 ==> I32
    convertInstr mem args (IReinterpretF BS64) = return $ F64 ==> I64
    convertInstr mem args (FReinterpretI BS32) = return $ I32 ==> F32
    convertInstr mem args (FReinterpretI BS64) = return $ I64 ==> F64
    convertInstr mem args _ = error "Wrong instr or wrong type"