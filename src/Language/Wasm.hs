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
    showModule,
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

showModule :: String -> IO String
showModule input = do
  binary <- LBS.readFile input
  case decodeLazy binary of
    Right mod -> return $ prettyPrint mod
    Left reason -> return reason
  where
    prettyPrint :: Module -> String
    prettyPrint Module { functions = t}  = concatMap ((++ "\n\nnew function:\n\n") . functionPrint) t

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
    
