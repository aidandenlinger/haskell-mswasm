{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified System.Directory              as Directory

import qualified Data.ByteString.Lazy          as LBS

import qualified Language.Wasm                 as Wasm
import qualified Language.Wasm.Script          as Script

main :: IO ()
main = do
  files           <- Directory.listDirectory "tests/samples"
  scriptTestCases <- (`mapM` files) $ \file -> do
    Right script <- Wasm.parseScript <$> LBS.readFile ("tests/samples/" ++ file)
    return $ testCase file $ do
      Script.runScript
        (\msg assert -> assertFailure
          ("Failed assert: " ++ msg ++ ". Assert " ++ show assert)
        )
        script
  defaultMain $ testGroup "Wasm Core Test Suit" scriptTestCases

-- | Basic function to take in filepaths to WasmText input and output binary
--   run 'stack ghci tests/Test.hs' to run this function
--   use 'xxd -b path-to-bin' to examine the binary, or toModule to go back
toBinary :: String -> String -> IO ()
toBinary input output = do
  -- read the text file into a lazy bytestring buffer
  content <- LBS.readFile input
  -- parse the text, which returns Either String (error) Wasm.Module
  case Wasm.parse content of
    -- When it's a Wasm.Module, encode the module into binary and output
    Right mod    -> LBS.writeFile output $ Wasm.encodeLazy mod
    -- Otherwise, print error string
    Left  reason -> putStrLn reason

-- | Hardcoded toBinary with exact filepaths, run from root dir
toBinaryHC :: IO ()
toBinaryHC =
  toBinary "./tests/toBinaryTest/test.wat" "./tests/toBinaryTest/bin.wasm"

-- | Hardcoded toBinary to take in the filename (not filepath!) of a wat file in
--   toBinaryTest, outputs to bin.wasm in toBinaryTest
toBinaryTest :: String -> IO ()
toBinaryTest input =
  toBinary ("./tests/toBinaryTest/" ++ input) "./tests/toBinaryTest/bin.wasm"

compile :: String -> IO ()
compile input =
  toBinary ("./tests/toBinaryTest/" ++ input ++ ".wat") ("./tests/graalBin/" ++ input ++ ".wasm")

-- | Takes in a filepath to a binary and outputs the Module representation.
showFullModule :: String -> IO ()
showFullModule input = do
  binary <- LBS.readFile input
  case Wasm.decodeLazy binary of
    Right mod    -> print mod
    Left  reason -> putStrLn reason

showModule :: String -> IO ()
showModule s = Wasm.showModule s >>= putStrLn

-- | Hardcoded toModule to examine the binary bin.wasm in toBinaryTest
toModuleHC :: IO ()
toModuleHC = showFullModule "./tests/toBinaryTest/bin.wasm"
