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

-- | Hardcoded toBinary with exact filepaths so I don't screw that up
toBinaryHC :: IO ()
toBinaryHC = toBinary "/home/aidan/Desktop/testing/test.wat"
                      "/home/aidan/Desktop/testing/bin.wasm"

-- | Takes in a filepath to a binary and outputs the Module representation.
toModule :: String -> IO ()
toModule input = do
  binary <- LBS.readFile input
  case Wasm.decodeLazy binary of
    Right mod    -> print mod
    Left  reason -> putStrLn reason
