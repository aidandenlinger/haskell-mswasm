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
import qualified Language.Wasm.Lexer           as Lexer
import qualified Language.Wasm.Parser          as Parser
import qualified Language.Wasm.Binary          as Binary

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

toBinary :: String -> IO ()
toBinary input = do
  content <- LBS.readFile input
  case Lexer.scanner content >>= Parser.parseModule of
    Right mod ->
      LBS.writeFile "/home/aidan/Desktop/bin.wasm" $ Binary.dumpModuleLazy mod
    Left reason -> putStrLn reason
