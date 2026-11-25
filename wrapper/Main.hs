module Main where

import System.Environment (getArgs, getProgName)
import System.Process (callProcess)

main :: IO ()
main = do
  args <- getArgs
  _prog <- getProgName
  -- Wrapper for GHC: forward arguments to real ghc
  callProcess "ghc" args
