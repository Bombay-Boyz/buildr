module Main where

import Buildr.Core
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  putStrLn "Running buildr test suite..."
  let m0 = initialModel
  let glyph = stateGlyph StateDone
  if glyph == '●' && not (modelDone m0)
    then do
      putStrLn "All tests passed."
      exitSuccess
    else do
      putStrLn "Test failed!"
      exitFailure
