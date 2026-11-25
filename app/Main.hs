module Main where

import Buildr.Core
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  TIO.putStrLn ("buildr v" <> buildrVersion)
  case args of
    ["doctor"] -> putStrLn "buildr doctor: environment check"
    _          -> putStrLn "Usage: buildr build|watch|why|replay|stats|theme|doctor"
