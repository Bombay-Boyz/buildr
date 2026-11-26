-- |
-- Module      : Main
-- Description : Executable entry point for standalone buildr binary.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
module Main (main) where

import Buildr.Cli (runBuildrCli)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runBuildrCli
