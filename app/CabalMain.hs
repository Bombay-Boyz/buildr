-- |
-- Module      : Main
-- Description : Executable entry point for native Cabal external command cabal-buildr.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Enables native Cabal external command invocation:
--
-- > cabal buildr [TARGETS] [--options...]
--
-- Passes informational and query flags directly through to Cabal (§18).
module Main (main) where

import Buildr.Cli (runCabalBuildrCli)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runCabalBuildrCli
