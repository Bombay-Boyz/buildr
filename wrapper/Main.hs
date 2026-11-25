-- |
-- Module      : Main
-- Description : Transparent GHC compiler wrapper (buildr-ghc).
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Passes through informational queries (--info, --numeric-version) untouched.
-- Adds JSON diagnostics and timing dumps to real compilation invocations (§8.1).
module Main (main) where

import System.Environment (getArgs, lookupEnv)
import System.Exit (exitWith)
import System.Process (proc, createProcess, waitForProcess)

-- | Query flags that must never receive extra compilation flags or interference.
isQueryFlag :: String -> Bool
isQueryFlag arg = arg `elem`
  [ "--info"
  , "--numeric-version"
  , "--supported-languages"
  , "--supported-extensions"
  , "--print-libdir"
  , "--print-project-version"
  , "--print-global-package-db"
  , "-V"
  , "--version"
  ]

-- | Determines if an argument list represents a query call rather than a compile call.
isQueryInvocation :: [String] -> Bool
isQueryInvocation = any isQueryFlag

-- | Wraps GHC invocation transparently, appending diagnostic flags only for compile passes.
main :: IO ()
main = do
  args <- getArgs
  if isQueryInvocation args
    then runGhc args
    else do
      whyMode <- lookupEnv "BUILDR_WHY"
      let extraFlags =
            [ "-fdiagnostics-as-json"
            , "-ddump-timings"
            ] <> [ "-ddump-hi-diffs" | whyMode == Just "1" ]
      runGhc (args <> extraFlags)

-- | Invokes the actual GHC executable with standard handles inherited.
runGhc :: [String] -> IO ()
runGhc args = do
  (_, _, _, ph) <- createProcess (proc "ghc" args)
  code <- waitForProcess ph
  exitWith code
