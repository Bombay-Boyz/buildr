-- |
-- Module      : Main
-- Description : Transparent GHC compiler wrapper (buildr-ghc).
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Transparent compiler wrapper adhering to §8.1 & §8.2:
--   * Passes through informational queries untouched.
--   * Appends JSON diagnostics and timing dumps to real compilation passes.
--   * Streams ModuleStarted / ModuleFinished / ModulePhase events to BUILDR_SOCKET.
--   * Completely preserves Cabal stdout/stderr streams.
module Main (main) where

import Buildr.Core
  ( EventPayload (..)
  , EventSource (..)
  , ValueKind (..)
  , formatDiagnostic
  , mkEvent
  , parseGhcDiagnosticJson
  , parseTimingLine
  , sendIpcEvent
  )
import Control.Concurrent.Async (async, wait)
import Control.Monad (unless)
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (Handle, hFlush, hIsEOF, stderr)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

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

-- | Extracts the unit identifier from GHC arguments (-this-unit-id).
extractUnitId :: [String] -> Text
extractUnitId = \case
  [] -> "unknown-unit"
  ("-this-unit-id" : uid : _) -> T.pack uid
  (_ : rest) -> extractUnitId rest

-- | Extracts the Haskell source file path being compiled from arguments.
extractSourceFile :: [String] -> FilePath
extractSourceFile = \case
  [] -> "Unknown.hs"
  (arg : rest)
    | ".hs" `isSuffixOf` arg || ".lhs" `isSuffixOf` arg -> arg
    | otherwise -> extractSourceFile rest

-- | Wraps GHC invocation transparently, sending IPC events if BUILDR_SOCKET is set.
main :: IO ()
main = do
  args <- getArgs
  if isQueryInvocation args
    then runGhc args
    else do
      socketEnv <- lookupEnv "BUILDR_SOCKET"
      whyMode <- lookupEnv "BUILDR_WHY"
      let unitId = extractUnitId args
          srcFile = extractSourceFile args
          modName = T.pack (takeWhile (/= '.') srcFile)
          extraFlags =
            [ "-fdiagnostics-as-json"
            , "-ddump-timings"
            ] <> [ "-ddump-hi-diffs" | whyMode == Just "1" ]

      startNs <- getMonotonicTimeNSec
      case socketEnv of
        Just sockPath -> do
          let startEv = mkEvent 0 (fromIntegral startNs :: Int64) SrcGhc ValueObserved
                          (PayloadModuleStarted unitId modName 1 1 srcFile (srcFile <> ".o") Nothing)
          _ <- sendIpcEvent sockPath startEv
          pure ()
        Nothing -> pure ()

      code <- runGhcCapture socketEnv unitId modName (args <> extraFlags)
      endNs <- getMonotonicTimeNSec

      case socketEnv of
        Just sockPath -> do
          let finishEv = case code of
                ExitSuccess ->
                  mkEvent 0 (fromIntegral endNs :: Int64) SrcGhc ValueReported
                    (PayloadModuleFinished unitId modName)
                ExitFailure c ->
                  mkEvent 0 (fromIntegral endNs :: Int64) SrcGhc ValueObserved
                    (PayloadUnitFailed unitId c Nothing)
          _ <- sendIpcEvent sockPath finishEv
          pure ()
        Nothing -> pure ()

      exitWith code

-- | Executes real GHC directly for queries.
runGhc :: [String] -> IO ()
runGhc args = do
  (_, _, _, ph) <- createProcess (proc "ghc" args)
  code <- waitForProcess ph
  exitWith code

-- | Executes real GHC with piped stderr, forwarding lines to stderr and parsing timings to IPC.
runGhcCapture :: Maybe FilePath -> Text -> Text -> [String] -> IO ExitCode
runGhcCapture mSocket unitId modName args = case mSocket of
  Nothing -> do
    (_, _, _, ph) <- createProcess (proc "ghc" args)
    waitForProcess ph
  Just sockPath -> do
    (_, _, Just hErr, ph) <- createProcess (proc "ghc" args)
      { std_in  = Inherit
      , std_out = Inherit
      , std_err = CreatePipe
      }
    errWorker <- async (teeStderr sockPath hErr)
    code <- waitForProcess ph
    wait errWorker
    pure code
  where
    teeStderr :: FilePath -> Handle -> IO ()
    teeStderr sockPath hErr = do
      eof <- hIsEOF hErr
      unless eof $ do
        line <- TIO.hGetLine hErr
        case parseGhcDiagnosticJson line of
          Just diag -> do
            nowNs <- getMonotonicTimeNSec
            let diagEv = mkEvent 0 (fromIntegral nowNs :: Int64) SrcGhc ValueReported
                           (PayloadDiagnostic (Just unitId) diag)
            _ <- sendIpcEvent sockPath diagEv
            TIO.hPutStr stderr (formatDiagnostic diag)
            hFlush stderr
          Nothing -> do
            TIO.hPutStrLn stderr line
            hFlush stderr
            case parseTimingLine line of
              Just (phase, ms, alloc) -> do
                nowNs <- getMonotonicTimeNSec
                let phaseEv = mkEvent 0 (fromIntegral nowNs :: Int64) SrcGhc ValueReported
                                (PayloadModulePhase unitId modName phase ms alloc)
                _ <- sendIpcEvent sockPath phaseEv
                pure ()
              Nothing -> pure ()
        teeStderr sockPath hErr
