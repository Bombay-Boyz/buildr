{-# LANGUAGE CPP #-}
-- |
-- Module      : Buildr.Signal
-- Description : Process group signal forwarding, terminal restoration, and graceful cancellation.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Manages POSIX signal interception, child process group propagation, terminal mode
-- restoration, and partial cancellation summary rendering per §20.
-- Also provides Windows Job Object / process tree termination abstractions per §7 and §23.
module Buildr.Signal
  ( -- * Signal Forwarding & Process Tree Termination
    forwardSignalToProcessGroup
  , forwardAbortSignals
  , terminateProcessTree
  , isWindowsProcessModel
  , withProcessGroupSignals
  , withWindowResizeHandler
    -- * Terminal State Restoration
  , setupTerminal
  , restoreTerminal
  , withTerminalRestored
    -- * Cancellation Outcomes
  , markModelCancelled
  , formatCancellationSummary
  ) where

import Buildr.Event (BuildOutcome (..))
import Buildr.Model (BuildModel (..))
import Buildr.View (renderSummaryBlock)
import Control.Concurrent (myThreadId, newMVar, modifyMVar)
import Control.Exception (IOException, bracket, bracket_, catch, throwTo)
import Control.Monad (unless, when)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import System.IO
  ( BufferMode (..)
  , hFlush
  , hSetBuffering
  , hSetEcho
  , stdin
  , stdout
  )
#if !defined(mingw32_HOST_OS)
import System.Posix.Signals
  ( Handler (Catch)
  , Signal
  , installHandler
  , sigINT
  , sigTERM
  , signalProcessGroup
  )
import System.Posix.Signals.Exts (sigWINCH)
#endif
#if defined(mingw32_HOST_OS)
import System.Process (ProcessHandle, getPid, terminateProcess)
#else
import System.Process (ProcessHandle, getPid)
#endif

-- | Indicates whether the host is executing under the Windows process model (Job Objects) rather than POSIX.
isWindowsProcessModel :: Bool
#if defined(mingw32_HOST_OS)
isWindowsProcessModel = True
#else
isWindowsProcessModel = False
#endif

-- | Terminates the child process and its entire sub-process tree using either Windows Job Object termination
-- or POSIX process group signal delivery.
terminateProcessTree :: ProcessHandle -> IO ()
terminateProcessTree ph = do
#if defined(mingw32_HOST_OS)
  terminateProcess ph `catch` ignoreIo
  where
    ignoreIo :: IOException -> IO ()
    ignoreIo _ = pure ()
#else
  forwardAbortSignals ph
#endif

#if defined(mingw32_HOST_OS)
-- | Dummy Signal identifier on Windows systems.
type Signal = Int

-- | Emulated SIGINT constant for Windows.
sigINT :: Signal
sigINT = 2

-- | Emulated SIGTERM constant for Windows.
sigTERM :: Signal
sigTERM = 15

-- | On Windows, forwardSignalToProcessGroup terminates the process handle directly.
forwardSignalToProcessGroup :: Signal -> ProcessHandle -> IO ()
forwardSignalToProcessGroup _ ph = terminateProcess ph `catch` ignoreIo
  where
    ignoreIo :: IOException -> IO ()
    ignoreIo _ = pure ()
#else
-- | Forwards a POSIX signal directly to the child process's process group.
-- When a process is spawned with 'create_group = True', its process group ID
-- equals its PID. Safe against already-terminated processes.
forwardSignalToProcessGroup :: Signal -> ProcessHandle -> IO ()
forwardSignalToProcessGroup sig ph = do
  mPid <- getPid ph
  case mPid of
    Just pid ->
      signalProcessGroup sig pid `catch` ignoreIo
    Nothing -> pure ()
  where
    ignoreIo :: IOException -> IO ()
    ignoreIo _ = pure ()
#endif

-- | Forwards both SIGINT and SIGTERM to the process group.
forwardAbortSignals :: ProcessHandle -> IO ()
forwardAbortSignals ph = do
  forwardSignalToProcessGroup sigINT ph
  forwardSignalToProcessGroup sigTERM ph

-- | Installs signal interception for SIGINT and SIGTERM, forwarding them
-- to the child process group and invoking the cancellation callback.
withProcessGroupSignals :: ProcessHandle -> IO () -> IO a -> IO a
withProcessGroupSignals ph onAbort action = do
#if defined(mingw32_HOST_OS)
  -- On Windows, catch asynchronous interrupt exceptions and terminate the process tree
  action `catch` \ex -> do
    terminateProcess ph `catch` (\(_ :: IOException) -> pure ())
    onAbort `catch` (\(_ :: IOException) -> pure ())
    throwTo (undefined) (ex :: IOException)
#else
  mainTid <- myThreadId
  abortVar <- newMVar False
  let handleSignal sig = do
        alreadyAborted <- modifyMVar abortVar (\b -> pure (True, b))
        unless alreadyAborted $ do
          forwardSignalToProcessGroup sig ph
          onAbort `catch` ignoreIo
          throwTo mainTid (IOError Nothing Interrupted "signal" "interrupted" Nothing Nothing)
      ignoreIo :: IOException -> IO ()
      ignoreIo _ = pure ()
      installSig sig = installHandler sig (Catch (handleSignal sig)) Nothing
      restoreSig sig old = installHandler sig old Nothing

  bracket (installSig sigINT) (\old -> restoreSig sigINT old) $ \_ ->
    bracket (installSig sigTERM) (\old -> restoreSig sigTERM old) $ \_ ->
      action
#endif

-- | Installs a SIGWINCH listener that triggers an action whenever the terminal window is resized.
withWindowResizeHandler :: IO () -> IO a -> IO a
withWindowResizeHandler onResize action = do
#if defined(mingw32_HOST_OS)
  action
#else
  let installWinch = installHandler sigWINCH (Catch onResize) Nothing
      restoreWinch old = installHandler sigWINCH old Nothing
  bracket installWinch restoreWinch (const action)
#endif

-- | Configures terminal settings: hides cursor and disables echo/buffering when attached to a TTY.
setupTerminal :: Bool -> IO ()
setupTerminal isTty = do
  putStr "\ESC[?25l"
  hFlush stdout
  when isTty $ do
    hSetBuffering stdin NoBuffering `catch` ignoreIo
    hSetEcho stdin False `catch` ignoreIo
  where
    ignoreIo :: IOException -> IO ()
    ignoreIo _ = pure ()

-- | Guarantees terminal restoration: reveals cursor, restores line buffering, and enables echo.
restoreTerminal :: Bool -> IO ()
restoreTerminal isTty = do
  putStr "\ESC[?25h\n"
  hFlush stdout
  when isTty $ do
    hSetEcho stdin True `catch` ignoreIo
    hSetBuffering stdin LineBuffering `catch` ignoreIo
  where
    ignoreIo :: IOException -> IO ()
    ignoreIo _ = pure ()

-- | Exception-safe wrapper guaranteeing terminal restoration under all exit paths
-- including asynchronous exceptions, interrupts, and early exits.
withTerminalRestored :: Bool -> IO a -> IO a
withTerminalRestored isTty =
  bracket_ (setupTerminal isTty) (restoreTerminal isTty)

-- | Updates the pure model to record cancellation per §20.
markModelCancelled :: BuildModel -> BuildModel
markModelCancelled model = model
  { bmCompleted = True
  , bmOutcome   = Just BuildCancelled
  }

-- | Formats an honest partial summary block documenting completed modules and elapsed time.
formatCancellationSummary :: BuildModel -> Int64 -> Text
formatCancellationSummary model wallNs =
  renderSummaryBlock (markModelCancelled model) wallNs
