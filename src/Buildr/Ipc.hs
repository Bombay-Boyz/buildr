-- |
-- Module      : Buildr.Ipc
-- Description : Unix domain socket IPC server and client transport.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- IPC transport linking buildr-ghc compiler wrappers to the buildr parent process:
--   * Asynchronous server accepting parallel compiler wrapper connections (§8.2).
--   * Streaming newline-delimited JSON Lines event decoding.
--   * Clean socket lifecycle with robust file removal and signal-safe teardown.
--   * 100% total stream parsing without partial list indexing (Standard 1.1).
module Buildr.Ipc
  ( -- * Server Types & Lifecycle
    IpcServerHandle
  , startIpcServer
  , stopIpcServer
    -- * Client Transport
  , sendIpcEvent
  ) where

import Buildr.Error (IpcError (..))
import Buildr.Event (Event, decodeEventLine, encodeEventLine)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (SomeException, bracket, handle, try)
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)

-- | Opaque handle managing the lifecycle of an active IPC server listener.
data IpcServerHandle = IpcServerHandle
  { ishSocket :: !Socket
  , ishThread :: !(Async ())
  , ishPath   :: !FilePath
  }

-- | Total partition of a list into its leading elements and final element.
splitLast :: [a] -> Maybe ([a], a)
splitLast = \case
  []     -> Nothing
  [x]    -> Just ([], x)
  (x:xs) -> case splitLast xs of
              Nothing      -> Nothing
              Just (ys, z) -> Just (x:ys, z)

-- | Starts an asynchronous Unix domain socket server dispatching decoded events to a callback.
startIpcServer :: FilePath -> (Event -> IO ()) -> IO (Either IpcError IpcServerHandle)
startIpcServer path onEvent = do
  res <- try $ do
    _ <- handle (\(_ :: SomeException) -> pure ()) (removeFile path)
    sock <- socket AF_UNIX Stream defaultProtocol
    bind sock (SockAddrUnix path)
    listen sock 128
    thread <- async (acceptLoop sock onEvent)
    pure (IpcServerHandle sock thread path)
  case res of
    Left (ex :: SomeException) ->
      pure (Left (IpcSocketCreationFailed path (T.pack (show ex))))
    Right serverHandle ->
      pure (Right serverHandle)

-- | Accepts incoming wrapper client connections indefinitely until cancelled.
acceptLoop :: Socket -> (Event -> IO ()) -> IO ()
acceptLoop serverSock onEvent = forever $ do
  bracket (accept serverSock) (\(clientSock, _) -> close clientSock) $ \(clientSock, _) ->
    readClientStream clientSock BS.empty onEvent

-- | Reads framed newline-delimited JSON Lines from a client connection buffer.
readClientStream :: Socket -> BS.ByteString -> (Event -> IO ()) -> IO ()
readClientStream clientSock buffer onEvent = do
  chunk <- recv clientSock 4096
  if BS.null chunk
    then
      -- Process any remaining trailing line
      case decodeEventLine buffer of
        Just ev -> onEvent ev
        Nothing -> pure ()
    else do
      let combined = buffer <> chunk
          lines' = BSC8.split '\n' combined
      case splitLast lines' of
        Nothing ->
          readClientStream clientSock BS.empty onEvent
        Just ([], single) ->
          readClientStream clientSock single onEvent
        Just (completeLines, remainder) -> do
          mapM_ processLine completeLines
          readClientStream clientSock remainder onEvent
  where
    processLine line
      | BS.null line = pure ()
      | otherwise = case decodeEventLine line of
          Just ev -> onEvent ev
          Nothing -> pure ()

-- | Gracefully terminates an IPC server, closing sockets and removing the socket file.
stopIpcServer :: IpcServerHandle -> IO ()
stopIpcServer handle' = do
  cancel (ishThread handle')
  _ <- handle (\(_ :: SomeException) -> pure ()) (close (ishSocket handle'))
  _ <- handle (\(_ :: SomeException) -> pure ()) (removeFile (ishPath handle'))
  pure ()

-- | Connects to an active IPC server and transmits an encoded Event record.
sendIpcEvent :: FilePath -> Event -> IO (Either IpcError ())
sendIpcEvent path ev = do
  res <- try $ bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    connect sock (SockAddrUnix path)
    sendAll sock (encodeEventLine ev)
  case res of
    Left (ex :: SomeException) ->
      pure (Left (IpcClientConnectionFailed path (T.pack (show ex))))
    Right () ->
      pure (Right ())
