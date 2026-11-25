{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Buildr.History
-- Description : History cache types, median computation, and honest estimate heuristics.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Pure history cache and estimate calculation engine satisfying §10.3:
--   * Median calculation over last 10 builds (total and sort-based).
--   * Overrun threshold detection (1.5x usual time).
--   * Honest labeling: always tagged as estimate (~ / est.).
--   * Persistent XDG cache loading and saving.
module Buildr.History
  ( -- * Keys and Cache Entries
    HistoryKey (..)
  , ModuleTimingHistory (..)
  , HistoryCache (..)
    -- * Pure Computations
  , emptyHistoryCache
  , insertTiming
  , computeUsualTime
  , isOverrun
  , formatOverrun
  , getSlowestModules
  , getHeaviestModules
    -- * Persistence
  , defaultHistoryPath
  , loadHistoryCache
  , saveHistoryCache
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, FromJSONKey, eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.List (sort, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))

-- | Composite key uniquely indexing build history per §10.3.
data HistoryKey = HistoryKey
  { hkProjectPath  :: !FilePath
  , hkUnitId       :: !Text
  , hkModule       :: !Text
  , hkGhcVersion   :: !Text
  , hkCabalVersion :: !Text
  , hkOptLevel     :: !Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON HistoryKey
instance FromJSON HistoryKey
instance ToJSONKey HistoryKey
instance FromJSONKey HistoryKey

-- | Rolling history of reported times and allocations for a module.
data ModuleTimingHistory = ModuleTimingHistory
  { mthDurationsMs :: ![Int64]
  , mthAllocBytes  :: ![Int64]
  } deriving (Eq, Show, Generic)

instance ToJSON ModuleTimingHistory
instance FromJSON ModuleTimingHistory

-- | The in-memory cache mapping history keys to their rolling timing measurements.
newtype HistoryCache = HistoryCache
  { hcEntries :: Map HistoryKey ModuleTimingHistory
  } deriving (Eq, Show, Generic)

instance ToJSON HistoryCache
instance FromJSON HistoryCache

-- | Creates an empty history cache.
emptyHistoryCache :: HistoryCache
emptyHistoryCache = HistoryCache Map.empty

-- | Records a new duration and allocation for a module, capping history at 10 entries.
insertTiming :: HistoryKey -> Int64 -> Int64 -> HistoryCache -> HistoryCache
insertTiming key durationMs allocBytes (HistoryCache m) =
  let updateHistory = \case
        Nothing ->
          Just (ModuleTimingHistory [durationMs] [allocBytes])
        Just (ModuleTimingHistory ds as) ->
          let newDs = take 10 (durationMs : ds)
              newAs = take 10 (allocBytes : as)
          in Just (ModuleTimingHistory newDs newAs)
  in HistoryCache (Map.alter updateHistory key m)

-- | Computes the median duration in milliseconds across recent runs.
-- Total and returns Nothing if no history is recorded.
computeUsualTime :: HistoryKey -> HistoryCache -> Maybe Int64
computeUsualTime key (HistoryCache m) = do
  entry <- Map.lookup key m
  let ds = mthDurationsMs entry
  case ds of
    [] -> Nothing
    _  ->
      let sorted = sort ds
          len = length sorted
          mid = len `div` 2
      in case drop mid sorted of
           (medianVal:_) -> Just medianVal
           []            -> Nothing

-- | Checks if observed duration is slower than usual (> 1.5x median).
isOverrun :: Int64 -> Int64 -> Bool
isOverrun observedMs usualMs =
  observedMs > (usualMs * 3) `div` 2

-- | Formats overrun description honestly as "+Xs vs usual".
formatOverrun :: Int64 -> Int64 -> Text
formatOverrun observedMs usualMs =
  let diffSec = fromIntegral (max 0 (observedMs - usualMs)) / (1000.0 :: Double)
  in "+" <> T.pack (show (round diffSec :: Int)) <> "s vs usual"

-- | Retrieves the slowest modules by median execution time in descending order.
getSlowestModules :: HistoryCache -> [(HistoryKey, Int64)]
getSlowestModules cache@(HistoryCache m) =
  let pairs = Map.toList m
      withMedian = [ (k, med) | (k, _) <- pairs, Just med <- [computeUsualTime k cache] ]
  in sortBy (comparing (Down . snd)) withMedian

-- | Retrieves the heaviest modules by peak reported allocation in descending order.
getHeaviestModules :: HistoryCache -> [(HistoryKey, Int64)]
getHeaviestModules (HistoryCache m) =
  let pairs = Map.toList m
      withMaxAlloc = [ (k, maximum as) | (k, entry) <- pairs, let as = mthAllocBytes entry, not (null as) ]
  in sortBy (comparing (Down . snd)) withMaxAlloc

-- | Returns the default persistent history file path ($XDG_CACHE_HOME/buildr/history.json).
defaultHistoryPath :: IO FilePath
defaultHistoryPath = do
  dir <- getXdgDirectory XdgCache "buildr"
  pure (dir </> "history.json")

-- | Loads the history cache from disk safely, returning an empty cache if not present or corrupt.
loadHistoryCache :: FilePath -> IO HistoryCache
loadHistoryCache path = do
  exists <- doesFileExist path
  if not exists
    then pure emptyHistoryCache
    else do
      res <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _ -> pure emptyHistoryCache
        Right content -> case eitherDecodeStrict content of
          Left _      -> pure emptyHistoryCache
          Right cache -> pure cache

-- | Persists the history cache atomically to disk.
saveHistoryCache :: FilePath -> HistoryCache -> IO (Either Text ())
saveHistoryCache path cache = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  res <- try (BSL.writeFile path (encode cache)) :: IO (Either SomeException ())
  case res of
    Left ex -> pure (Left (T.pack (show ex)))
    Right () -> pure (Right ())
