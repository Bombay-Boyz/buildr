module Buildr.History
  ( HistoryKey (..)
  , HistoryEntry (..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)

data HistoryKey = HistoryKey
  { hkProjectPath  :: FilePath
  , hkUnitId       :: Text
  , hkModule       :: Text
  , hkGhcVersion   :: Text
  , hkCabalVersion :: Text
  , hkOptLevel     :: Text
  } deriving (Eq, Ord, Show)

data HistoryEntry = HistoryEntry
  { heKey          :: HistoryKey
  , heDurationsMs  :: [Int64]
  , heAllocBytes   :: [Int64]
  } deriving (Eq, Show)
