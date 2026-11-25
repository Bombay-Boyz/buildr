module Buildr.Event
  ( Event (..)
  , EventKind (..)
  , ValueKind (..)
  , EventSource (..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)

data ValueKind
  = Observed
  | Reported
  | Derived
  | Estimate
  deriving (Eq, Show)

data EventSource
  = SrcCabal
  | SrcGhc
  | SrcPlan
  | SrcGraph
  | SrcHistory
  | SrcBuildr
  deriving (Eq, Show)

data EventKind
  = BuildStarted
  | PlanReady
  | GraphKnown
  | UnitStarted
  | UnitCompleted
  | UnitFailed
  | UnitStopped
  | ModuleStarted
  | ModulePhase
  | ModuleFinished
  | ModuleSkipped
  | Diagnostic
  | Opaque
  | SlotsChanged
  | Linking
  | Explanation
  | ExplanationUnavailable
  | BuildDone
  deriving (Eq, Show)

data Event = Event
  { eventVersion   :: Int
  , eventSeq       :: Int64
  , eventTimestamp :: Int64
  , eventKind      :: EventKind
  , eventSource    :: EventSource
  , eventValKind   :: ValueKind
  , eventSummary   :: Text
  } deriving (Eq, Show)
