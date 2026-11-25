module Buildr.Model
  ( ModuleStatus (..)
  , UnitStatus (..)
  , BuildModel (..)
  , initialModel
  , applyEvent
  ) where

import Buildr.Event
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data ModuleStatus
  = ModPending
  | ModCompiling
  | ModDone
  | ModSkipped
  | ModFailed
  | ModBlocked
  | ModNotReached
  deriving (Eq, Show)

data UnitStatus
  = UnitStatusPlanned
  | UnitStatusBuilding
  | UnitStatusCompleted
  | UnitStatusFailed
  | UnitStatusStopped
  deriving (Eq, Show)

data BuildModel = BuildModel
  { modelUnits   :: Map Text UnitStatus
  , modelModules :: Map Text ModuleStatus
  , modelDone    :: Bool
  } deriving (Eq, Show)

initialModel :: BuildModel
initialModel = BuildModel
  { modelUnits   = Map.empty
  , modelModules = Map.empty
  , modelDone    = False
  }

applyEvent :: BuildModel -> Event -> BuildModel
applyEvent model ev = case eventKind ev of
  BuildDone -> model { modelDone = True }
  _         -> model
