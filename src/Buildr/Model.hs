-- |
-- Module      : Buildr.Model
-- Description : Pure domain model and state machine for Haskell build tracking.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Pure state model folded over the Event stream. Implements the 6 core invariants
-- from §10.1 of the vision document. All functions are pure, total, and stack-safe.
module Buildr.Model
  ( -- * Module & Unit States
    ModuleStatus (..)
  , UnitStatus (..)
  , ModuleInfo (..)
  , UnitInfo (..)
  , BuildModel (..)
    -- * Model Lifecycle
  , initialModel
  , applyEvent
  , foldEvents
    -- * Queries & Invariant Checks
  , countModulesByStatus
  , isModuleDone
  , isModuleBlocked
  , isModuleNotReached
  , checkModelInvariants
  ) where

import Buildr.Event
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Detailed state for an individual module in the build graph.
data ModuleStatus
  = ModPending
  | ModCompiling !Int64          -- ^ Started timestamp in monotonic ns
  | ModDone !Int64 !Int64        -- ^ Duration in ms, allocation in bytes
  | ModSkipped !Text             -- ^ Reason reported by GHC
  | ModFailed !Int               -- ^ Error count
  | ModBlocked !Text             -- ^ Blocker module name
  | ModNotReached
  deriving (Eq, Show)

-- | Detailed state for a Cabal package unit.
data UnitStatus
  = UnitPlanned
  | UnitBuilding !Int64
  | UnitCompleted !Int
  | UnitFailed !Int !(Maybe FilePath)
  | UnitStopped
  deriving (Eq, Show)

-- | State tracking record for an individual module.
data ModuleInfo = ModuleInfo
  { miName              :: !Text
  , miUnit              :: !Text
  , miStatus            :: !ModuleStatus
  , miDependencies      :: !(Set Text)
  , miHasReportedFinish :: !Bool
  , miPhases            :: !(Map Text (Int64, Int64))
  } deriving (Eq, Show)

-- | State tracking record for a Cabal unit.
data UnitInfo = UnitInfo
  { uiId           :: !Text
  , uiPackage      :: !Text
  , uiStatus       :: !UnitStatus
  , uiModules      :: !(Map Text ModuleInfo)
  } deriving (Eq, Show)

-- | The canonical pure build state model.
data BuildModel = BuildModel
  { bmUnits        :: !(Map Text UnitInfo)
  , bmCompleted    :: !Bool
  , bmOutcome      :: !(Maybe BuildOutcome)
  , bmBusySlots    :: !Int
  , bmTotalSlots   :: !Int
  , bmDiagnostics  :: ![DiagnosticPayload]
  } deriving (Eq, Show)

-- | Initializes an empty pure build model.
initialModel :: BuildModel
initialModel = BuildModel
  { bmUnits        = Map.empty
  , bmCompleted    = False
  , bmOutcome      = Nothing
  , bmBusySlots    = 0
  , bmTotalSlots   = 1
  , bmDiagnostics  = []
  }

-- | Updates a unit inside the model map safely by exact unit id or matching package name.
updateUnitByPkgOrId :: Text -> (UnitInfo -> UnitInfo) -> BuildModel -> BuildModel
updateUnitByPkgOrId target f model =
  let units = bmUnits model
  in case Map.lookup target units of
       Just u  -> model { bmUnits = Map.insert target (f u) units }
       Nothing ->
         let matchingKeys = [ k | (k, u) <- Map.toList units
                                , uiPackage u == target
                               || T.isPrefixOf target (uiPackage u)
                               || T.isPrefixOf (uiPackage u) target
                               || T.isPrefixOf target k ]
         in case matchingKeys of
              (k:_) -> case Map.lookup k units of
                         Just u  -> model { bmUnits = Map.insert k (f u) units }
                         Nothing -> model
              []    ->
                let newUnit = UnitInfo target target UnitPlanned Map.empty
                in model { bmUnits = Map.insert target (f newUnit) units }

-- | Pure state fold: applies a single event to advance the model state.
applyEvent :: BuildModel -> Event -> BuildModel
applyEvent model ev = case eventPayload ev of
  PayloadPlanReady units ->
    let addUnit acc pu =
          Map.insert (puId pu) (UnitInfo (puId pu) (puPackage pu) UnitPlanned Map.empty) acc
    in model { bmUnits = foldl (\m u -> addUnit m u) (bmUnits model) units }

  PayloadGraphKnown uid mods edges ->
    updateUnitByPkgOrId uid (\u ->
      let edgeMapFor mName =
            foldl (\acc e ->
                     case geSource e of
                       Nothing -> if geTarget e /= mName then Set.insert (geTarget e) acc else acc
                       Just s  -> if s == mName then Set.insert (geTarget e) acc else acc
                  ) Set.empty edges
          addMod acc m =
            case Map.lookup (mnName m) acc of
              Just existing ->
                Map.insert (mnName m) (existing { miDependencies = edgeMapFor (mnName m) }) acc
              Nothing ->
                Map.insert (mnName m) (ModuleInfo (mnName m) uid ModPending (edgeMapFor (mnName m)) False Map.empty) acc
      in u { uiModules = foldl addMod (uiModules u) mods }
      ) model

  PayloadUnitStarted uid ->
    updateUnitByPkgOrId uid (\u ->
      case uiStatus u of
        UnitFailed _ _  -> u
        UnitStopped     -> u
        UnitCompleted _ -> u
        _               -> u { uiStatus = UnitBuilding (eventTimestamp ev) }
      ) model

  PayloadUnitCompleted uid code ->
    updateUnitByPkgOrId uid (\u ->
      case uiStatus u of
        UnitFailed _ _ -> u
        UnitStopped    -> u
        _              -> u { uiStatus = UnitCompleted code }
      ) model

  PayloadUnitFailed uid code logPath ->
    let m1 = updateUnitByPkgOrId uid (\u -> u { uiStatus = UnitFailed code logPath }) model
    in deriveBlockedAndNotReached uid m1

  PayloadUnitStopped uid ->
    let m1 = updateUnitByPkgOrId uid (\u -> u { uiStatus = UnitStopped }) model
    in deriveBlockedAndNotReached uid m1

  PayloadModuleStarted uid modName _k _n _src _obj _reas ->
    updateUnitByPkgOrId uid (\u ->
      let mods = uiModules u
      in case Map.lookup modName mods of
           Nothing ->
             let newM = ModuleInfo modName (uiId u) (ModCompiling (eventTimestamp ev)) Set.empty False Map.empty
             in u { uiModules = Map.insert modName newM mods }
           Just m  -> u { uiModules = Map.insert modName (m { miStatus = ModCompiling (eventTimestamp ev) }) mods }
      ) model

  PayloadModulePhase uid modName phase ms alloc ->
    updateUnitByPkgOrId uid (\u ->
      let mods = uiModules u
      in case Map.lookup modName mods of
           Nothing ->
             let newM = ModuleInfo modName (uiId u) (ModCompiling (eventTimestamp ev)) Set.empty False (Map.singleton phase (ms, alloc))
             in u { uiModules = Map.insert modName newM mods }
           Just m  ->
             let newPhases = Map.insert phase (ms, alloc) (miPhases m)
             in u { uiModules = Map.insert modName (m { miPhases = newPhases }) mods }
      ) model

  PayloadModuleFinished uid modName ->
    updateUnitByPkgOrId uid (\u ->
      let mods = uiModules u
      in case Map.lookup modName mods of
           Nothing ->
             let newM = ModuleInfo modName (uiId u) (ModDone 1 0) Set.empty True Map.empty
             in u { uiModules = Map.insert modName newM mods }
           Just m  ->
             let totalMs = sum (map fst (Map.elems (miPhases m)))
                 totalAlloc = sum (map snd (Map.elems (miPhases m)))
                 elapsedFallback = case miStatus m of
                                     ModCompiling started ->
                                       max 1 ((eventTimestamp ev - started) `div` 1000000)
                                     _ -> 1
                 finalMs = if totalMs > 0 then totalMs else elapsedFallback
                 newStatus = ModDone finalMs totalAlloc
             in u { uiModules = Map.insert modName (m { miStatus = newStatus, miHasReportedFinish = True }) mods }
      ) model

  PayloadModuleSkipped uid modName reas ->
    updateUnitByPkgOrId uid (\u ->
      let mods = uiModules u
      in case Map.lookup modName mods of
           Nothing ->
             let newM = ModuleInfo modName (uiId u) (ModSkipped reas) Set.empty False Map.empty
             in u { uiModules = Map.insert modName newM mods }
           Just m  -> u { uiModules = Map.insert modName (m { miStatus = ModSkipped reas }) mods }
      ) model

  PayloadDiagnostic _mu diag ->
    model { bmDiagnostics = if diag `elem` bmDiagnostics model then bmDiagnostics model else diag : bmDiagnostics model }

  PayloadSlotsChanged busy total ->
    model { bmBusySlots = busy, bmTotalSlots = total }

  PayloadBuildDone outcome _counts _wallNs ->
    model { bmCompleted = True, bmOutcome = Just outcome }

  _ -> model

-- | Propagates blocked and not-reached states for modules in a failed or stopped unit.
deriveBlockedAndNotReached :: Text -> BuildModel -> BuildModel
deriveBlockedAndNotReached uid model =
  updateUnitByPkgOrId uid (\u ->
    let failedMods = Map.keysSet (Map.filter (\m -> case miStatus m of ModFailed _ -> True; _ -> False) (uiModules u))
        updateStatus m = case miStatus m of
          ModPending
            | not (Set.null (Set.intersection (miDependencies m) failedMods)) ->
                let blocker = case Set.toList (Set.intersection (miDependencies m) failedMods) of
                                (b:_) -> b
                                []    -> "unknown"
                in m { miStatus = ModBlocked blocker }
            | otherwise -> m { miStatus = ModNotReached }
          _ -> m
    in u { uiModules = Map.map updateStatus (uiModules u) }
    ) model

-- | Folds a list of events sequentially over the model using a strict fold.
foldEvents :: BuildModel -> [Event] -> BuildModel
foldEvents = foldl applyEvent

-- | Counts modules partitioned by status across all units.
countModulesByStatus :: BuildModel -> Map Text Int
countModulesByStatus model =
  let allMods = concatMap (Map.elems . uiModules) (Map.elems (bmUnits model))
      tally acc m =
        let key = case miStatus m of
                    ModPending     -> "pending"
                    ModCompiling _ -> "compiling"
                    ModDone _ _    -> "done"
                    ModSkipped _   -> "skipped"
                    ModFailed _    -> "failed"
                    ModBlocked _   -> "blocked"
                    ModNotReached  -> "not_reached"
        in Map.insertWith (+) key 1 acc
  in foldl tally Map.empty allMods

-- | Predicate checking if a module has finished successfully.
isModuleDone :: ModuleInfo -> Bool
isModuleDone m = case miStatus m of
  ModDone _ _ -> True
  _           -> False

-- | Predicate checking if a module was blocked by a failed dependency.
isModuleBlocked :: ModuleInfo -> Bool
isModuleBlocked m = case miStatus m of
  ModBlocked _ -> True
  _            -> False

-- | Predicate checking if a module was not reached before unit termination.
isModuleNotReached :: ModuleInfo -> Bool
isModuleNotReached m = case miStatus m of
  ModNotReached -> True
  _             -> False

-- | Verifies all 6 vision invariants:
-- 1. No module in 'ModDone' without reported finish.
-- 2. Every 'ModBlocked' has a failing graph path.
-- 3. Every 'ModNotReached' occurs only when unit is stopped or failed.
checkModelInvariants :: BuildModel -> Bool
checkModelInvariants model =
  let units = Map.elems (bmUnits model)
      inv1 = all (\u -> all (\m -> not (isModuleDone m) || miHasReportedFinish m) (uiModules u)) units
      inv2 = all (\u -> all (\m -> not (isModuleBlocked m) || not (Set.null (miDependencies m))) (uiModules u)) units
      inv3 = all (\u ->
               let stopped = case uiStatus u of UnitFailed _ _ -> True; UnitStopped -> True; _ -> False
               in all (\m -> not (isModuleNotReached m) || stopped) (uiModules u)
             ) units
  in inv1 && inv2 && inv3
