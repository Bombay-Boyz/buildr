-- |
-- Module      : Buildr.View
-- Description : Pure view renderers for Graph, Zen, Mosaic, Plain, and Linear modes.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Pure functions from BuildModel to formatted frames. Every view is total,
-- stack-safe, and independent of IO effects.
module Buildr.View
  ( -- * View Modes
    ViewMode (..)
    -- * View Dispatcher
  , renderView
    -- * Specific Renderers
  , renderGraphView
  , renderZenView
  , renderMosaicView
  , renderPlainView
  , renderLinearView
  ) where

import Buildr.Grammar
import Buildr.Model
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Closed sum of user-selectable viewing modes.
data ViewMode
  = ViewGraph
  | ViewZen
  | ViewMosaic
  | ViewPlain
  | ViewLinear
  deriving (Eq, Show)

-- | Top-level view rendering function dispatching purely by mode.
renderView :: ViewMode -> BuildModel -> Text
renderView = \case
  ViewGraph  -> renderGraphView
  ViewZen    -> renderZenView
  ViewMosaic -> renderMosaicView
  ViewPlain  -> renderPlainView
  ViewLinear -> renderLinearView

-- | Formats the default Graph Frontier view.
renderGraphView :: BuildModel -> Text
renderGraphView model =
  let units = Map.elems (bmUnits model)
      totalUnits = length units
      unitHeaders = map renderUnitSummary units
      footer = "Overall: " <> renderOverallBar model
  in T.unlines
       ( [ "=== buildr: Graph Frontier (Units: " <> T.pack (show totalUnits)
             <> " | Slots: " <> T.pack (show (bmBusySlots model)) <> "/"
             <> T.pack (show (bmTotalSlots model)) <> ") ==="
         ]
       <> unitHeaders
       <> [footer]
       )

-- | Renders a concise summary for a single unit within the graph view.
renderUnitSummary :: UnitInfo -> Text
renderUnitSummary u =
  let totalMods = Map.size (uiModules u)
      doneMods = length (filter isModuleDone (Map.elems (uiModules u)))
      ratio = if totalMods > 0 then fromIntegral doneMods / fromIntegral totalMods else 0.0
      bar = renderBar defaultBarWidth ratio
      statusLabel = case uiStatus u of
        UnitPlanned       -> "planned"
        UnitBuilding _    -> "building"
        UnitCompleted c   -> "completed (" <> T.pack (show c) <> ")"
        UnitFailed c _    -> "failed (" <> T.pack (show c) <> ")"
        UnitStopped       -> "stopped"
  in "  " <> uiPackage u <> " " <> bar <> " "
       <> T.pack (show doneMods) <> "/" <> T.pack (show totalMods) <> " [" <> statusLabel <> "]"

-- | Renders the single-line Zen view.
renderZenView :: BuildModel -> Text
renderZenView model =
  let counts = countModulesByStatus model
      doneCount = Map.findWithDefault 0 "done" counts
      failedCount = Map.findWithDefault 0 "failed" counts
      totalMods = sum (Map.elems counts)
      ratio = if totalMods > 0 then fromIntegral doneCount / fromIntegral totalMods else 0.0
      bar = renderBar defaultBarWidth ratio
      statusStr = if failedCount > 0
                    then "✖ " <> T.pack (show failedCount) <> " failed"
                    else "compiling"
  in "buildr " <> bar <> " " <> T.pack (show doneCount) <> "/" <> T.pack (show totalMods) <> " · " <> statusStr

-- | Renders the Mosaic view (compact character cell per module).
renderMosaicView :: BuildModel -> Text
renderMosaicView model =
  let allMods = concatMap (Map.elems . uiModules) (Map.elems (bmUnits model))
      cellForMod m = case miStatus m of
        ModPending     -> '·'
        ModCompiling _ -> '◐'
        ModDone _ _    -> '█'
        ModSkipped _   -> '◇'
        ModFailed _    -> '✖'
        ModBlocked _   -> '⊘'
        ModNotReached  -> '◌'
      cells = map cellForMod allMods
  in "buildr mosaic: [" <> T.pack cells <> "]"

-- | Renders the Plain / CI view suitable for non-interactive log files.
renderPlainView :: BuildModel -> Text
renderPlainView model =
  let units = Map.elems (bmUnits model)
      renderUnitPlain u =
        let mods = Map.elems (uiModules u)
            modLines = map (\m -> "  " <> T.singleton (stateGlyph (statusToVisual (miStatus m)))
                                    <> " " <> miName m <> " ("
                                    <> stateName (statusToVisual (miStatus m)) <> ")") mods
        in "Unit " <> uiId u <> ":\n" <> T.unlines modLines
  in T.concat (map renderUnitPlain units)

-- | Renders the Linear accessible view designed for screen readers.
renderLinearView :: BuildModel -> Text
renderLinearView model =
  let counts = countModulesByStatus model
      doneCount = Map.findWithDefault 0 "done" counts
      totalMods = sum (Map.elems counts)
      pct = if totalMods > 0 then (doneCount * 100) `div` totalMods else 0
  in "Build progress is at " <> T.pack (show pct) <> " percent. "
       <> T.pack (show doneCount) <> " of " <> T.pack (show totalMods) <> " modules compiled."

-- | Formats the composite overall progress bar for the footer.
renderOverallBar :: BuildModel -> Text
renderOverallBar model =
  let counts = countModulesByStatus model
      doneCount = Map.findWithDefault 0 "done" counts
      blockedCount = Map.findWithDefault 0 "blocked" counts
      totalMods = sum (Map.elems counts)
  in renderSegmentedBar defaultBarWidth doneCount blockedCount totalMods

-- | Maps domain module status to the visual state grammar.
statusToVisual :: ModuleStatus -> VisualState
statusToVisual = \case
  ModPending     -> VisWaiting
  ModCompiling _ -> VisCompiling
  ModDone _ _    -> VisDone
  ModSkipped _   -> VisSkipped
  ModFailed _    -> VisFailed
  ModBlocked _   -> VisBlocked
  ModNotReached  -> VisNotReached
