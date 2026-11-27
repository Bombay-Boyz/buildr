-- |
-- Module      : Buildr.View
-- Description : Pure view renderers for Graph, Zen, Mosaic, Plain, and Linear modes.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Pure functions from BuildModel to formatted frames satisfying §11, §12, and §16.
-- Every view is total, stack-safe, and independent of IO effects.
module Buildr.View
  ( -- * View Modes
    ViewMode (..)
    -- * View Dispatcher
  , renderView
  , renderViewSelected
  , renderViewWithDims
    -- * Specific Renderers
  , renderGraphView
  , renderGraphViewSelected
  , renderGraphViewWithDims
  , renderUnitSummary
  , renderUnitSummaryWithWidth
  , renderOverallBar
  , renderOverallBarWithWidth
  , renderZenView
  , renderMosaicView
  , renderMosaicViewWithDims
  , renderPlainView
  , renderPlainViewCi
  , renderLinearView
    -- * Summary Block (§16)
  , renderSummaryBlock
  , computeHeaviestModule
  , computeSlowestModule
  , computeCriticalPath
  ) where

import Buildr.Event
  ( BuildOutcome (..)
  , DiagnosticPayload (..)
  , DiagnosticSeverity (..)
  , SourceSpan (..)
  , formatDiagnostic
  )
import Buildr.Grammar
import Buildr.Model
import Buildr.Terminal
  ( TerminalDimensions (..)
  , calculateDynamicBarWidth
  , defaultDimensions
  , effectiveViewMode
  , truncateToCols
  )
import Data.Int (Int64)
import Data.List (maximumBy, partition)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

-- | Top-level view rendering function dispatching purely by mode.
renderView :: ViewMode -> BuildModel -> Text
renderView mode = renderViewSelected mode Nothing

-- | Formats view with an optional selected row index for keyboard navigation (§12.6).
renderViewSelected :: ViewMode -> Maybe Int -> BuildModel -> Text
renderViewSelected = renderViewWithDims defaultDimensions

-- | Formats view with dynamic terminal dimensions (§7, §19, §20):
-- Dynamically collapses to Zen view if terminal height drops below 10 rows.
-- Recalculates progress bar widths and truncates module rows to terminal columns.
renderViewWithDims :: TerminalDimensions -> ViewMode -> Maybe Int -> BuildModel -> Text
renderViewWithDims dims mode mSel model =
  case effectiveViewMode mode dims of
    ViewZen    -> renderZenView model
    ViewGraph  -> renderGraphViewWithDims dims mSel model
    ViewMosaic -> renderMosaicViewWithDims dims model
    ViewPlain  -> renderPlainView model
    ViewLinear -> renderLinearView model

-- | Formats the default Graph Frontier view with optional keyboard selection cursor (§12.1, §12.6).
renderGraphViewSelected :: Maybe Int -> BuildModel -> Text
renderGraphViewSelected = renderGraphViewWithDims defaultDimensions

-- | Formats the Graph Frontier view with dynamic column width scaling and truncation (§19, §20).
renderGraphViewWithDims :: TerminalDimensions -> Maybe Int -> BuildModel -> Text
renderGraphViewWithDims dims mSel model =
  let units = Map.elems (bmUnits model)
      totalUnits = length units
      isActive u = case uiStatus u of
        UnitPlanned -> not (Map.null (uiModules u))
        _           -> True
      (activeUnits, inactiveUnits) = partition isActive units
      (activeLines, _) = foldl renderUnitWithCursor ([], 0) activeUnits
      foldedLine = if null inactiveUnits
                     then []
                     else [truncateToCols cols ("  ▸ " <> T.pack (show (length inactiveUnits)) <> " dependencies planned/waiting")]
      footer = truncateToCols cols ("Overall: " <> renderOverallBarWithWidth dynBarWidth model)
      helpLine = case mSel of
        Just idx -> [truncateToCols cols ("  [Keys: ↑/↓ select (row " <> T.pack (show idx) <> ") | g/m/z view | t warnings | x explain | 1-9 fits | e edit | q detach]")]
        Nothing  -> [truncateToCols cols "  [Keys: g/m/z view | t warnings | x explain | 1-9 fits | ↑/↓ select | q detach]"]
      header = truncateToCols cols ("=== buildr: Graph Frontier (Units: " <> T.pack (show totalUnits)
                                   <> " | Slots: " <> T.pack (show (bmBusySlots model)) <> "/"
                                   <> T.pack (show (bmTotalSlots model)) <> ") ===")
  in T.unlines
       ( [header]
       <> (if null activeLines then map (renderUnitSummaryWithWidth dynBarWidth cols) (take 10 units) else activeLines)
       <> foldedLine
       <> [footer]
       <> helpLine
       )
  where
    cols = tdCols dims
    dynBarWidth = calculateDynamicBarWidth cols
    renderUnitWithCursor (accLines, curIdx) u =
      let uHeader = renderUnitSummaryWithWidth dynBarWidth cols u
          mods = Map.elems (uiModules u)
          renderMod (mAcc, rIdx) m =
            let vState = statusToVisual (miStatus m)
                glyph = stateGlyph vState
                modRatio = case miStatus m of
                  ModDone _ _ -> 1.0
                  ModCompiling _ -> 0.5
                  _ -> 0.0
                modBar = renderBar (max 6 (dynBarWidth `div` 2)) modRatio
                infoStr = case miStatus m of
                  ModDone ms alloc -> formatDurationMs ms <> " (" <> formatBytes alloc <> ")"
                  ModCompiling _ ->
                    let phases = Map.keys (miPhases m)
                    in if null phases then "compiling" else T.intercalate "/" (take 2 phases)
                  ModSkipped reas -> reas
                  ModFailed errs -> "failed (" <> T.pack (show errs) <> " errors)"
                  ModBlocked blk -> "blocked by " <> blk
                  _ -> stateName vState
                cursor = if mSel == Just rIdx then "▶ " else "  "
                rowRaw = cursor <> "  " <> T.singleton glyph <> " " <> miName m <> " " <> modBar <> " " <> infoStr
                rowText = truncateToCols cols rowRaw
            in (rowText : mAcc, rIdx + 1)
          (modRowsRev, nextIdx) = foldl renderMod ([], curIdx) (take 10 mods)
      in (accLines <> [uHeader] <> reverse modRowsRev, nextIdx)

-- | Formats the default Graph Frontier view (§12.1).
renderGraphView :: BuildModel -> Text
renderGraphView = renderGraphViewSelected Nothing

-- | Renders a concise summary for a single unit with dynamic width.
renderUnitSummaryWithWidth :: Int -> Int -> UnitInfo -> Text
renderUnitSummaryWithWidth barWidth cols u =
  let totalMods = Map.size (uiModules u)
      doneMods = length (filter isModuleDone (Map.elems (uiModules u)))
      ratio = if totalMods > 0 then fromIntegral doneMods / fromIntegral totalMods else 0.0
      bar = renderBar barWidth ratio
      statusLabel = case uiStatus u of
        UnitPlanned       -> "planned"
        UnitBuilding _    -> "building"
        UnitCompleted c   -> "completed (" <> T.pack (show c) <> ")"
        UnitFailed c _    -> "failed (" <> T.pack (show c) <> ")"
        UnitStopped       -> "stopped"
      raw = "  " <> uiPackage u <> " " <> bar <> " "
         <> T.pack (show doneMods) <> "/" <> T.pack (show totalMods) <> " [" <> statusLabel <> "]"
  in truncateToCols cols raw

-- | Renders a concise summary for a single unit within the graph view.
renderUnitSummary :: UnitInfo -> Text
renderUnitSummary = renderUnitSummaryWithWidth defaultBarWidth 80

-- | Computes the overall progress bar with customized width.
renderOverallBarWithWidth :: Int -> BuildModel -> Text
renderOverallBarWithWidth barW model =
  let allMods = concatMap (Map.elems . uiModules) (Map.elems (bmUnits model))
      totalMods = length allMods
      doneMods = length (filter isModuleDone allMods)
      ratio = if totalMods > 0 then fromIntegral doneMods / fromIntegral totalMods else 0.0
  in renderBar barW ratio <> " " <> T.pack (show doneMods) <> "/" <> T.pack (show totalMods)

-- | Renders the single-line Zen view (§12.2).
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

-- | Renders the Mosaic view adapted to dynamic terminal dimensions (§12.3, §19, §20).
renderMosaicViewWithDims :: TerminalDimensions -> BuildModel -> Text
renderMosaicViewWithDims dims model =
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
      chunkSize = max 10 (tdCols dims - 6)
      chunkedLines = map T.pack (chunksOf chunkSize cells)
  in if null chunkedLines
       then "buildr mosaic: []"
       else T.unlines ("buildr mosaic:" : map (\l -> "  [" <> l <> "]") chunkedLines)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Renders the Mosaic view with default terminal dimensions (§12.3).
renderMosaicView :: BuildModel -> Text
renderMosaicView = renderMosaicViewWithDims defaultDimensions

-- | Renders the Plain / CI view suitable for non-interactive log files (§12.4).
renderPlainView :: BuildModel -> Text
renderPlainView model =
  let units = Map.elems (bmUnits model)
      renderUnitPlain u =
        let mods = Map.elems (uiModules u)
            modLines = map (\m -> "  " <> T.singleton (stateGlyph (statusToVisual (miStatus m)))
                                    <> " " <> miName m <> " ("
                                    <> stateName (statusToVisual (miStatus m)) <> ")") mods
        in "Unit " <> uiId u <> ":\n" <> T.unlines modLines
      diags = bmDiagnostics model
      diagSection = if null diags
                      then ""
                      else "\n--- Diagnostics ---\n" <> T.concat (map formatDiagnostic (reverse diags))
  in T.concat (map renderUnitPlain units) <> diagSection

-- | Renders the Plain view tailored for GitHub Actions with collapsible groups and annotations (§19).
renderPlainViewCi :: BuildModel -> Text
renderPlainViewCi model =
  let units = Map.elems (bmUnits model)
      renderUnitCi u =
        let mods = Map.elems (uiModules u)
            modLines = map (\m -> "  " <> T.singleton (stateGlyph (statusToVisual (miStatus m)))
                                    <> " " <> miName m <> " ("
                                    <> stateName (statusToVisual (miStatus m)) <> ")") mods
        in "::group::Unit " <> uiPackage u <> " (" <> uiId u <> ")\n"
             <> T.unlines modLines
             <> "::endgroup::\n"
      diags = bmDiagnostics model
      renderDiagCi d =
        let sp = diagSpan d
            file = spanFile sp
            line = spanStartLine sp
            col = spanStartCol sp
            cmd = if diagSeverity d == SeverityError then "::error " else "::warning "
            loc = "file=" <> T.pack file <> ",line=" <> T.pack (show line) <> ",col=" <> T.pack (show col) <> "::"
            msg = T.intercalate " " (diagMessage d)
        in cmd <> loc <> msg <> "\n"
      diagSection = if null diags
                      then ""
                      else T.concat (map renderDiagCi (reverse diags))
  in T.concat (map renderUnitCi units) <> diagSection

-- | Renders the Linear accessible view designed for screen readers (§12.5).
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

-- | Finds the module with the highest memory allocation, returning its name, allocation, and dominant phase.
computeHeaviestModule :: BuildModel -> Maybe (Text, Int64, Text)
computeHeaviestModule model =
  let allMods = concatMap (Map.elems . uiModules) (Map.elems (bmUnits model))
      doneMods = [ m | m <- allMods, isModuleDone m ]
      extractAlloc m =
        let phases = Map.toList (miPhases m)
            totalAlloc = case miStatus m of ModDone _ a -> a; _ -> 0
            domPhase = if null phases
                         then "compile"
                         else fst (maximumBy (comparing (snd . snd)) phases)
        in (miName m, totalAlloc, domPhase)
  in if null doneMods
       then Nothing
       else Just (maximumBy (comparing (\(_, a, _) -> a)) (map extractAlloc doneMods))

-- | Finds the module that took the longest time to compile.
computeSlowestModule :: BuildModel -> Maybe (Text, Int64)
computeSlowestModule model =
  let allMods = concatMap (Map.elems . uiModules) (Map.elems (bmUnits model))
      doneDurations = [ (miName m, d) | m <- allMods, ModDone d _ <- [miStatus m] ]
  in if null doneDurations
       then Nothing
       else Just (maximumBy (comparing snd) doneDurations)

-- | Approximates the critical path (longest chain by module count and dependencies).
computeCriticalPath :: BuildModel -> [Text]
computeCriticalPath model =
  let allMods = concatMap (Map.elems . uiModules) (Map.elems (bmUnits model))
      modMap = Map.fromList [ (miName m, m) | m <- allMods ]
      longestFrom name visited =
        if name `elem` visited
          then []
          else case Map.lookup name modMap of
                 Nothing -> [name]
                 Just _ ->
                   let deps = [ d | d <- Map.keys (Map.filter (\other -> name `elem` miDependencies other) modMap) ]
                   in if null deps
                        then [name]
                        else name : maximumBy (comparing length) (map (\d -> longestFrom d (name : visited)) deps)
      roots = [ miName m | m <- allMods, null (miDependencies m) ]
  in if null roots
       then map miName (take 5 allMods)
       else maximumBy (comparing length) (map (\r -> longestFrom r []) roots)

-- | Formats a human-readable duration in milliseconds to seconds / minutes.
formatDurationMs :: Int64 -> Text
formatDurationMs ms
  | ms < 1000 = T.pack (show ms) <> "ms"
  | otherwise =
      let secs = fromIntegral ms / 1000.0 :: Double
          rounded = (fromIntegral (round (secs * 10.0) :: Int) / 10.0) :: Double
      in T.pack (show rounded) <> "s"

-- | Formats a memory allocation in bytes to KB/MB/GB.
formatBytes :: Int64 -> Text
formatBytes b
  | b < 1024 = T.pack (show b) <> " B"
  | b < 1048576 = T.pack (show (b `div` 1024)) <> " KB"
  | otherwise = T.pack (show (b `div` 1048576)) <> " MB"

-- | Canonical Summary Block specified in §16.
-- Always printed at the end of a build run in all views except Zen.
renderSummaryBlock :: BuildModel -> Int64 -> Text
renderSummaryBlock model wallTimeNs =
  let wallMs = wallTimeNs `div` 1000000
      counts = countModulesByStatus model
      doneCount    = Map.findWithDefault 0 "done" counts
      skippedCount = Map.findWithDefault 0 "skipped" counts
      failedCount  = Map.findWithDefault 0 "failed" counts
      blockedCount = Map.findWithDefault 0 "blocked" counts
      notReached   = Map.findWithDefault 0 "not_reached" counts

      outcomeLabel = case bmOutcome model of
        Just BuildSuccess   -> "SUCCESS"
        Just BuildFailed    -> "FAILED"
        Just BuildCancelled -> "CANCELLED"
        Nothing -> if failedCount > 0 then "FAILED" else "SUCCESS"

      countsLine = "Modules: " <> T.pack (show doneCount) <> " done, "
                              <> T.pack (show skippedCount) <> " skipped, "
                              <> T.pack (show failedCount) <> " failed, "
                              <> T.pack (show blockedCount) <> " blocked, "
                              <> T.pack (show notReached) <> " not reached"

      heaviestInfo = case computeHeaviestModule model of
        Just (name, alloc, phase) ->
          "Heaviest module: " <> name <> " (" <> formatBytes alloc <> " in " <> phase <> ")"
        Nothing -> "Heaviest module: none recorded"

      slowestInfo = case computeSlowestModule model of
        Just (name, dur) ->
          "Slowest module:  " <> name <> " (" <> formatDurationMs dur <> ")"
        Nothing -> "Slowest module:  none recorded"

      critPath = computeCriticalPath model
      critPathLine = if null critPath
                       then ""
                       else "Critical path:   " <> T.intercalate " -> " (take 6 critPath)

      failedUnits = [ uiId u | u <- Map.elems (bmUnits model), any isModFailed (Map.elems (uiModules u)) ]
      isModFailed m = case miStatus m of
        ModFailed _ -> True
        _           -> False

      nextStep = if failedCount > 0
                   then if not (null failedUnits)
                          then "Next steps: Run 'buildr build " <> T.intercalate " " (take 3 failedUnits) <> " --keep-going' to rebuild failed targets."
                          else "Next steps: Run 'buildr build --keep-going' or check compiler diagnostics above."
                   else "Build completed cleanly."
  in T.unlines
       [ "================================================================================"
       , "=== buildr Outcome: " <> outcomeLabel <> " (Wall time: " <> formatDurationMs wallMs <> ") ==="
       , "================================================================================"
       , countsLine
       , heaviestInfo
       , slowestInfo
       , critPathLine
       , nextStep
       , "================================================================================"
       ]
