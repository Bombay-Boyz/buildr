-- |
-- Module      : Main
-- Description : Comprehensive property and boundary test suite for buildr.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Exhaustive property-based verification of:
--   * §10.1 Pure model state machine invariants (QuickCheck)
--   * §9 Event Protocol v1 serialization round-tripping
--   * §11 Visual Grammar 20-cell progress bar bounds and resolution
--   * §10.3 History cache median computation and overrun detection
--   * Boundary outliers (empty plan, single module, zero width, large sequences)
module Main (main) where

import Buildr.Core
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck

-- | Generator for valid Event Protocol v1 event sources.
genSource :: Gen EventSource
genSource = elements [SrcCabal, SrcGhc, SrcPlan, SrcGraph, SrcHistory, SrcBuildr]

-- | Generator for valid ValueKinds.
genValueKind :: Gen ValueKind
genValueKind = elements [ValueObserved, ValueReported, ValueDerived, ValueEstimate]

-- | Generator for EventPayload variants.
genPayload :: Gen EventPayload
genPayload = oneof
  [ PayloadBuildStarted <$> pure "0.1.0" <*> pure "3.16.1.0" <*> pure "9.10.3"
  , pure (PayloadPlanReady [PlanUnit "unit-1" "pkg-a" "1.0" "lib" StyleLocal [] Nothing])
  , pure (PayloadUnitStarted "unit-1")
  , pure (PayloadUnitCompleted "unit-1" 0)
  , pure (PayloadUnitFailed "unit-1" 1 (Just "cabal.log"))
  , pure (PayloadUnitStopped "unit-1")
  , pure (PayloadSlotsChanged 2 4)
  , pure (PayloadBuildDone BuildSuccess (BuildCounts 10 0 0 0 0) 5000000000)
  ]

-- | Generator for complete Event envelopes.
genEvent :: Gen Event
genEvent = do
  s <- arbitrary
  t <- arbitrary
  src <- genSource
  vk <- genValueKind
  payload <- genPayload
  pure (mkEvent (abs s) (abs t) src vk payload)

-- | Property 1: Event Protocol v1 JSON round-trip serialization.
prop_eventJsonRoundTrip :: Property
prop_eventJsonRoundTrip = forAll genEvent $ \ev ->
  decodeEventLine (encodeEventLine ev) == Just ev

-- | Property 2: Visual Grammar bar formatting strictly respects width bounds.
prop_barWidthRespectsBounds :: Property
prop_barWidthRespectsBounds = forAll (choose (1, 100)) $ \w ->
  forAll (choose (-0.5, 1.5)) $ \ratio ->
    let rendered = renderBar w ratio
    in T.length rendered == w + 2 && T.head rendered == '[' && T.last rendered == ']'

-- | Property 3: Model fold invariants hold across arbitrary event sequences.
prop_modelInvariantsPreserved :: Property
prop_modelInvariantsPreserved = forAll (listOf genEvent) $ \evs ->
  let finalModel = foldEvents initialModel evs
  in checkModelInvariants finalModel

-- | Property 4: Determinism — folding the same event sequence twice yields identical models.
prop_foldDeterminism :: Property
prop_foldDeterminism = forAll (listOf genEvent) $ \evs ->
  foldEvents initialModel evs == foldEvents initialModel evs

-- | Property 5: History cache median is sound and bounded by recorded minimum and maximum.
prop_historyMedianBounded :: [Positive Int64] -> Property
prop_historyMedianBounded rawList = not (null rawList) ==>
  let ds = map getPositive (take 10 rawList)
      key = HistoryKey "proj" "unit" "Mod" "9.10.3" "3.16.1.0" "-O1"
      cache = foldr (\d acc -> insertTiming key d 1000 acc) emptyHistoryCache ds
  in case computeUsualTime key cache of
       Nothing -> False
       Just med -> med >= minimum ds && med <= maximum ds

-- | Property 6: Overrun detection correctly triggers when observed > 1.5x usual (for realistic build timings up to 24 hours).
prop_overrunDetection :: Property
prop_overrunDetection = forAll (choose (10, 86400000 :: Int64)) $ \usual ->
  let normal = usual
      slow = (usual * 16) `div` 10
  in not (isOverrun normal usual) && isOverrun slow usual

-- | Boundary test cases.
runBoundaryTests :: IO Bool
runBoundaryTests = do
  putStrLn "Running boundary outlier tests..."
  let emptyBar = renderBar 0 0.5
      zeroBarOk = T.null emptyBar

      largeBar = renderBar 1000 0.75
      largeBarOk = T.length largeBar == 1002

      -- Empty build model invariant check
      emptyModelOk = checkModelInvariants initialModel

      -- Single unit and single module life cycle
      events =
        [ mkEvent 1 100 SrcPlan ValueReported
            (PayloadPlanReady [PlanUnit "u1" "lib-core" "0.1" "lib" StyleLocal [] Nothing])
        , mkEvent 2 200 SrcGraph ValueReported
            (PayloadGraphKnown "u1" [ModuleNode "Core.Main" "src/Main.hs" False] [])
        , mkEvent 3 300 SrcCabal ValueObserved
            (PayloadUnitStarted "u1")
        , mkEvent 4 400 SrcGhc ValueObserved
            (PayloadModuleStarted "u1" "Core.Main" 1 1 "src/Main.hs" "src/Main.o" Nothing)
        , mkEvent 5 500 SrcGhc ValueReported
            (PayloadModulePhase "u1" "Core.Main" "codegen" 150 2048000)
        , mkEvent 6 600 SrcGhc ValueReported
            (PayloadModuleFinished "u1" "Core.Main")
        , mkEvent 7 700 SrcCabal ValueObserved
            (PayloadUnitCompleted "u1" 0)
        , mkEvent 8 800 SrcBuildr ValueObserved
            (PayloadBuildDone BuildSuccess (BuildCounts 1 0 0 0 0) 800)
        ]
      builtModel = foldEvents initialModel events
      singleModDone = case Map.lookup "u1" (bmUnits builtModel) of
        Just u -> case Map.lookup "Core.Main" (uiModules u) of
          Just m  -> isModuleDone m && miHasReportedFinish m
          Nothing -> False
        Nothing -> False

      boundaryOk = zeroBarOk && largeBarOk && emptyModelOk && singleModDone
  if boundaryOk
    then do
      putStrLn "  Boundary outlier tests: PASS"
      pure True
    else do
      putStrLn "  Boundary outlier tests: FAIL"
      pure False

main :: IO ()
main = do
  putStrLn "=========================================================="
  putStrLn "  buildr Verification & Invariant Suite (QuickCheck)"
  putStrLn "=========================================================="
  bOk <- runBoundaryTests

  res1 <- quickCheckResult (withNumTests 100 prop_eventJsonRoundTrip)
  res2 <- quickCheckResult (withNumTests 200 prop_barWidthRespectsBounds)
  res3 <- quickCheckResult (withNumTests 200 prop_modelInvariantsPreserved)
  res4 <- quickCheckResult (withNumTests 100 prop_foldDeterminism)
  res5 <- quickCheckResult (withNumTests 100 prop_historyMedianBounded)
  res6 <- quickCheckResult (withNumTests 100 prop_overrunDetection)

  let allPassed = bOk && isSuccess res1 && isSuccess res2 && isSuccess res3
                      && isSuccess res4 && isSuccess res5 && isSuccess res6
  if allPassed
    then do
      putStrLn "\nAll QuickCheck properties and boundary invariants PASSED."
      exitSuccess
    else do
      putStrLn "\nVerification suite FAILED."
      exitFailure
