-- |
-- Module      : Main
-- Description : Comprehensive property, IPC, theme, and plan test suite for buildr.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Exhaustive property-based verification of:
--   * §10.1 Pure model state machine invariants (QuickCheck)
--   * §9 Event Protocol v1 serialization round-tripping
--   * §11 Visual Grammar 20-cell progress bar bounds and resolution
--   * §10.3 History cache median computation, overrun detection, and persistence
--   * §17 Theme contrast ratios against WCAG 2.1 (minimum 4.5:1)
--   * §16 Summary block consistency with pure model
--   * §8.2 Unix domain socket IPC streaming and client/server transport
--   * Cabal plan.json decoding and validation
module Main (main) where

import Buildr.Core
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
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

-- | Property 6: Overrun detection correctly triggers when observed > 1.5x usual.
prop_overrunDetection :: Property
prop_overrunDetection = forAll (choose (10, 86400000 :: Int64)) $ \usual ->
  let normal = usual
      slow = (usual * 16) `div` 10
  in not (isOverrun normal usual) && isOverrun slow usual

-- | Property 7: Rasika theme strictly satisfies WCAG 2.1 (minimum 4.5:1 contrast) per §17.
prop_rasikaThemeContrast :: Property
prop_rasikaThemeContrast = property $
  let checks = checkThemeContrast rasikaTheme
  in all (\(_, _, pass) -> pass) checks

-- | Property 8: Summary block always contains valid counts matching model status totals.
prop_summaryBlockCountsMatchModel :: Property
prop_summaryBlockCountsMatchModel = forAll (listOf genEvent) $ \evs ->
  let model = foldEvents initialModel evs
      summary = renderSummaryBlock model 1000000000
      counts = countModulesByStatus model
      doneCount = Map.findWithDefault 0 "done" counts
  in T.isInfixOf (T.pack (show doneCount) <> " done") summary

-- | Property 9: HistoryCache JSON serialization round-trips losslessly.
prop_historyCacheJsonRoundTrip :: [Positive Int64] -> Property
prop_historyCacheJsonRoundTrip rawList = not (null rawList) ==>
  let ds = map getPositive (take 10 rawList)
      key = HistoryKey "p" "u" "M" "9.10" "3.16" "-O1"
      cache = foldr (\d acc -> insertTiming key d 1024 acc) emptyHistoryCache ds
  in Aeson.decode (Aeson.encode cache) == Just cache

-- | Tests Plan JSON parser functionality on edge cases and live plan.json.
runPlanTests :: IO Bool
runPlanTests = do
  putStrLn "Running Cabal plan.json tests..."
  let emptyPlanResult = parsePlanJson "{\"install-plan\": []}"
      emptyOk = emptyPlanResult == Right []

      malformedResult = parsePlanJson "{ not valid json }"
      malformedOk = case malformedResult of
        Left (PlanJsonParseFailure _ _) -> True
        _                               -> False

  liveExists <- doesFileExist defaultPlanPath
  liveOk <- if liveExists
    then do
      res <- loadPlanJson defaultPlanPath
      case res of
        Right units -> do
          putStrLn ("  Loaded " <> show (length units) <> " units from " <> defaultPlanPath)
          pure (not (null units))
        Left err -> do
          putStrLn ("  Failed to load plan: " <> show err)
          pure False
    else pure True

  let allPlanOk = emptyOk && malformedOk && liveOk
  if allPlanOk
    then putStrLn "  Plan parser tests: PASS"
    else putStrLn "  Plan parser tests: FAIL"
  pure allPlanOk

-- | Tests Unix domain socket IPC streaming.
runIpcTests :: IO Bool
runIpcTests = do
  putStrLn "Running Unix domain socket IPC integration tests..."
  tmpDir <- getTemporaryDirectory
  let sockPath = tmpDir </> "buildr-test-ipc.sock"
  recvVar <- newEmptyMVar :: IO (MVar Event)

  let testEvent = mkEvent 42 123456789 SrcGhc ValueReported (PayloadModuleFinished "u1" "TestModule")
  res <- bracket (startIpcServer sockPath (putMVar recvVar)) cleanupServer $ \case
    Left err -> do
      putStrLn ("  Failed to bind server socket: " <> show err)
      pure False
    Right serverHandle -> do
      sendRes <- sendIpcEvent sockPath testEvent
      case sendRes of
        Left err -> do
          putStrLn ("  Failed to send IPC event: " <> show err)
          stopIpcServer serverHandle
          pure False
        Right () -> do
          receivedEvent <- takeMVar recvVar
          stopIpcServer serverHandle
          let match = receivedEvent == testEvent
          if match
            then putStrLn "  IPC transport test: PASS"
            else putStrLn "  IPC transport test: FAIL (Event mismatch)"
          pure match
  pure res
  where
    cleanupServer _ = pure ()

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

      -- CI plain rendering check (§12.4)
      ciOut = renderPlainViewCi builtModel
      ciGroupOk = T.isInfixOf "::group::Unit" ciOut && T.isInfixOf "::endgroup::" ciOut

      -- Stream recogniser tests (§22 G4, G5)
      t1 = parseTimingLine "Renamer/typechecker [Buildr.Model]: alloc=14856000 time=15.420"
      t1Ok = case t1 of
        Just (phase, ms, alloc) -> phase == "Renamer/typechecker" && ms == 15 && alloc == 14856000
        Nothing -> False

      c1 = parseCabalLifecycleLine "Building library for buildr-0.1.0.0..."
      c1Ok = case c1 of
        Just (PayloadUnitStarted p) -> p == "buildr-0.1.0.0"
        _ -> False

      c2 = parseCabalLifecycleLine "Completed text-2.1.3 (lib)"
      c2Ok = case c2 of
        Just (PayloadUnitCompleted p 0) -> p == "text-2.1.3"
        _ -> False

      boundaryOk = zeroBarOk && largeBarOk && emptyModelOk && singleModDone && ciGroupOk && t1Ok && c1Ok && c2Ok
  if boundaryOk
    then do
      putStrLn "  Boundary outlier tests: PASS"
      pure True
    else do
      putStrLn "  Boundary outlier tests: FAIL"
      pure False

-- | Replay log golden tests against stored fixtures (§21).
runGoldenReplayTests :: IO Bool
runGoldenReplayTests = do
  putStrLn "Running Golden Replay log tests..."
  let successLogPath = "fixtures/sample-success.jsonl"
      failureLogPath = "fixtures/sample-failure.jsonl"

  succExists <- doesFileExist successLogPath
  failExists <- doesFileExist failureLogPath

  if not (succExists && failExists)
    then do
      putStrLn "  Skipping golden tests: fixture files missing"
      pure True
    else do
      succBytes <- BS.readFile successLogPath
      let succEvents = mapMaybe decodeEventLine (BSC8.lines succBytes)
          succModel = foldEvents initialModel succEvents
          succZen = renderZenView succModel
          succLinear = renderLinearView succModel
          succInv = checkModelInvariants succModel
          succOk = T.isInfixOf "3/3" succZen && T.isInfixOf "100 percent" succLinear && succInv

      failBytes <- BS.readFile failureLogPath
      let failEvents = mapMaybe decodeEventLine (BSC8.lines failBytes)
          failModel = foldEvents initialModel failEvents
          failInv = checkModelInvariants failModel
          failOk = failInv

      let goldenOk = succOk && failOk
      if goldenOk
        then putStrLn "  Golden Replay log tests: PASS"
        else putStrLn "  Golden Replay log tests: FAIL"
      pure goldenOk

main :: IO ()
main = do
  putStrLn "=========================================================="
  putStrLn "  buildr Verification & Invariant Suite (QuickCheck)"
  putStrLn "=========================================================="
  bOk <- runBoundaryTests
  planOk <- runPlanTests
  ipcOk <- runIpcTests
  goldenOk <- runGoldenReplayTests

  res1 <- quickCheckResult (withNumTests 100 prop_eventJsonRoundTrip)
  res2 <- quickCheckResult (withNumTests 200 prop_barWidthRespectsBounds)
  res3 <- quickCheckResult (withNumTests 200 prop_modelInvariantsPreserved)
  res4 <- quickCheckResult (withNumTests 100 prop_foldDeterminism)
  res5 <- quickCheckResult (withNumTests 100 prop_historyMedianBounded)
  res6 <- quickCheckResult (withNumTests 100 prop_overrunDetection)
  res7 <- quickCheckResult (withNumTests 50 prop_rasikaThemeContrast)
  res8 <- quickCheckResult (withNumTests 100 prop_summaryBlockCountsMatchModel)
  res9 <- quickCheckResult (withNumTests 100 prop_historyCacheJsonRoundTrip)

  let allPassed = bOk && planOk && ipcOk && goldenOk && isSuccess res1 && isSuccess res2 && isSuccess res3
                      && isSuccess res4 && isSuccess res5 && isSuccess res6
                      && isSuccess res7 && isSuccess res8 && isSuccess res9
  if allPassed
    then do
      putStrLn "\nAll QuickCheck properties, IPC tests, and boundary invariants PASSED."
      exitSuccess
    else do
      putStrLn "\nVerification suite FAILED."
      exitFailure
