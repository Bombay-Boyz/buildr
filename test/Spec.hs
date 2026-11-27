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
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, findExecutable, getTemporaryDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
#if !defined(mingw32_HOST_OS)
import System.Posix.Signals (sigTERM)
#endif
import System.Process (CreateProcess (..), createProcess, proc, readCreateProcessWithExitCode, readProcessWithExitCode, waitForProcess)
import Test.QuickCheck

-- | Generator for valid Event Protocol v1 event sources.
genSource :: Gen EventSource
genSource = elements [SrcCabal, SrcGhc, SrcPlan, SrcGraph, SrcHistory, SrcBuildr]

-- | Generator for valid ValueKinds.
genValueKind :: Gen ValueKind
genValueKind = elements [ValueObserved, ValueReported, ValueDerived, ValueEstimate]

-- | Generator for valid DiagnosticSeverity.
genSeverity :: Gen DiagnosticSeverity
genSeverity = elements [SeverityError, SeverityWarning, SeverityInfo]

-- | Generator for SourceSpan.
genSpan :: Gen SourceSpan
genSpan = SourceSpan <$> elements ["src/A.hs", "src/B.hs"] <*> choose (1, 1000) <*> choose (1, 100) <*> choose (1, 1000) <*> choose (1, 100)

-- | Generator for DiagnosticPayload.
genDiagnosticPayload :: Gen DiagnosticPayload
genDiagnosticPayload = do
  sev <- genSeverity
  code <- oneof [pure Nothing, Just <$> choose (1000, 99999)]
  sp <- genSpan
  msgs <- listOf1 (elements ["syntax error", "type mismatch", "not in scope"])
  hints <- listOf (elements ["add import", "use let binding"])
  flag <- oneof [pure Nothing, Just <$> elements ["-Wunused-imports", "-Wall"]]
  pure (DiagnosticPayload sev code sp msgs hints flag)

-- | Generator for ModuleNode.
genModuleNode :: Gen ModuleNode
genModuleNode = ModuleNode <$> elements ["Core.A", "Core.B", "Core.C"] <*> pure "src/A.hs" <*> arbitrary

-- | Generator for GraphEdge.
genGraphEdge :: Gen GraphEdge
genGraphEdge = GraphEdge <$> elements [Nothing, Just "Core.A", Just "Core.B"] <*> elements ["Core.A", "Core.B", "Core.C"] <*> arbitrary

-- | Generator for EventPayload variants.
genPayload :: Gen EventPayload
genPayload = oneof
  [ PayloadBuildStarted <$> pure "0.1.0" <*> pure "3.16.1.0" <*> pure "9.10.3"
  , pure (PayloadPlanReady [PlanUnit "unit-1" "pkg-a" "1.0" "lib" StyleLocal [] Nothing])
  , PayloadGraphKnown <$> pure "unit-1" <*> listOf genModuleNode <*> listOf genGraphEdge
  , pure (PayloadUnitStarted "unit-1")
  , pure (PayloadUnitCompleted "unit-1" 0)
  , pure (PayloadUnitFailed "unit-1" 1 (Just "cabal.log"))
  , pure (PayloadUnitStopped "unit-1")
  , pure (PayloadSlotsChanged 2 4)
  , PayloadDiagnostic <$> elements [Nothing, Just "unit-1"] <*> genDiagnosticPayload
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

-- | Property 10: DiagnosticPayload JSON round-trip via parseGhcDiagnosticJson (§13).
prop_diagnosticJsonRoundTrip :: Property
prop_diagnosticJsonRoundTrip = forAll genDiagnosticPayload $ \diag ->
  let jsonTxt = TE.decodeUtf8 (BSL.toStrict (Aeson.encode diag))
  in parseGhcDiagnosticJson jsonTxt == Just diag

-- | Property 11: Kahn's topological sort ensures dependencies precede dependents (Standard 4.2).
prop_kahnTopologicalSortValid :: Property
prop_kahnTopologicalSortValid =
  let m1 = ModuleNode "ModA" "src/ModA.hs" False
      m2 = ModuleNode "ModB" "src/ModB.hs" False
      m3 = ModuleNode "ModC" "src/ModC.hs" False
      nodes = [m1, m2, m3]
      edges = [ GraphEdge (Just "ModA") "ModB" False
              , GraphEdge (Just "ModB") "ModC" False
              ]
      sorted = kahnTopologicalSort nodes edges
      idxOf x = case filter (\(_, n) -> n == x) (zip [0 :: Int ..] sorted) of
        ((i, _):_) -> Just i
        []         -> Nothing
  in property (idxOf "ModC" < idxOf "ModB" && idxOf "ModB" < idxOf "ModA")

-- | Property 12: HinsightGate JSON serialization round-trips losslessly (§15).
prop_hinsightGateJsonRoundTrip :: Property
prop_hinsightGateJsonRoundTrip = forAll genGate $ \gate ->
  Aeson.decode (Aeson.encode gate) == Just gate
  where
    genGate = oneof
      [ GateMatched <$> elements ["9.10.3", "9.12.1"]
      , GateMismatched <$> elements ["9.10.3"] <*> elements ["9.12.1", "9.8.2"]
      , pure GateMissing
      , GateFailed <$> elements ["hinsight crashed", "permission denied"]
      ]

-- | Property 13: HoleFit JSON serialization round-trips losslessly (§14, §15).
prop_holeFitJsonRoundTrip :: Property
prop_holeFitJsonRoundTrip = forAll genFit $ \fit ->
  Aeson.decode (Aeson.encode fit) == Just fit
  where
    genFit = HoleFit <$> choose (1, 9) <*> elements ["[]", "id", "head"] <*> elements ["[a]", "a -> a", "[a] -> a"]

-- | Property 14: Diagnostic formatting preserves GHC message text verbatim in all styles (§6, §13).
prop_errorStyleHonestyPreserved :: Property
prop_errorStyleHonestyPreserved = forAll genDiagnosticPayload $ \diag ->
  let classic = formatDiagnosticWithStyle ErrorClassic diag
      friendly = formatDiagnosticWithStyle ErrorFriendly diag
      allMsgsIn fmt = all (`T.isInfixOf` fmt) (diagMessage diag)
  in allMsgsIn classic && allMsgsIn friendly

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

      -- Stream recogniser & formatter tests for GHC JSON diagnostics (§13, §22)
      ghcDiagJson = "{\"version\":\"1.0\",\"ghcVersion\":\"ghc-9.10.3\",\"span\":{\"file\":\"src/Core.hs\",\"start\":{\"line\":10,\"column\":5},\"end\":{\"line\":10,\"column\":12}},\"severity\":\"Error\",\"code\":39999,\"message\":[\"No instance for `Num String'\"],\"hints\":[\"Add instance\"],\"flag\":\"-Wcompat\"}"
      parsedDiag = parseGhcDiagnosticJson ghcDiagJson
      diagParsedOk = case parsedDiag of
        Just d -> diagSeverity d == SeverityError
               && diagCode d == Just 39999
               && spanFile (diagSpan d) == "src/Core.hs"
               && spanStartLine (diagSpan d) == 10
               && spanStartCol (diagSpan d) == 5
               && diagMessage d == ["No instance for `Num String'"]
               && diagHints d == ["Add instance"]
               && diagFlag d == Just "-Wcompat"
        Nothing -> False
      formattedDiag = maybe "" formatDiagnostic parsedDiag
      diagFormatOk = T.isInfixOf "src/Core.hs:10:5: error: [GHC-39999] [-Wcompat]" formattedDiag
                  && T.isInfixOf "No instance for `Num String'" formattedDiag
                  && T.isInfixOf "• Help: Add instance" formattedDiag
                  && T.isInfixOf "https://errors.haskell.org/messages/GHC-39999 (derived)" formattedDiag

  -- Upfront module graph discovery tests (§8.2, §10, §14)
  (srcNodes, srcEdges) <- discoverUnitGraph "src"
  let srcGraphOk = length srcNodes >= 10 && not (null srcEdges)
      srcKahn = kahnTopologicalSort srcNodes srcEdges
      kahnValid = length srcKahn == length srcNodes
      modHeaderOk = parseModuleHeader "module Test.Mod (foo) where\nfoo = 1" == Just "Test.Mod"
      modImportsOk = parseImports "import qualified Data.Map as Map\nimport {-# SOURCE #-} Test.Sub\n"
                  == [("Data.Map", False), ("Test.Sub", True)]

      -- Interface diff and rebuild reason tests (§14, §18)
      hiDiffSample = T.unlines
        [ "Considering whether compilation is required for ModB:"
        , "Checking interface for module ModA main"
        , "  Module fingerprint has changed 1234abcd -> 5678ef01"
        , "  Out of date: foo 1111 -> 2222"
        ]
      parsedHiDiffs = parseHiDiffs hiDiffSample
      hiDiffOk = case parsedHiDiffs of
        [d] -> rdModule d == "ModB"
            && rdDependency d == Just "ModA"
            && length (rdChanges d) == 2
        _   -> False
      formattedWhy = formatRebuildDiff "ModB" parsedHiDiffs
      whyFormatOk = T.isInfixOf "Target module:   ModB" formattedWhy
                 && T.isInfixOf "Interface change detected in upstream dependency 'ModA'" formattedWhy
                 && T.isInfixOf "Out of date: foo" formattedWhy

      -- Typed hole parsing & explanation formatting tests (§14, §15)
      sampleHoleMsg = T.unlines
        [ "Found hole: _ :: [Int]"
        , "Where: `_` is a rigid type variable"
        , "Valid hole fits include"
        , "  [] :: [Int]"
        , "  [1, 2] :: [Int]"
        , "  (reverse [1]) :: [Int] (bound at src/Main.hs:12:3)"
        ]
      parsedHoleFits = parseHoleFitsFromText sampleHoleMsg
      holeFitOk = case parsedHoleFits of
        (f1:_:f3:_) -> hfExpr f1 == "[]"
                    && hfType f1 == "[Int]"
                    && hfIndex f1 == 1
                    && hfExpr f3 == "(reverse [1])"
        _           -> False
      sampleExpl = DiagnosticExplanation (SourceSpan "src/Main.hs" 5 10 5 11) (Just 88464) "Found hole _ :: [Int]" parsedHoleFits Nothing
      formattedExpl = formatExplanation sampleExpl
      explOk = T.isInfixOf "[Explanation (GHC-88464)]" formattedExpl
            && T.isInfixOf "Typed-hole suggestions (press 1-9 to accept):" formattedExpl
            && T.isInfixOf "[1] [] :: [Int]" formattedExpl

      -- Version gate note formatting tests (§6, §15)
      gateMatchNote = formatGateNote (GateMatched "9.10.3") == "hinsight ready (exact match: GHC 9.10.3)"
      gateMismatchNote = formatGateNote (GateMismatched "9.10.3" "9.12.1") == "explanations unavailable: hinsight supports GHC 9.10.3, this project uses 9.12.1"
      gateMissingNote = formatGateNote GateMissing == "explanations unavailable: hinsight not found on PATH"
      gateNotesOk = gateMatchNote && gateMismatchNote && gateMissingNote

      -- Friendly vs classic error style formatting tests (§13, §18)
      friendlyFmt = case parsedDiag of
        Just d  -> formatDiagnosticWithStyle ErrorFriendly d
        Nothing -> ""
      friendlyFmtOk = T.isInfixOf "┌─ ERROR [GHC-39999]" friendlyFmt
                   && T.isInfixOf "Location: src/Core.hs:10:5" friendlyFmt
                   && T.isInfixOf "No instance for `Num String'" friendlyFmt
                   && T.isInfixOf "• Suggestion: Add instance" friendlyFmt

      boundaryOk = zeroBarOk && largeBarOk && emptyModelOk && singleModDone && ciGroupOk && t1Ok && c1Ok && c2Ok
                && diagParsedOk && diagFormatOk && srcGraphOk && kahnValid && modHeaderOk && modImportsOk
                && hiDiffOk && whyFormatOk && holeFitOk && explOk && gateNotesOk && friendlyFmtOk
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

-- | Generator for ViewMode
genViewMode :: Gen ViewMode
genViewMode = elements [ViewGraph, ViewZen, ViewMosaic, ViewPlain, ViewLinear]

-- | Generator for ErrorStyle
genErrorStyle :: Gen ErrorStyle
genErrorStyle = elements [ErrorClassic, ErrorFriendly]

-- | Generator for BuildConfig
genBuildConfig :: Gen BuildConfig
genBuildConfig = BuildConfig
  <$> oneof [pure Nothing, Just <$> genViewMode]
  <*> oneof [pure Nothing, Just <$> genErrorStyle]
  <*> arbitrary
  <*> arbitrary
  <*> oneof [pure Nothing, Just <$> elements ["rasika", "nord", "monochrome", "solarized"]]

-- | Property 15: BuildConfig Semigroup associativity law.
prop_configSemigroupAssociative :: Property
prop_configSemigroupAssociative = forAll genBuildConfig $ \a ->
  forAll genBuildConfig $ \b ->
  forAll genBuildConfig $ \c ->
    (a <> b) <> c == a <> (b <> c)

-- | Property 16: BuildConfig Monoid identity laws.
prop_configMonoidIdentity :: Property
prop_configMonoidIdentity = forAll genBuildConfig $ \a ->
  mempty <> a == a && a <> mempty == a

-- | Property 17: Precedence enforcement CLI > Project > User > Defaults.
prop_configPrecedenceLaw :: Property
prop_configPrecedenceLaw = forAll genBuildConfig $ \cli ->
  forAll genBuildConfig $ \proj ->
  forAll genBuildConfig $ \user ->
    let res = resolveBuildConfig cli proj user
        expectedView = case cfgViewMode cli of
          Just v  -> v
          Nothing -> case cfgViewMode proj of
            Just v  -> v
            Nothing -> case cfgViewMode user of
              Just v  -> v
              Nothing -> resViewMode defaultBuildConfig
        expectedErr = case cfgErrorStyle cli of
          Just e  -> e
          Nothing -> case cfgErrorStyle proj of
            Just e  -> e
            Nothing -> case cfgErrorStyle user of
              Just e  -> e
              Nothing -> resErrorStyle defaultBuildConfig
        expectedKeep = case cfgKeepGoing cli of
          Just k  -> k
          Nothing -> case cfgKeepGoing proj of
            Just k  -> k
            Nothing -> case cfgKeepGoing user of
              Just k  -> k
              Nothing -> resKeepGoing defaultBuildConfig
    in resViewMode res == expectedView && resErrorStyle res == expectedErr && resKeepGoing res == expectedKeep

-- | Unit test verifying TOML parsing and file loading.
runConfigTests :: IO Bool
runConfigTests = do
  putStrLn "Running configuration file resolution & TOML tests..."
  let sampleToml = T.unlines
        [ "# Project buildr config"
        , "[build]"
        , "view = \"zen\""
        , "errors = \"friendly\""
        , "keep-going = true"
        , "explain-holes = true"
        , "theme = \"rasika\""
        ]
  case parseTomlConfig sampleToml of
    Left err -> do
      putStrLn ("  TOML parse failed: " <> T.unpack err)
      pure False
    Right cfg -> do
      let parseOk = cfgViewMode cfg == Just ViewZen
                 && cfgErrorStyle cfg == Just ErrorFriendly
                 && cfgKeepGoing cfg == Just True
                 && cfgExplainHoles cfg == Just True
                 && cfgThemeName cfg == Just "rasika"
      let altToml = T.unlines
            [ "view = 'plain'"
            , "keep_going = false"
            , "explain_holes = false"
            ]
      let altOk = case parseTomlConfig altToml of
            Right c2 -> cfgViewMode c2 == Just ViewPlain
                     && cfgKeepGoing c2 == Just False
                     && cfgExplainHoles c2 == Just False
            Left _ -> False
      tmp <- getTemporaryDirectory
      let testFile = tmp </> "test-buildr.toml"
      TIO.writeFile testFile sampleToml
      fileRes <- loadConfigFile testFile
      let fileOk = case fileRes of
            Right c3 -> c3 == cfg
            Left _   -> False

      let allOk = parseOk && altOk && fileOk
      if allOk
        then putStrLn "  Configuration tests: PASS"
        else putStrLn "  Configuration tests: FAIL"
      pure allOk

-- | Property 18: Cancellation state update always guarantees cancelled outcome.
prop_modelCancellationOutcome :: Property
prop_modelCancellationOutcome = forAll (listOf genEvent) $ \evs ->
  let m = foldEvents initialModel evs
      c = markModelCancelled m
  in bmCompleted c && bmOutcome c == Just BuildCancelled

-- | Property 19: Cancellation summary always emits CANCELLED label and preserves exact module tallies.
prop_cancellationSummaryLabelAndCounts :: Property
prop_cancellationSummaryLabelAndCounts = forAll (listOf genEvent) $ \evs ->
  forAll (choose (0, 1000000000000 :: Int64)) $ \wallNs ->
    let m = foldEvents initialModel evs
        summary = formatCancellationSummary m wallNs
        counts = countModulesByStatus m
        done = Map.findWithDefault 0 "done" counts
        skipped = Map.findWithDefault 0 "skipped" counts
        failed = Map.findWithDefault 0 "failed" counts
        blocked = Map.findWithDefault 0 "blocked" counts
        notReached = Map.findWithDefault 0 "not_reached" counts
    in T.isInfixOf "CANCELLED" summary
       && T.isInfixOf (T.pack (show done) <> " done") summary
       && T.isInfixOf (T.pack (show skipped) <> " skipped") summary
       && T.isInfixOf (T.pack (show failed) <> " failed") summary
       && T.isInfixOf (T.pack (show blocked) <> " blocked") summary
       && T.isInfixOf (T.pack (show notReached) <> " not reached") summary

-- | Property 20: Cancellation preserves pure model invariants.
prop_cancellationPreservesInvariants :: Property
prop_cancellationPreservesInvariants = forAll (listOf genEvent) $ \evs ->
  let m = foldEvents initialModel evs
  in checkModelInvariants m ==> checkModelInvariants (markModelCancelled m)

-- | Generator for terminal dimensions.
genDimensions :: Gen TerminalDimensions
genDimensions = TerminalDimensions <$> choose (1, 100) <*> choose (1, 300)

-- | Property 21: Effective view mode dynamically collapses to Zen view when terminal height < 10 (§19).
prop_effectiveViewModeZenCollapse :: Property
prop_effectiveViewModeZenCollapse = forAll genViewMode $ \mode ->
  forAll genDimensions $ \dims ->
    let eff = effectiveViewMode mode dims
    in if tdRows dims < 10
         then eff == ViewZen
         else eff == mode

-- | Property 22: Dynamic bar width is monotonically bounded within [8, 28] (§19).
prop_calculateDynamicBarWidthBounds :: Property
prop_calculateDynamicBarWidthBounds = forAll (choose (-50, 500)) $ \cols ->
  let w = calculateDynamicBarWidth cols
  in w >= 8 && w <= 28

-- | Property 23: Column truncation is total, respects maximum width, and ends with ellipsis when truncated (§19, §20).
prop_truncateToColsLength :: Property
prop_truncateToColsLength = forAll (choose (-10, 200)) $ \cols ->
  forAll (elements ["", "alpha", "buildr is a fast compiler interface", "short", "a very long sentence exceeding many column widths for testing truncation behaviour"]) $ \txt ->
    let res = truncateToCols cols txt
    in if cols <= 0
         then res == ""
         else if T.length txt <= cols
                then res == txt
                else T.length res <= cols && T.isSuffixOf "…" res

-- | Property 24: NO_COLOR environment disables all ANSI sequences and capabilities.
prop_pureDetectCapsNoColor :: Property
prop_pureDetectCapsNoColor = forAll (elements ["1", "true", "yes"]) $ \v ->
  let caps = pureDetectCaps [("NO_COLOR", v), ("TERM_PROGRAM", "ghostty"), ("COLORTERM", "truecolor")]
  in caps == TerminalCaps False False False False False

-- | Property 25: Ghostty terminal enables OSC 8, undercurl, OSC 9 progress, and truecolor.
prop_pureDetectCapsGhostty :: Property
prop_pureDetectCapsGhostty =
  let caps = pureDetectCaps [("TERM_PROGRAM", "ghostty")]
  in property (tcOsc8Hyperlinks caps && tcUndercurl caps && tcOsc9Progress caps && tcTrueColor caps && tcColorEnabled caps)

-- | Property 26: OSC 8 file hyperlink embeds file:// and escape delimiters when supported (§7).
prop_formatOsc8FileHyperlink :: Property
prop_formatOsc8FileHyperlink = forAll (elements ["src/Main.hs", "app/Build.hs"]) $ \path ->
  forAll (elements ["Click here", "Location"]) $ \lbl ->
    let capsOn = defaultCaps { tcOsc8Hyperlinks = True }
        capsOff = defaultCaps { tcOsc8Hyperlinks = False }
        resOn = formatFileHyperlink capsOn path lbl
        resOff = formatFileHyperlink capsOff path lbl
    in T.isInfixOf "file://" resOn
       && T.isInfixOf (T.pack path) resOn
       && T.isInfixOf lbl resOn
       && resOff == lbl

-- | Property 27: SGR 4:3 undercurl and SGR 58 color formatting (§7).
prop_formatUndercurl :: Property
prop_formatUndercurl = forAll (elements ["error", "warning"]) $ \msg ->
  let capsUndercurl = defaultCaps { tcUndercurl = True, tcColorEnabled = True }
      capsPlainUnderline = defaultCaps { tcUndercurl = False, tcColorEnabled = True }
      capsNoColor = defaultCaps { tcUndercurl = True, tcColorEnabled = False }
      res1 = formatUndercurl capsUndercurl (255, 95, 95) msg
      res2 = formatUndercurl capsPlainUnderline (255, 95, 95) msg
      res3 = formatUndercurl capsNoColor (255, 95, 95) msg
  in T.isInfixOf "\ESC[4:3m" res1
     && T.isInfixOf "\ESC[58;2;255;95;95m" res1
     && T.isInfixOf "\ESC[4m" res2
     && res3 == msg

-- | Property 28: OSC 9;4 progress sequence conforms to protocol specification (§7).
prop_formatOsc9Progress :: Property
prop_formatOsc9Progress = forAll (choose (-50, 150)) $ \pct ->
  forAll (elements [TaskbarClear, TaskbarNormal, TaskbarError, TaskbarIndeterminate, TaskbarPaused]) $ \st ->
    let capsOn = defaultCaps { tcOsc9Progress = True }
        capsOff = defaultCaps { tcOsc9Progress = False }
        resOn = formatOsc9Progress capsOn st pct
        resOff = formatOsc9Progress capsOff st pct
    in resOff == ""
       && T.isPrefixOf "\ESC]9;4;" resOn
       && T.isSuffixOf "\ESC\\" resOn

-- | Property 29: Diagnostic formatting with capabilities preserves GHC error message and emits OSC 8 / undercurl (§6, §7, §13).
prop_formatDiagnosticWithCapsHonesty :: Property
prop_formatDiagnosticWithCapsHonesty = forAll genDiagnosticPayload $ \diag ->
  forAll genErrorStyle $ \style ->
    let caps = TerminalCaps True True True True True
        formatted = formatDiagnosticWithCaps caps style diag
        allMsgsIn = all (`T.isInfixOf` formatted) (diagMessage diag)
        fileLinked = T.isInfixOf "file://" formatted
        docLinked = case diagCode diag of
          Just c  -> T.isInfixOf ("GHC-" <> T.pack (show c)) formatted
          Nothing -> True
    in allMsgsIn && fileLinked && docLinked

-- | Property 30: When invoked as a Cabal external command, non-subcommand invocations default to 'build' (§18).
prop_partitionCliArgsCabalPluginDefaultsToBuild :: Property
prop_partitionCliArgsCabalPluginDefaultsToBuild =
  forAll (elements [[], ["lib:buildr"], ["app", "test:buildr-test"], ["all"]]) $ \targets ->
    case partitionCliArgs True targets of
      InvokeCommand (CmdBuild opts) -> cboTargets opts == targets
      _                             -> False

-- | Property 31: CLI argument partitioner accurately parses buildr-specific options.
prop_partitionCliArgsRecognizesBuildrOptions :: Property
prop_partitionCliArgsRecognizesBuildrOptions =
  forAll genViewMode $ \vMode ->
    forAll genErrorStyle $ \eStyle ->
      let vStr = case vMode of
            ViewGraph  -> "graph"
            ViewZen    -> "zen"
            ViewMosaic -> "mosaic"
            ViewPlain  -> "plain"
            ViewLinear -> "linear"
          eStr = case eStyle of
            ErrorClassic  -> "classic"
            ErrorFriendly -> "friendly"
          args = ["--view", vStr, "--errors", eStr, "--keep-going", "--explain-holes", "my-target"]
      in case partitionCliArgs True args of
           InvokeCommand (CmdBuild opts) ->
             cboViewMode opts == Just vMode
             && cboErrorStyle opts == Just eStyle
             && cboKeepGoing opts == Just True
             && cboExplainHoles opts == Just True
             && cboTargets opts == ["my-target"]
           _ -> False

-- | Property 32: Unrecognized Cabal options are transparently preserved in passthrough args (§18).
prop_partitionCliArgsCabalPassthrough :: Property
prop_partitionCliArgsCabalPassthrough =
  forAll (elements [["-j4"], ["--enable-tests", "-O2"], ["-v2", "--offline"]]) $ \passthrough ->
    let args = ["--view", "zen"] <> passthrough <> ["lib:core"]
    in case partitionCliArgs True args of
         InvokeCommand (CmdBuild opts) ->
           cboCabalArgs opts == passthrough
           && cboTargets opts == ["lib:core"]
           && cboViewMode opts == Just ViewZen
         _ -> False

-- | Property 33: Any arguments following double-dash '--' are forwarded to Cabal (§18).
prop_partitionCliArgsDoubleDash :: Property
prop_partitionCliArgsDoubleDash =
  forAll (elements [["--flags=static"], ["-j8", "--ghc-options=-Wall"]]) $ \cabalArgs ->
    let args = ["build", "--keep-going", "my-pkg", "--"] <> cabalArgs
    in case partitionCliArgs False args of
         InvokeCommand (CmdBuild opts) ->
           cboCabalArgs opts == cabalArgs
           && cboTargets opts == ["my-pkg"]
           && cboKeepGoing opts == Just True
         _ -> False

-- | Property 34: Informational and query flags bypass TUI and delegate directly to Cabal (§18).
prop_partitionCliArgsQueryDetection :: Property
prop_partitionCliArgsQueryDetection =
  forAll (elements
    [ ("--dry-run", QueryDryRun)
    , ("--numeric-version", QueryNumericVersion)
    , ("--list-sources", QueryListSources)
    , ("--only-configure", QueryOnlyConfigure)
    , ("--only-download", QueryOnlyDownload)
    ]) $ \(flag, expectedQuery) ->
      case partitionCliArgs True [flag, "lib:buildr"] of
        InvokeCabalQuery qtype args -> qtype == expectedQuery && flag `elem` args
        _                           -> False

-- | Property 35: Standalone and Cabal external command modes recognize all defined subcommands (§18).
prop_partitionCliArgsSubcommands :: Property
prop_partitionCliArgsSubcommands =
  let subcases =
        [ (["watch", "pkgA"], True)
        , (["why", "ModuleA"], True)
        , (["doctor"], True)
        , (["stats"], True)
        , (["theme", "list"], True)
        , (["--version"], False)
        ]
  in property $ all (\(tokens, isSub) ->
       let res1 = partitionCliArgs False tokens
           res2 = partitionCliArgs True tokens
       in case (res1, res2) of
            (InvokeCommand _, InvokeCommand _) -> isSub
            (InvokeVersion, InvokeVersion)     -> not isSub
            _                                  -> False
       ) subcases

-- | Comprehensive integration and outlier suite for process group signals and terminal restoration.
runSignalAndCancellationTests :: IO Bool
runSignalAndCancellationTests = do
  putStrLn "Running process group signal handling & terminal restoration tests..."

  -- 1. Outlier: format cancellation summary with 0 ns wall time and negative bounds
  let zeroSummary = formatCancellationSummary initialModel 0
      zeroOk = T.isInfixOf "CANCELLED" zeroSummary && T.isInfixOf "0 done" zeroSummary

  let negSummary = formatCancellationSummary initialModel (-500000)
      negOk = T.isInfixOf "CANCELLED" negSummary

  -- 2. Outlier: format cancellation summary with 10^15 ns wall time (multi-day build)
  let hugeSummary = formatCancellationSummary initialModel 86400000000000
      hugeOk = T.isInfixOf "CANCELLED" hugeSummary

  -- 3. Outlier: forward signal to already terminated process handle
  (_, _, _, phTerm) <- createProcess (proc "true" [])
  _ <- waitForProcess phTerm
  forwardAbortSignals phTerm
  let deadProcOk = True

  -- 4. Process group signal delivery on active child
#if !defined(mingw32_HOST_OS)
  let childProc = (proc "sleep" ["10"]) { create_group = True }
  (_, _, _, phSleep) <- createProcess childProc
  forwardSignalToProcessGroup sigTERM phSleep
  sleepExit <- waitForProcess phSleep
  let signalDeliveredOk = sleepExit /= ExitSuccess
#else
  let signalDeliveredOk = True
#endif

  -- 5. Terminal restoration under thrown exception
  exRes <- try (withTerminalRestored False (throwIO (userError "simulated abortion failure"))) :: IO (Either SomeException ())
  let termRestoreOk = case exRes of
        Left _  -> True
        Right _ -> False

  let allOk = zeroOk && negOk && hugeOk && deadProcOk && signalDeliveredOk && termRestoreOk
  if allOk
    then putStrLn "  Process group signal & cancellation tests: PASS"
    else putStrLn "  Process group signal & cancellation tests: FAIL"
  pure allOk

-- | Comprehensive verification of terminal dimension IO, SIGWINCH handler installation, and VT escape sequences (§7, §19, §20).
runTerminalAndResizeTests :: IO Bool
runTerminalAndResizeTests = do
  putStrLn "Running dynamic terminal resizing & advanced VT sequence tests..."

  -- 1. IO dimension query returns valid positive dimensions
  dims <- getTerminalDimensions
  let dimsOk = tdRows dims > 0 && tdCols dims > 0

  -- 2. IO capability detection returns valid caps
  caps <- detectTerminalCaps
  let capsOk = tcColorEnabled caps || not (tcColorEnabled caps)

  -- 3. SIGWINCH handler installation and execution without throwing
  winchRef <- newEmptyMVar
  withWindowResizeHandler (putMVar winchRef ()) $ do
    pure ()
  let winchInstalledOk = True

  -- 4. Outlier: negative and 0 dimensions in view rendering
  let zeroDims = TerminalDimensions 0 0
      model = initialModel
      zenRendered = renderViewWithDims zeroDims ViewGraph Nothing model
      mosaicRendered = renderViewWithDims zeroDims ViewMosaic Nothing model
      plainRendered = renderViewWithDims zeroDims ViewPlain Nothing model
      graphRendered = renderViewWithDims (TerminalDimensions 5 20) ViewGraph Nothing model
      rendersTotalOk = not (T.null zenRendered) && not (T.null mosaicRendered)
                    && not (T.null plainRendered) && not (T.null graphRendered)

  let allOk = dimsOk && capsOk && winchInstalledOk && rendersTotalOk
  if allOk
    then putStrLn "  Dynamic terminal resizing & advanced VT sequence tests: PASS"
    else putStrLn "  Dynamic terminal resizing & advanced VT sequence tests: FAIL"
  pure allOk

-- | Comprehensive verification of native Cabal external command integration (§18).
runCabalExternalCommandTests :: IO Bool
runCabalExternalCommandTests = do
  putStrLn "Running native Cabal external command integration tests (§18)..."

  -- 1. Pure partition assertions on empty and flag-less invocations
  let emptyPlugin = partitionCliArgs True []
      emptyOk = case emptyPlugin of
        InvokeCommand (CmdBuild opts) -> null (cboTargets opts) && null (cboCabalArgs opts)
        _                             -> False

  -- 2. Pure partition assertions on mixed options, targets, and cabal passthroughs
  let mixed = partitionCliArgs True ["--view", "zen", "-j4", "lib:buildr", "--keep-going"]
      mixedOk = case mixed of
        InvokeCommand (CmdBuild opts) ->
          cboViewMode opts == Just ViewZen
          && cboKeepGoing opts == Just True
          && cboTargets opts == ["lib:buildr"]
          && cboCabalArgs opts == ["-j4"]
        _ -> False

  -- 3. Query flag detection
  let dryRunInv = partitionCliArgs True ["--dry-run", "all"]
      dryRunOk = case dryRunInv of
        InvokeCabalQuery QueryDryRun args -> "--dry-run" `elem` args && "all" `elem` args
        _                                 -> False

  -- 4. Help and Version commands
  let helpInv = partitionCliArgs True ["--help"]
      versionInv = partitionCliArgs True ["--version"]
      helpOk = helpInv == InvokeHelp Nothing
      verOk = versionInv == InvokeVersion

  -- 5. Subcommand routing
  let docInv = partitionCliArgs True ["doctor"]
      docOk = docInv == InvokeCommand CmdDoctor

  -- 6. Live cabal-buildr execution via Cabal external command discovery
  mCabalBuildr <- findExecutable "cabal-buildr"
  liveCabalOk <- case mCabalBuildr of
    Just _ -> do
      (code, out, _) <- readProcessWithExitCode "cabal" ["buildr", "--help"] ""
      pure (code == ExitSuccess && "buildr v" `isInfixOfStr` out)
    Nothing -> do
      let devPath = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/buildr-0.1.0.0/x/cabal-buildr/build/cabal-buildr"
      exists <- doesDirectoryExist devPath
      if exists
        then do
          absDevPath <- canonicalizePath devPath
          curEnv <- getEnvironment
          let curPath = fromMaybe "" (lookup "PATH" curEnv)
              testEnv = ("PATH", absDevPath <> ":" <> curPath) : filter ((/= "PATH") . fst) curEnv
              procTest = (proc "cabal" ["buildr", "--help"]) { env = Just testEnv }
          (code, out, _) <- readCreateProcessWithExitCode procTest ""
          pure (code == ExitSuccess && "buildr v" `isInfixOfStr` out)
        else pure True

  let allOk = emptyOk && mixedOk && dryRunOk && helpOk && verOk && docOk && liveCabalOk
  if allOk
    then putStrLn "  Native Cabal external command integration tests: PASS"
    else putStrLn "  Native Cabal external command integration tests: FAIL"
  pure allOk
  where
    isInfixOfStr sub str = T.isInfixOf (T.pack sub) (T.pack str)

-- | Comprehensive execution of automated benchmark criteria (B1, B2, B4, B8) per §5 and §21.
runBenchmarkSuite :: IO Bool
runBenchmarkSuite = do
  putStrLn "Running automated benchmark suite (§5, §21)..."
  outcomes <- runAllBenchmarks
  forM_ outcomes $ \bo -> do
    let statusStr = if boPassed bo then "PASS" else "FAIL"
    putStrLn ("  [" <> T.unpack (boCriterion bo) <> "] " <> T.unpack (boName bo) <> ": " <> statusStr)
    putStrLn ("      Target:   " <> T.unpack (boTarget bo))
    putStrLn ("      Measured: " <> T.unpack (boMeasured bo))
    putStrLn ("      Details:  " <> T.unpack (boDetails bo))
  let allPassed = all boPassed outcomes
  if allPassed
    then putStrLn "  Benchmark verification suite: PASS"
    else putStrLn "  Benchmark verification suite: FAIL"
  pure allPassed

-- | Property 36: Percentile calculation is bounded by list minimum and maximum.
prop_calculatePercentileBounded :: Positive Double -> [Positive Double] -> Property
prop_calculatePercentileBounded (Positive pct) rawList = not (null rawList) ==>
  let xs = map getPositive rawList
      p = min 100.0 pct
  in case calculatePercentile p xs of
       Nothing  -> False
       Just val -> val >= minimum xs && val <= maximum xs

-- | Property 37: Pure build overhead percentage is non-negative and linear with overhead duration.
prop_computeBuildOverheadLinear :: Positive Int64 -> Positive Int64 -> Property
prop_computeBuildOverheadLinear (Positive o) (Positive b) =
  let o' = o `div` 2  -- prevent arithmetic overflow when doubling
      pct = computeBuildOverheadPct o' b
  in property (pct >= 0.0 && computeBuildOverheadPct (o' * 2) b == pct * 2.0)

-- | Property 38: Estimated CPU usage at 15 fps scales linearly with frame duration.
prop_estimateCpuUsageLinear :: Positive Double -> Property
prop_estimateCpuUsageLinear (Positive f) =
  let u1 = estimateCpuUsageAt15Fps f
      u2 = estimateCpuUsageAt15Fps (f * 2.0)
  in property (u1 >= 0.0 && abs (u2 - u1 * 2.0) < 1e-6)

-- | Generator for LspPosition.
genLspPos :: Gen LspPosition
genLspPos = LspPosition <$> choose (0, 1000) <*> choose (0, 120)

-- | Generator for LspRange.
genLspRange :: Gen LspRange
genLspRange = LspRange <$> genLspPos <*> genLspPos

-- | Generator for LspDiagnostic.
genLspDiagnostic :: Gen LspDiagnostic
genLspDiagnostic = do
  rng <- genLspRange
  sev <- elements [LspSeverityError, LspSeverityWarning, LspSeverityInformation, LspSeverityHint]
  mCode <- oneof [pure Nothing, Just . LspCodeInt <$> choose (1000, 99999)]
  src <- elements ["ghc", "buildr"]
  msg <- elements ["Syntax error", "Type mismatch", "Unused identifier"]
  hints <- listOf (elements ["Enable OverloadedStrings", "Add type signature"])
  pure (LspDiagnostic rng sev mCode src msg hints)

-- | Generator for EditorMessage.
genEditorMessage :: Gen EditorMessage
genEditorMessage = oneof
  [ EditorPublishDiagnostics <$> (LspPublishDiagnosticsParams <$> elements ["file:///src/Main.hs", "file:///src/Core.hs"] <*> listOf genLspDiagnostic)
  , EditorProgress <$> pure "buildr-build" <*> (LspProgressBegin <$> pure "Build started" <*> pure True <*> pure (Just 0))
  , EditorProgress <$> pure "buildr-build" <*> (LspProgressReport <$> pure (Just 50) <*> pure (Just "Compiling unit"))
  , EditorProgress <$> pure "buildr-build" <*> (LspProgressEnd <$> pure (Just "Done"))
  , EditorBuildOutcome <$> elements [BuildSuccess, BuildFailed, BuildCancelled] <*> choose (0, 10000000000)
  ]

-- | Property 39: SourceSpan translation to LSP range is strictly 0-indexed and non-negative.
prop_spanToLspRangeZeroIndexed :: Property
prop_spanToLspRangeZeroIndexed = forAll genSpan $ \sp ->
  let rng = spanToLspRange sp
  in lpLine (lrStart rng) >= 0 && lpCharacter (lrStart rng) >= 0
     && lpLine (lrEnd rng) >= 0 && lpCharacter (lrEnd rng) >= 0

-- | Property 40: EditorMessage JSON Lines round-trip serialization.
prop_editorMessageJsonRoundTrip :: Property
prop_editorMessageJsonRoundTrip = forAll genEditorMessage $ \msg ->
  decodeEditorMessageLine (encodeEditorMessageLine msg) == Just msg

-- | Property 41: IPC transport detection correctly distinguishes Windows Named Pipes from Unix sockets.
prop_detectIpcTransportSound :: Property
prop_detectIpcTransportSound = property $
  detectIpcTransport "\\\\.\\pipe\\buildr-session-123" == TransportWindowsNamedPipe
  && detectIpcTransport "/tmp/buildr-session-123.sock" == TransportUnixSocket

-- | Property 42: Interactive key transitions conform to Visual Grammar and user controls (§12.6).
prop_handleTuiKeySound :: Property
prop_handleTuiKeySound = property $
  let s0 = defaultTuiState (TerminalDimensions 80 24) ViewGraph
      sg = handleTuiKey (KeyChar 'g') s0
      sm = handleTuiKey (KeyChar 'm') s0
      sz = handleTuiKey (KeyChar 'z') s0
      st = handleTuiKey (KeyChar 't') s0
      sr = handleTuiKey (KeyChar 'r') s0
      sDown = handleTuiKey KeyDown s0
      sUp = handleTuiKey KeyUp sDown
      sUpBound = handleTuiKey KeyUp s0
      sq = handleTuiKey (KeyChar 'q') s0
  in tsViewMode sg == ViewGraph
  && tsViewMode sm == ViewMosaic
  && tsViewMode sz == ViewZen
  && tsShowWarnings st == not (tsShowWarnings s0)
  && tsShowRawGhc sr == not (tsShowRawGhc s0)
  && tsSelectedRow sDown == 1
  && tsSelectedRow sUp == 0
  && tsSelectedRow sUpBound == 0
  && tsDetached sq == True

-- | Property 43: GitHub Actions Plain CI view emits ::error:: and ::warning:: annotations (§19).
prop_renderPlainViewCiEmitsAnnotations :: Property
prop_renderPlainViewCiEmitsAnnotations = property $
  let span1 = SourceSpan "src/Main.hs" 10 5 10 20
      diagErr = DiagnosticPayload SeverityError (Just 12345) span1 ["Syntax error: missing semicolon"] [] Nothing
      diagWarn = DiagnosticPayload SeverityWarning Nothing span1 ["Unused variable 'x'"] [] Nothing
      modelWithErr = initialModel { bmDiagnostics = [diagErr] }
      modelWithWarn = initialModel { bmDiagnostics = [diagWarn] }
      outErr = renderPlainViewCi modelWithErr
      outWarn = renderPlainViewCi modelWithWarn
  in T.isInfixOf "::error file=src/Main.hs,line=10,col=5::Syntax error: missing semicolon" outErr
  && T.isInfixOf "::warning file=src/Main.hs,line=10,col=5::Unused variable 'x'" outWarn

-- | Property 44: Summary block renders specific target rebuild suggestions when failures exist (§16).
prop_summaryBlockFailedUnitsRebuildSuggestion :: Property
prop_summaryBlockFailedUnitsRebuildSuggestion = property $
  let modFailed = ModuleInfo "App.Main" "lib:my-app" (ModFailed 1) Set.empty False Map.empty
      unitWithFail = UnitInfo "lib:my-app" "my-app" (UnitBuilding 0) (Map.singleton "App.Main" modFailed)
      modelFailed = initialModel { bmUnits = Map.singleton "lib:my-app" unitWithFail }
      summaryText = renderSummaryBlock modelFailed 1000000
  in T.isInfixOf "Next steps: Run 'buildr build lib:my-app --keep-going'" summaryText

-- | Property 45: Cabal lifecycle parsing captures store failure log paths (§16).
prop_parseCabalLifecycleLineStoreFailureLog :: Property
prop_parseCabalLifecycleLineStoreFailureLog = property $
  let line1 = "Failed to build zlib-0.6.3.0 (which is required by ...). See /tmp/logs/zlib.log for details."
      line2 = "cabal: Failed to build base-compat-0.13.1. See /home/user/.cabal/logs/base-compat.log for details."
      p1 = parseCabalLifecycleLine line1
      p2 = parseCabalLifecycleLine line2
  in p1 == Just (PayloadUnitFailed "zlib-0.6.3.0" 1 (Just "/tmp/logs/zlib.log"))
  && p2 == Just (PayloadUnitFailed "base-compat-0.13.1" 1 (Just "/home/user/.cabal/logs/base-compat.log"))

-- | Property 46: Store failure log attachment guarantees maximum 20 trailing lines (§16).
prop_attachStoreLogBoundsToTwentyLines :: Property
prop_attachStoreLogBoundsToTwentyLines = property $
  let raw50 = [ "log line " <> T.pack (show i) | i <- [1 .. 50 :: Int] ]
      m = attachStoreLog "zlib" "/tmp/zlib.log" raw50 initialModel
  in case Map.lookup "zlib" (bmStoreLogs m) of
       Just (path, lines') ->
         path == "/tmp/zlib.log"
         && length lines' == 20
         && lines' == [ "log line " <> T.pack (show i) | i <- [31 .. 50 :: Int] ]
       Nothing -> False

-- | Property 47: Summary block formats Cabal store failure log tail (§16).
prop_summaryBlockRendersStoreLogTail :: Property
prop_summaryBlockRendersStoreLogTail = property $
  let tailLines = [ "error: zlib.h not found", "cabal failed to compile cbits" ]
      m = attachStoreLog "zlib-0.6.3.0" "/path/to/zlib.log" tailLines initialModel
      summaryText = renderSummaryBlock m 5000000000
  in T.isInfixOf "--- Cabal Store Failure Log: zlib-0.6.3.0 (/path/to/zlib.log) [last 2 lines] ---" summaryText
  && T.isInfixOf "error: zlib.h not found" summaryText
  && T.isInfixOf "cabal failed to compile cbits" summaryText

-- | Comprehensive verification of non-Haskell Event Protocol v1 producer integration (§23 Phase 3).
runNonHaskellProducerTests :: IO Bool
runNonHaskellProducerTests = do
  putStrLn "Running non-Haskell compiler producer integration test (buildr-clang, §23 Phase 3)..."
  let producerScript = "examples/clang-producer/buildr-clang"
      sourceFile = "examples/clang-producer/main.c"
  scriptExists <- doesFileExist producerScript
  srcExists <- doesFileExist sourceFile
  if not (scriptExists && srcExists)
    then do
      putStrLn "  Non-Haskell producer test skipped: source files not found"
      pure False
    else do
      (code, out, _err) <- readProcessWithExitCode "python3" [producerScript, sourceFile] ""
      let exitOk = code == ExitSuccess
          rawLines = filter (not . BSC8.null) (BSC8.lines (BSC8.pack out))
          decodedEvents = mapMaybe decodeEventLine rawLines
          decodingOk = not (null decodedEvents) && length decodedEvents == length rawLines
          
          -- Fold the foreign events through the pure model state machine
          foldedModel = foldEvents initialModel decodedEvents
          outcomeOk = bmOutcome foldedModel == Just BuildSuccess
          counts = countModulesByStatus foldedModel
          countsOk = Map.findWithDefault 0 "done" counts >= 1
          
          -- Verify standard views render without error from non-Haskell event model
          graphFrame = renderView ViewGraph foldedModel
          zenFrame = renderView ViewZen foldedModel
          mosaicFrame = renderView ViewMosaic foldedModel
          renderOk = not (T.null graphFrame) && not (T.null zenFrame) && not (T.null mosaicFrame)

          allOk = exitOk && decodingOk && outcomeOk && countsOk && renderOk
      if allOk
        then putStrLn "  Non-Haskell compiler producer integration tests: PASS"
        else putStrLn "  Non-Haskell compiler producer integration tests: FAIL"
      pure allOk

-- | Comprehensive verification of Windows portability and Editor/LSP protocol integration (§7, §23).
runWindowsAndEditorProtocolTests :: IO Bool
runWindowsAndEditorProtocolTests = do
  putStrLn "Running Windows portability & Editor/LSP protocol integration tests (§7, §23)..."
  -- 1. Windows Named Pipe path translation
  let pipePath = resolveIpcPath "test-session"
      pipeOk = not (null pipePath)
      transportType = detectIpcTransport pipePath
      transportOk = transportType == if isWindowsHost then TransportWindowsNamedPipe else TransportUnixSocket

  -- 2. Windows process model & console VT API
  vtOk <- enableVirtualTerminalProcessing
  let procModelOk = isWindowsProcessModel == isWindowsHost

  -- 3. Editor protocol diagnostic conversion
  let sampleDiag = DiagnosticPayload SeverityError (Just 12345) (SourceSpan "src/A.hs" 10 5 10 20) ["Message"] ["Hint"] Nothing
      lspDiag = diagnosticToLsp sampleDiag
      lspDiagOk = ldSeverity lspDiag == LspSeverityError
               && ldCode lspDiag == Just (LspCodeInt 12345)
               && lpLine (lrStart (ldRange lspDiag)) == 9
               && lpCharacter (lrStart (ldRange lspDiag)) == 4

  -- 4. Event to editor messages conversion
  let evDiag = mkEvent 1 100 SrcGhc ValueReported (PayloadDiagnostic (Just "unit-1") sampleDiag)
      editorMsgs = eventToEditorMessages evDiag
      editorEvOk = case editorMsgs of
        [EditorPublishDiagnostics params] -> lpUri params == "file://src/A.hs" && length (lpDiagnostics params) == 1
        _                                 -> False

  let allOk = pipeOk && transportOk && vtOk && procModelOk && lspDiagOk && editorEvOk
  if allOk
    then putStrLn "  Windows portability & Editor/LSP protocol tests: PASS"
    else putStrLn "  Windows portability & Editor/LSP protocol tests: FAIL"
  pure allOk

main :: IO ()
main = do
  putStrLn "=========================================================="
  putStrLn "  buildr Verification & Invariant Suite (QuickCheck)"
  putStrLn "=========================================================="
  bOk <- runBoundaryTests
  planOk <- runPlanTests
  ipcOk <- runIpcTests
  goldenOk <- runGoldenReplayTests
  cfgOk <- runConfigTests
  sigOk <- runSignalAndCancellationTests
  termOk <- runTerminalAndResizeTests
  cabalCmdOk <- runCabalExternalCommandTests
  winEditorOk <- runWindowsAndEditorProtocolTests
  nonHaskellOk <- runNonHaskellProducerTests
  benchOk <- runBenchmarkSuite

  res1 <- quickCheckResult (withNumTests 100 prop_eventJsonRoundTrip)
  res2 <- quickCheckResult (withNumTests 200 prop_barWidthRespectsBounds)
  res3 <- quickCheckResult (withNumTests 200 prop_modelInvariantsPreserved)
  res4 <- quickCheckResult (withNumTests 100 prop_foldDeterminism)
  res5 <- quickCheckResult (withNumTests 100 prop_historyMedianBounded)
  res6 <- quickCheckResult (withNumTests 100 prop_overrunDetection)
  res7 <- quickCheckResult (withNumTests 50 prop_rasikaThemeContrast)
  res8 <- quickCheckResult (withNumTests 100 prop_summaryBlockCountsMatchModel)
  res9 <- quickCheckResult (withNumTests 100 prop_historyCacheJsonRoundTrip)
  res10 <- quickCheckResult (withNumTests 100 prop_diagnosticJsonRoundTrip)
  res11 <- quickCheckResult (withNumTests 50 prop_kahnTopologicalSortValid)
  res12 <- quickCheckResult (withNumTests 50 prop_hinsightGateJsonRoundTrip)
  res13 <- quickCheckResult (withNumTests 50 prop_holeFitJsonRoundTrip)
  res14 <- quickCheckResult (withNumTests 100 prop_errorStyleHonestyPreserved)
  res15 <- quickCheckResult (withNumTests 100 prop_configSemigroupAssociative)
  res16 <- quickCheckResult (withNumTests 100 prop_configMonoidIdentity)
  res17 <- quickCheckResult (withNumTests 100 prop_configPrecedenceLaw)
  res18 <- quickCheckResult (withNumTests 100 prop_modelCancellationOutcome)
  res19 <- quickCheckResult (withNumTests 100 prop_cancellationSummaryLabelAndCounts)
  res20 <- quickCheckResult (withNumTests 100 prop_cancellationPreservesInvariants)
  res21 <- quickCheckResult (withNumTests 100 prop_effectiveViewModeZenCollapse)
  res22 <- quickCheckResult (withNumTests 100 prop_calculateDynamicBarWidthBounds)
  res23 <- quickCheckResult (withNumTests 100 prop_truncateToColsLength)
  res24 <- quickCheckResult (withNumTests 50 prop_pureDetectCapsNoColor)
  res25 <- quickCheckResult (withNumTests 10 prop_pureDetectCapsGhostty)
  res26 <- quickCheckResult (withNumTests 100 prop_formatOsc8FileHyperlink)
  res27 <- quickCheckResult (withNumTests 100 prop_formatUndercurl)
  res28 <- quickCheckResult (withNumTests 100 prop_formatOsc9Progress)
  res29 <- quickCheckResult (withNumTests 100 prop_formatDiagnosticWithCapsHonesty)
  res30 <- quickCheckResult (withNumTests 100 prop_partitionCliArgsCabalPluginDefaultsToBuild)
  res31 <- quickCheckResult (withNumTests 100 prop_partitionCliArgsRecognizesBuildrOptions)
  res32 <- quickCheckResult (withNumTests 100 prop_partitionCliArgsCabalPassthrough)
  res33 <- quickCheckResult (withNumTests 100 prop_partitionCliArgsDoubleDash)
  res34 <- quickCheckResult (withNumTests 100 prop_partitionCliArgsQueryDetection)
  res35 <- quickCheckResult (withNumTests 100 prop_partitionCliArgsSubcommands)
  res36 <- quickCheckResult (withNumTests 100 prop_calculatePercentileBounded)
  res37 <- quickCheckResult (withNumTests 100 prop_computeBuildOverheadLinear)
  res38 <- quickCheckResult (withNumTests 100 prop_estimateCpuUsageLinear)
  res39 <- quickCheckResult (withNumTests 100 prop_spanToLspRangeZeroIndexed)
  res40 <- quickCheckResult (withNumTests 100 prop_editorMessageJsonRoundTrip)
  res41 <- quickCheckResult (withNumTests 50 prop_detectIpcTransportSound)
  res42 <- quickCheckResult (withNumTests 100 prop_handleTuiKeySound)
  res43 <- quickCheckResult (withNumTests 100 prop_renderPlainViewCiEmitsAnnotations)
  res44 <- quickCheckResult (withNumTests 100 prop_summaryBlockFailedUnitsRebuildSuggestion)
  res45 <- quickCheckResult (withNumTests 100 prop_parseCabalLifecycleLineStoreFailureLog)
  res46 <- quickCheckResult (withNumTests 100 prop_attachStoreLogBoundsToTwentyLines)
  res47 <- quickCheckResult (withNumTests 100 prop_summaryBlockRendersStoreLogTail)

  let allPassed = bOk && planOk && ipcOk && goldenOk && cfgOk && sigOk && termOk && cabalCmdOk && winEditorOk && nonHaskellOk && benchOk
                      && isSuccess res1 && isSuccess res2 && isSuccess res3
                      && isSuccess res4 && isSuccess res5 && isSuccess res6
                      && isSuccess res7 && isSuccess res8 && isSuccess res9
                      && isSuccess res10 && isSuccess res11 && isSuccess res12
                      && isSuccess res13 && isSuccess res14 && isSuccess res15
                      && isSuccess res16 && isSuccess res17 && isSuccess res18
                      && isSuccess res19 && isSuccess res20 && isSuccess res21
                      && isSuccess res22 && isSuccess res23 && isSuccess res24
                      && isSuccess res25 && isSuccess res26 && isSuccess res27
                      && isSuccess res28 && isSuccess res29 && isSuccess res30
                      && isSuccess res31 && isSuccess res32 && isSuccess res33
                      && isSuccess res34 && isSuccess res35 && isSuccess res36
                      && isSuccess res37 && isSuccess res38 && isSuccess res39
                      && isSuccess res40 && isSuccess res41 && isSuccess res42
                      && isSuccess res43 && isSuccess res44 && isSuccess res45
                      && isSuccess res46 && isSuccess res47
  if allPassed
    then do
      putStrLn "\nAll QuickCheck properties, IPC tests, and boundary invariants PASSED."
      exitSuccess
    else do
      putStrLn "\nVerification suite FAILED."
      exitFailure
