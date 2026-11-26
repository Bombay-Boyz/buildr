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
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
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

  let allPassed = bOk && planOk && ipcOk && goldenOk && cfgOk
                      && isSuccess res1 && isSuccess res2 && isSuccess res3
                      && isSuccess res4 && isSuccess res5 && isSuccess res6
                      && isSuccess res7 && isSuccess res8 && isSuccess res9
                      && isSuccess res10 && isSuccess res11 && isSuccess res12
                      && isSuccess res13 && isSuccess res14 && isSuccess res15
                      && isSuccess res16 && isSuccess res17
  if allPassed
    then do
      putStrLn "\nAll QuickCheck properties, IPC tests, and boundary invariants PASSED."
      exitSuccess
    else do
      putStrLn "\nVerification suite FAILED."
      exitFailure
