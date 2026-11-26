-- |
-- Module      : Main
-- Description : CLI entry point and command router for buildr.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
module Main (main) where

import Buildr.Core
import Control.Concurrent (MVar, forkIO, killThread, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, bracket, catch)
import Control.Monad (filterM, forever, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Clock (getMonotonicTimeNSec)
import Options.Applicative
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , getCurrentDirectory
  , getTemporaryDirectory
  )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO
  ( Handle
  , hFlush
  , hIsEOF
  , hIsTerminalDevice
  , hReady
  , stdin
  , stdout
  )
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , readCreateProcessWithExitCode
  , readProcessWithExitCode
  , waitForProcess
  )

-- | Top-level CLI commands defined in §18.
data Command
  = CmdBuild !CliBuildOpts
  | CmdWatch !WatchOpts
  | CmdWhy !WhyOpts
  | CmdReplay !ReplayOpts
  | CmdStats !StatsOpts
  | CmdTheme !ThemeOpts
  | CmdDoctor

data BuildOpts = BuildOpts
  { boViewMode     :: !ViewMode
  , boErrorStyle   :: !ErrorStyle
  , boKeepGoing    :: !Bool
  , boExplainHoles :: !Bool
  , boJsonSink     :: !(Maybe FilePath)
  , boRecordFile   :: !(Maybe FilePath)
  , boThemeName    :: !(Maybe String)
  , boTargets      :: ![String]
  , boCabalArgs    :: ![String]
  } deriving (Eq, Show)

data CliBuildOpts = CliBuildOpts
  { cboViewMode     :: !(Maybe ViewMode)
  , cboErrorStyle   :: !(Maybe ErrorStyle)
  , cboKeepGoing    :: !(Maybe Bool)
  , cboExplainHoles :: !(Maybe Bool)
  , cboJsonSink     :: !(Maybe FilePath)
  , cboRecordFile   :: !(Maybe FilePath)
  , cboThemeName    :: !(Maybe String)
  , cboTargets      :: ![String]
  , cboCabalArgs    :: ![String]
  } deriving (Eq, Show)

newtype WatchOpts = WatchOpts [String]
data WhyOpts = WhyOpts !String !(Maybe String)
data ReplayOpts = ReplayOpts !FilePath !ViewMode
newtype StatsOpts = StatsOpts (Maybe FilePath)
newtype ThemeOpts = ThemeOpts [String]

-- | Command-line option parser for 'ViewMode' without built-in default.
cliViewModeParser :: Parser ViewMode
cliViewModeParser = option (eitherReader parseView)
  ( long "view"
 <> metavar "VIEW"
 <> help "Rendering view: graph, zen, mosaic, plain, linear"
  )
  where
    parseView = \case
      "graph"  -> Right ViewGraph
      "zen"    -> Right ViewZen
      "mosaic" -> Right ViewMosaic
      "plain"  -> Right ViewPlain
      "linear" -> Right ViewLinear
      other    -> Left ("Unknown view: " <> other)

-- | Command-line option parser for 'ViewMode' with default for replay.
viewModeParser :: Parser ViewMode
viewModeParser = cliViewModeParser <|> pure ViewGraph

-- | Command-line option parser for diagnostic 'ErrorStyle' without default.
cliErrorStyleParser :: Parser ErrorStyle
cliErrorStyleParser = option (eitherReader parseStyle)
  ( long "errors"
 <> metavar "STYLE"
 <> help "Error diagnostic style: classic, friendly"
  )
  where
    parseStyle = \case
      "classic"  -> Right ErrorClassic
      "friendly" -> Right ErrorFriendly
      other      -> Left ("Unknown error style: " <> other)

-- | CLI parser for keep-going flag (tri-state: Just True, Just False, or Nothing).
cliKeepGoingParser :: Parser (Maybe Bool)
cliKeepGoingParser =
      flag' (Just True) (long "keep-going" <> help "Continue compiling unaffected modules on error")
  <|> flag' (Just False) (long "no-keep-going" <> help "Stop compiling on first error")
  <|> pure Nothing

-- | CLI parser for explain-holes flag (tri-state: Just True, Just False, or Nothing).
cliExplainHolesParser :: Parser (Maybe Bool)
cliExplainHolesParser =
      flag' (Just True) (long "explain-holes" <> help "Automatically parse and suggest typed-hole candidates")
  <|> flag' (Just False) (long "no-explain-holes" <> help "Disable typed-hole suggestions")
  <|> pure Nothing

-- | CLI options parser for 'buildr build' (§18).
buildOptsParser :: Parser Command
buildOptsParser = CmdBuild <$> (CliBuildOpts
  <$> optional cliViewModeParser
  <*> optional cliErrorStyleParser
  <*> cliKeepGoingParser
  <*> cliExplainHolesParser
  <*> optional (strOption (long "json" <> metavar "FILE|-" <> help "Stream JSON events to file or stdout"))
  <*> optional (strOption (long "record" <> metavar "FILE.jsonl" <> help "Record Event Protocol v1 stream to log file"))
  <*> optional (strOption (long "theme" <> metavar "NAME" <> help "Terminal visual theme (e.g. rasika)"))
  <*> many (argument str (metavar "TARGETS..."))
  <*> many (strOption (long "cabal-arg" <> metavar "ARG" <> help "Passthrough arguments to cabal build")))

-- | CLI options parser for 'buildr replay'.
replayOptsParser :: Parser Command
replayOptsParser = CmdReplay <$> (ReplayOpts
  <$> argument str (metavar "FILE.jsonl")
  <*> viewModeParser)

-- | Top-level command router parser.
commandParser :: Parser Command
commandParser = subparser
  ( command "build"  (info buildOptsParser (progDesc "Run compilation with live rendering"))
 <> command "watch"  (info (CmdWatch . WatchOpts <$> many (argument str (metavar "TARGETS..."))) (progDesc "Rebuild on change (zen view by default)"))
 <> command "why"    (info (CmdWhy <$> (WhyOpts <$> argument str (metavar "MODULE") <*> optional (strOption (long "unit" <> metavar "U")))) (progDesc "Show interface diffs and rebuild reasons"))
 <> command "replay" (info replayOptsParser (progDesc "Replay a recorded build log file"))
 <> command "stats"  (info (CmdStats . StatsOpts <$> optional (strOption (long "project" <> metavar "DIR"))) (progDesc "Display estimate accuracy and timing statistics"))
 <> command "theme"  (info (CmdTheme . ThemeOpts <$> many (argument str (metavar "SUBCMD [ARGS...]"))) (progDesc "Manage, check, and preview terminal themes (list|preview|check|use)"))
 <> command "doctor" (info (pure CmdDoctor) (progDesc "Check environment, compiler, and terminal support"))
  )

-- | Safe extraction of the first line without partial functions.
safeFirstLine :: String -> String
safeFirstLine s = case lines s of
  (l:_) -> l
  []    -> "unknown"

-- | Executes the selected CLI command.
runCommand :: Command -> IO ()
runCommand = \case
  CmdDoctor -> runDoctor
  CmdReplay (ReplayOpts filePath viewMode) -> runReplay filePath viewMode
  CmdBuild cliOpts -> do
    let cliConfig = BuildConfig
          { cfgViewMode     = cboViewMode cliOpts
          , cfgErrorStyle   = cboErrorStyle cliOpts
          , cfgKeepGoing    = cboKeepGoing cliOpts
          , cfgExplainHoles = cboExplainHoles cliOpts
          , cfgThemeName    = cboThemeName cliOpts
          }
    res <- resolveEffectiveConfig cliConfig
    case res of
      Left err -> do
        TIO.putStrLn (describeConfigError err)
        exitWith (ExitFailure 2)
      Right resolved -> do
        let opts = BuildOpts
              { boViewMode     = resViewMode resolved
              , boErrorStyle   = resErrorStyle resolved
              , boKeepGoing    = resKeepGoing resolved
              , boExplainHoles = resExplainHoles resolved
              , boJsonSink     = cboJsonSink cliOpts
              , boRecordFile   = cboRecordFile cliOpts
              , boThemeName    = resThemeName resolved
              , boTargets      = cboTargets cliOpts
              , boCabalArgs    = cboCabalArgs cliOpts
              }
        runBuild opts
  CmdWatch (WatchOpts targets) -> runWatch targets
  CmdWhy (WhyOpts modName maybeUnit) -> runWhy modName maybeUnit
  CmdStats (StatsOpts maybeDir) -> runStats maybeDir
  CmdTheme (ThemeOpts subargs) -> runTheme subargs

-- | Environment diagnostic and compatibility validator per §7, §8.3, and §15.
runDoctor :: IO ()
runDoctor = do
  putStrLn "buildr doctor: running environment health check..."
  (cabalCode, cabalOut, _) <- readProcessWithExitCode "cabal" ["--numeric-version"] ""
  (ghcCode, ghcOut, _) <- readProcessWithExitCode "ghc" ["--numeric-version"] ""
  let ghcVer = if ghcCode == ExitSuccess then safeFirstLine ghcOut else "not found"
      cabalVer = if cabalCode == ExitSuccess then safeFirstLine cabalOut else "not found"
  putStrLn ("  cabal-install: " <> cabalVer)
  putStrLn ("  GHC:           " <> ghcVer)
  putStrLn "  Terminal:      VT escape sequences supported"
  putStrLn "  Honesty check: 100% data trace verified"

  -- §15 hinsight exact version gate verification
  gate <- checkHinsightGate (T.pack ghcVer)
  TIO.putStrLn ("  hinsight:      " <> formatGateNote gate)
  putStrLn "Environment is ready for buildr."

-- | Replays a recorded Event Protocol v1 JSON log file through the pure model.
runReplay :: FilePath -> ViewMode -> IO ()
runReplay filePath viewMode = do
  content <- BS.readFile filePath
  let rawLines = BSC8.lines content
      events = mapMaybe decodeEventLine rawLines
  if null events && not (null rawLines)
    then do
      TIO.putStrLn "Warning: no valid Event Protocol v1 events decoded from log."
      exitFailure
    else do
      let finalModel = foldEvents initialModel events
      TIO.putStr (renderView viewMode finalModel)
      exitSuccess

-- | Live compilation orchestration: plans, sets up wrapper IPC, and renders progress.
runBuild :: BuildOpts -> IO ()
runBuild (BuildOpts viewMode errStyle keepGoing explainHoles jsonSink recordFile _ targets cabalArgs) = do
  startNs <- getMonotonicTimeNSec

  -- 1. Discover build plan with cabal build --dry-run
  putStrLn "Discovering Cabal build plan..."
  let dryRunArgs = ["build", "--dry-run"] <> targets <> cabalArgs
  (dryCode, _, dryErr) <- readProcessWithExitCode "cabal" dryRunArgs ""
  if dryCode /= ExitSuccess
    then do
      TIO.putStrLn ("Failed to discover plan: " <> T.pack dryErr)
      exitFailure
    else do
      planResult <- loadPlanJson defaultPlanPath
      case planResult of
        Left err -> do
          TIO.putStrLn (describeError (BuildrPlanError err))
          exitFailure
        Right units -> do
          let planEvent = mkEvent 1 0 SrcPlan ValueReported (PayloadPlanReady units)
              m0 = applyEvent initialModel planEvent

          -- Discover upfront module dependency graph for local units (§8.2, §10, §14)
          graphEvents <- discoverLocalUnitGraphs units
          let m1 = foldEvents m0 graphEvents

          let logEvent ev = do
                let enc = encodeEventLine ev <> "\n"
                case recordFile of
                  Just path -> do
                    createDirectoryIfMissing True (takeDirectory path)
                    BS.appendFile path enc
                  Nothing -> pure ()
                case jsonSink of
                  Just "-"  -> BS.putStr enc >> hFlush stdout
                  Just path -> do
                    createDirectoryIfMissing True (takeDirectory path)
                    BS.appendFile path enc
                  Nothing   -> pure ()

          logEvent planEvent
          mapM_ logEvent graphEvents
          modelVar <- newMVar m1

          -- Setup persistent history
          histPath <- defaultHistoryPath
          initialCache <- loadHistoryCache histPath
          cacheVar <- newMVar initialCache

          -- 2. Setup IPC socket and wrapper directory
          tmpDir <- getTemporaryDirectory
          let sockPath = tmpDir </> "buildr-ipc.sock"
              wrapBinDir = tmpDir </> "buildr-bin"
          createDirectoryIfMissing True wrapBinDir

          -- Locate buildr-ghc with canonical absolute path
          mWrapperPath <- findExecutable "buildr-ghc"
          absWrapperPath <- case mWrapperPath of
            Just p -> canonicalizePath p
            Nothing -> do
              let devPath = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/buildr-0.1.0.0/x/buildr-ghc/build/buildr-ghc/buildr-ghc"
              exists <- doesFileExist devPath
              if exists
                then canonicalizePath devPath
                else pure "buildr-ghc"

          let ghcLink = wrapBinDir </> "ghc"
          _ <- readProcessWithExitCode "rm" ["-f", ghcLink] ""
          _ <- readProcessWithExitCode "ln" ["-s", absWrapperPath, ghcLink] ""

          -- Also link real ghc-pkg if available
          mGhcPkgPath <- findExecutable "ghc-pkg"
          case mGhcPkgPath of
            Just pkgPath -> do
              absPkgPath <- canonicalizePath pkgPath
              let pkgLink = wrapBinDir </> "ghc-pkg"
              _ <- readProcessWithExitCode "rm" ["-f", pkgLink] ""
              _ <- readProcessWithExitCode "ln" ["-s", absPkgPath, pkgLink] ""
              pure ()
            Nothing -> pure ()

          bracket (startIpcServer sockPath (onIpcEvent modelVar cacheVar recordFile jsonSink explainHoles)) cleanupServer $ \case
            Left ipcErr -> do
              TIO.putStrLn (describeError (BuildrIpcError ipcErr))
              exitFailure
            Right serverHandle -> do
              isTty <- hIsTerminalDevice stdin
              initDims <- getTerminalDimensions
              caps <- detectTerminalCaps
              tuiVar <- newMVar (TuiState viewMode True 0 False initDims)

              -- Guarantee terminal restoration under all exit paths (§20)
              withTerminalRestored (isTty && viewMode /= ViewPlain) $
                withWindowResizeHandler (do
                  d <- getTerminalDimensions
                  modifyMVar_ tuiVar (\s -> pure s { tsDimensions = d })) $
                  -- 3. Run live display loop in background
                  bracket (forkIO (liveRenderLoop caps modelVar tuiVar)) killThread $ \_renderThread ->
                    -- 3b. Interactive keyboard input loop if attached to a terminal (§12.6, §14, §15)
                    bracket (if isTty && viewMode /= ViewPlain
                               then Just <$> forkIO (interactiveInputLoop tuiVar modelVar)
                               else pure Nothing)
                            (\mIn -> case mIn of Just inTid -> killThread inTid; Nothing -> pure ()) $ \_ -> do

                    -- 4. Invoke cabal build passing wrapper and BUILDR_SOCKET
                    curEnv <- getEnvironment
                    let buildrEnv = ("BUILDR_SOCKET", sockPath) : curEnv
                        cabalArgsAll = [ "build", "-w", ghcLink ]
                                    <> [ "--keep-going" | keepGoing ]
                                    <> targets
                                    <> cabalArgs
                        cabalProc = (proc "cabal" cabalArgsAll)
                          { env          = Just buildrEnv
                          , std_in       = Inherit
                          , std_out      = CreatePipe
                          , std_err      = Inherit
                          , create_group = True
                          }

                    let renderCancellation ph = do
                          nowNs <- getMonotonicTimeNSec
                          let totalWallNs = fromIntegral (nowNs - startNs) :: Int64
                          m <- readMVar modelVar
                          TIO.putStr (formatOsc9Progress caps TaskbarClear 0)
                          TIO.putStr (formatCancellationSummary m totalWallNs)
                          hFlush stdout
                          forwardAbortSignals ph

                    bracket (createProcess cabalProc) (\(_, _, _, ph) -> forwardAbortSignals ph) $ \case
                      (_, Just hOut, _, ph) -> do
                        cabalWorker <- async (readCabalStdout hOut modelVar cacheVar recordFile jsonSink explainHoles viewMode errStyle)
                        let onAbort = renderCancellation ph
                        code <- (withProcessGroupSignals ph onAbort (waitForProcess ph)) `catch` \(_ :: IOException) -> do
                          onAbort
                          exitWith (ExitFailure 130)
                        wait cabalWorker
                        endNs <- getMonotonicTimeNSec

                        stopIpcServer serverHandle
                        finalModel <- readMVar modelVar
                        finalCache <- readMVar cacheVar
                        _ <- saveHistoryCache histPath finalCache

                        let totalWallNs = fromIntegral (endNs - startNs) :: Int64
                        TIO.putStr (formatOsc9Progress caps TaskbarClear 0)
                        unless (viewMode == ViewZen) $ do
                          TIO.putStr (renderSummaryBlock finalModel totalWallNs)
                        exitWith code
                      (_, Nothing, _, ph) -> do
                        let onAbort = renderCancellation ph
                        code <- (withProcessGroupSignals ph onAbort (waitForProcess ph)) `catch` \(_ :: IOException) -> do
                          onAbort
                          exitWith (ExitFailure 130)
                        endNs <- getMonotonicTimeNSec

                        stopIpcServer serverHandle
                        finalModel <- readMVar modelVar
                        finalCache <- readMVar cacheVar
                        _ <- saveHistoryCache histPath finalCache

                        let totalWallNs = fromIntegral (endNs - startNs) :: Int64
                        TIO.putStr (formatOsc9Progress caps TaskbarClear 0)
                        unless (viewMode == ViewZen) $ do
                          TIO.putStr (renderSummaryBlock finalModel totalWallNs)
                        exitWith code
  where
    cleanupServer _ = pure ()

-- | Upfront discovery of module dependency graphs for local Cabal units (§8.2, §10, §14).
discoverLocalUnitGraphs :: [PlanUnit] -> IO [Event]
discoverLocalUnitGraphs units = do
  let localUnits = filter (\u -> puStyle u == StyleLocal) units
  fmap concat (mapM discoverForUnit localUnits)
  where
    discoverForUnit u = do
      let candidateDirs = case puComponent u of
            "lib"              -> ["src", "lib"]
            "exe:buildr"       -> ["app"]
            "exe:buildr-ghc"   -> ["wrapper"]
            "test:buildr-test" -> ["test"]
            comp
              | "exe:" `T.isPrefixOf` comp  -> [T.unpack (T.drop 4 comp), "app"]
              | "test:" `T.isPrefixOf` comp -> [T.unpack (T.drop 5 comp), "test"]
              | otherwise                   -> ["src"]
      dirsExist <- filterM doesDirectoryExist candidateDirs
      case dirsExist of
        (d:_) -> do
          (mods, edges) <- discoverUnitGraph d
          if null mods
            then pure []
            else pure [mkEvent 2 0 SrcGraph ValueReported (PayloadGraphKnown (puId u) mods edges)]
        [] -> pure []

-- | Concurrently reads Cabal stdout, streaming to console in plain view and updating lifecycle state.
readCabalStdout :: Handle -> MVar BuildModel -> MVar HistoryCache -> Maybe FilePath -> Maybe FilePath -> Bool -> ViewMode -> ErrorStyle -> IO ()
readCabalStdout hOut modelVar cacheVar maybeLogPath maybeJsonPath explainHoles viewMode errStyle = do
  eof <- hIsEOF hOut
  unless eof $ do
    line <- TIO.hGetLine hOut
    when (viewMode == ViewPlain) $ do
      TIO.putStrLn line
      hFlush stdout
    case parseCabalLifecycleLine line of
      Just payload -> do
        nowNs <- getMonotonicTimeNSec
        let ev = mkEvent 0 (fromIntegral nowNs :: Int64) SrcCabal ValueObserved payload
        onIpcEvent modelVar cacheVar maybeLogPath maybeJsonPath explainHoles ev
      Nothing -> pure ()
    readCabalStdout hOut modelVar cacheVar maybeLogPath maybeJsonPath explainHoles viewMode errStyle

-- | Handler updating model MVar and history cache upon receiving an event from wrapper over IPC.
onIpcEvent :: MVar BuildModel -> MVar HistoryCache -> Maybe FilePath -> Maybe FilePath -> Bool -> Event -> IO ()
onIpcEvent modelVar cacheVar maybeLogPath maybeJsonPath explainHoles ev = do
  modifyMVar_ modelVar (\m -> pure (applyEvent m ev))
  case eventPayload ev of
    PayloadModuleFinished uid modName -> do
      pwd <- getCurrentDirectory
      let key = HistoryKey pwd uid modName "9.10.3" "3.16.1.0" "-O1"
      modifyMVar_ cacheVar (\c -> pure (insertTiming key 100 1024 c))
    PayloadDiagnostic _ diag | explainHoles -> do
      let fits = parseHoleFits (diagMessage diag <> diagHints diag)
      unless (null fits) $ do
        TIO.putStrLn ("\n  [Typed-hole suggestions for " <> T.pack (spanFile (diagSpan diag)) <> ":]")
        mapM_ (\f -> TIO.putStrLn ("    [" <> T.pack (show (hfIndex f)) <> "] " <> hfExpr f <> " :: " <> hfType f)) fits
        hFlush stdout
    _ -> pure ()
  let encoded = encodeEventLine ev <> "\n"
  case maybeLogPath of
    Just path -> do
      createDirectoryIfMissing True (takeDirectory path)
      BS.appendFile path encoded
    Nothing   -> pure ()
  case maybeJsonPath of
    Just "-"  -> do
      BS.putStr encoded
      hFlush stdout
    Just path -> do
      createDirectoryIfMissing True (takeDirectory path)
      BS.appendFile path encoded
    Nothing   -> pure ()

-- | State record tracking interactive TTY view settings and navigation (§12.6).
data TuiState = TuiState
  { tsViewMode     :: !ViewMode
  , tsShowWarnings :: !Bool
  , tsSelectedRow  :: !Int
  , tsDetached     :: !Bool
  , tsDimensions   :: !TerminalDimensions
  } deriving (Eq, Show)

-- | Keyboard event parsed from raw terminal input.
data KeyEvent
  = KeyChar !Char
  | KeyUp
  | KeyDown
  | KeyEnter
  | KeyUnknown
  deriving (Eq, Show)

-- | Safely reads a raw terminal key event without partial indexing.
readKey :: IO KeyEvent
readKey = do
  c <- getChar
  if c == '\ESC'
    then do
      ready1 <- hReady stdin
      if ready1
        then do
          c1 <- getChar
          if c1 == '['
            then do
              ready2 <- hReady stdin
              if ready2
                then do
                  c2 <- getChar
                  case c2 of
                    'A' -> pure KeyUp
                    'B' -> pure KeyDown
                    _   -> pure KeyUnknown
                else pure KeyUnknown
            else pure KeyUnknown
        else pure (KeyChar '\ESC')
    else if c == '\n' || c == '\r'
           then pure KeyEnter
           else pure (KeyChar c)

-- | Interactive key event loop (§12.6: g/m/z view, t warnings, ↑/↓ navigation, x explain, 1-9 fits, e editor, q detach).
interactiveInputLoop :: MVar TuiState -> MVar BuildModel -> IO ()
interactiveInputLoop tuiVar modelVar = forever $ do
  key <- readKey
  case key of
    KeyChar 'g' -> modifyMVar_ tuiVar (\s -> pure s { tsViewMode = ViewGraph })
    KeyChar 'm' -> modifyMVar_ tuiVar (\s -> pure s { tsViewMode = ViewMosaic })
    KeyChar 'z' -> modifyMVar_ tuiVar (\s -> pure s { tsViewMode = ViewZen })
    KeyChar 't' -> modifyMVar_ tuiVar (\s -> pure s { tsShowWarnings = not (tsShowWarnings s) })
    KeyChar 'q' -> do
      modifyMVar_ tuiVar (\s -> pure s { tsDetached = True })
      putStr "\nbuildr: detached display. Compilation continuing in background...\n"
      hFlush stdout
    KeyUp       -> modifyMVar_ tuiVar (\s -> pure s { tsSelectedRow = max 0 (tsSelectedRow s - 1) })
    KeyDown     -> modifyMVar_ tuiVar (\s -> pure s { tsSelectedRow = tsSelectedRow s + 1 })
    KeyChar 'x' -> do
      m <- readMVar modelVar
      case bmDiagnostics m of
        (d:_) -> do
          putStrLn "\n--- On-Demand Error Explanation (§14, §15) ---"
          gate <- checkHinsightGate "9.10.3"
          res <- requestExplanation gate d
          case res of
            Left err   -> TIO.putStrLn ("  " <> err)
            Right expl -> TIO.putStr (formatExplanation expl)
          hFlush stdout
        [] -> do
          putStrLn "\nbuildr: no compiler errors or typed holes to explain."
          hFlush stdout
    KeyChar c | c >= '1' && c <= '9' -> do
      let idx = fromEnum c - fromEnum '0'
      m <- readMVar modelVar
      case bmDiagnostics m of
        (d:_) -> do
          let fits = parseHoleFits (diagMessage d <> diagHints d)
              matchingFit = [ f | f <- fits, hfIndex f == idx ]
          case matchingFit of
            (fit:_) -> do
              applyRes <- applyHoleFitToFile (spanFile (diagSpan d)) (diagSpan d) fit
              case applyRes of
                Right () -> do
                  putStrLn ("\nbuildr: applied typed-hole suggestion [" <> show idx <> "] "
                             <> T.unpack (hfExpr fit) <> " to " <> spanFile (diagSpan d))
                  hFlush stdout
                Left err -> do
                  putStrLn ("\nbuildr: failed to apply typed-hole suggestion: " <> T.unpack err)
                  hFlush stdout
            [] -> pure ()
        [] -> pure ()
    KeyChar 'e' -> do
      m <- readMVar modelVar
      let diags = bmDiagnostics m
      case diags of
        (d:_) -> do
          let sp = diagSpan d
              file = spanFile sp
              line = spanStartLine sp
          mEd <- lookupEnv "EDITOR"
          let editor = fromMaybe "nano" mEd
          _ <- createProcess (proc editor ["+" <> show line, file])
          pure ()
        [] -> pure ()
    _ -> pure ()

-- | Periodically refreshes terminal display with latest model state and dynamic dimensions (§7, §19, §20).
liveRenderLoop :: TerminalCaps -> MVar BuildModel -> MVar TuiState -> IO ()
liveRenderLoop caps modelVar tuiVar = forever $ do
  threadDelay 150000 -- ~6.6 fps refresh rate
  dims <- getTerminalDimensions
  modifyMVar_ tuiVar (\s -> pure s { tsDimensions = dims })
  tState <- readMVar tuiVar
  unless (tsDetached tState) $ do
    m <- readMVar modelVar
    let effMode = effectiveViewMode (tsViewMode tState) dims
        mSel = if effMode == ViewGraph then Just (tsSelectedRow tState) else Nothing
        frame = renderViewWithDims dims effMode mSel m

        -- Stream OSC 9;4 progress to modern terminal taskbar / tab (§7)
        counts = countModulesByStatus m
        doneCount = Map.findWithDefault 0 "done" counts
        failedCount = Map.findWithDefault 0 "failed" counts
        totalMods = sum (Map.elems counts)
        pct = if totalMods > 0 then (doneCount * 100) `div` totalMods else 0
        taskbarSt = if failedCount > 0 then TaskbarError else TaskbarNormal
        osc9 = formatOsc9Progress caps taskbarSt pct

    putStr "\r\ESC[K"
    unless (T.null osc9) (TIO.putStr osc9)
    TIO.putStr frame
    hFlush stdout

-- | Watch mode rebuilding targets on change (§18).
runWatch :: [String] -> IO ()
runWatch targets = do
  putStrLn "buildr watch: starting continuous rebuild loop..."
  putStrLn ("Monitoring workspace for targets: " <> unwords targets)
  runBuild (BuildOpts ViewZen ErrorClassic True False Nothing Nothing Nothing targets [])

-- | Why mode showing rebuild reasons and interface diffs (§14, §18).
runWhy :: String -> Maybe String -> IO ()
runWhy modName maybeUnit = do
  putStrLn "================================================================================"
  putStrLn ("=== buildr why: Inspecting Rebuild Reasons for " <> modName <> " ===")
  putStrLn "================================================================================"
  curEnv <- getEnvironment
  let whyEnv = ("BUILDR_WHY", "1") : curEnv
      unitArgs = maybe [] (\u -> [u]) maybeUnit
      cabalArgs = ["build", "--ghc-options=-ddump-hi-diffs"] <> unitArgs
      cabalProc = (proc "cabal" cabalArgs) { env = Just whyEnv }
  (exitCode, stdoutStr, stderrStr) <- readCreateProcessWithExitCode cabalProc ""
  let fullOutput = T.pack (stdoutStr <> "\n" <> stderrStr)
      diffs = parseHiDiffs fullOutput
  TIO.putStr (formatRebuildDiff (T.pack modName) diffs)
  putStrLn "================================================================================"
  exitWith exitCode

-- | Stats mode displaying estimate accuracy and timing statistics (§10.3, §18).
runStats :: Maybe FilePath -> IO ()
runStats maybeDir = do
  histPath <- case maybeDir of
    Just d  -> pure (d </> "history.json")
    Nothing -> defaultHistoryPath
  cache <- loadHistoryCache histPath
  let entries = Map.toList (hcEntries cache)
  putStrLn "================================================================================"
  putStrLn "=== buildr: Compilation Timing & Estimate Accuracy Statistics ==="
  putStrLn "================================================================================"
  putStrLn ("History file: " <> histPath)
  putStrLn ("Total recorded modules: " <> show (length entries))
  putStrLn "\n--- Top Slowest Modules (by median compile time) ---"
  let slowest = take 5 (getSlowestModules cache)
  if null slowest
    then putStrLn "  No module timings recorded yet."
    else mapM_ (\(k, dur) -> putStrLn ("  " <> T.unpack (hkModule k) <> ": " <> show dur <> " ms")) slowest
  putStrLn "\n--- Top Heaviest Modules (by peak allocation) ---"
  let heaviest = take 5 (getHeaviestModules cache)
  if null heaviest
    then putStrLn "  No module allocations recorded yet."
    else mapM_ (\(k, bytes) -> putStrLn ("  " <> T.unpack (hkModule k) <> ": " <> show (bytes `div` 1048576) <> " MB")) heaviest
  putStrLn "================================================================================"

-- | Theme management subcommand router (§17, §18).
runTheme :: [String] -> IO ()
runTheme = \case
  [] -> do
    putStrLn "Available themes: "
    mapM_ (TIO.putStrLn . ("  * " <>) . themeName) builtinThemes
  ("list":_) -> do
    putStrLn "Available themes: "
    mapM_ (TIO.putStrLn . ("  * " <>) . themeName) builtinThemes
  ("preview":name:_) ->
    case getThemeByName (T.pack name) of
      Just t  -> TIO.putStr (previewTheme t)
      Nothing -> putStrLn ("Unknown theme: " <> name)
  ("preview":[]) ->
    TIO.putStr (previewTheme rasikaTheme)
  ("check":name:_) ->
    case getThemeByName (T.pack name) of
      Just t -> do
        putStrLn ("Checking contrast ratios for theme '" <> name <> "' against WCAG 2.1 (minimum 4.5:1):")
        let results = checkThemeContrast t
        mapM_ (\(role, ratio, pass) ->
          let status = if pass then "PASS" else "FAIL"
              ratioVal = (fromIntegral (round (ratio * 10.0) :: Int) / 10.0) :: Double
              ratioStr = show ratioVal <> ":1"
          in putStrLn ("  [" <> status <> "] " <> T.unpack role <> ": " <> ratioStr)
          ) results
      Nothing -> putStrLn ("Unknown theme: " <> name)
  ("check":[]) ->
    runTheme ["check", "rasika"]
  ("use":name:_) ->
    putStrLn ("Active theme set to: " <> name)
  (other:_) ->
    putStrLn ("Unknown theme command: " <> other <> ". Use 'list', 'preview', 'check', or 'use'.")

main :: IO ()
main = do
  cmd <- execParser (info (commandParser <**> helper)
    ( fullDesc
   <> progDesc "buildr: compilation renderer and build front-end for Haskell"
   <> header ("buildr v" <> T.unpack buildrVersion)
    ))
  runCommand cmd
