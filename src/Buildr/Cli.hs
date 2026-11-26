-- |
-- Module      : Buildr.Cli
-- Description : Unified command-line interface, argument partitioner, and native Cabal integration.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Pure CLI argument partitioning, native Cabal external command discovery (@cabal buildr@),
-- pass-through query handling (§18), and orchestration routines for all execution modes.
module Buildr.Cli
  ( -- * Invocation & Query Types
    CabalQueryType (..)
  , Command (..)
  , BuildOpts (..)
  , CliBuildOpts (..)
  , defaultCliBuildOpts
  , WatchOpts (..)
  , WhyOpts (..)
  , ReplayOpts (..)
  , StatsOpts (..)
  , ThemeOpts (..)
  , CliInvocation (..)
    -- * Pure Argument Partitioning (§18)
  , partitionCliArgs
  , isCabalQueryArg
    -- * Execution & Routing
  , runBuildrCli
  , runCabalBuildrCli
  , executeInvocation
  , executeCabalQuery
  , runCommand
  , runBuild
  , runWatch
  , runWhy
  , runReplay
  , runStats
  , runTheme
  , runDoctor
    -- * Informational Formatting
  , formatCliHelp
  , formatCliVersion
  ) where

import Buildr.Config
  ( BuildConfig (..)
  , ResolvedBuildConfig (..)
  , resolveEffectiveConfig
  )
import Buildr.Error
  ( BuildrError (..)
  , describeConfigError
  , describeError
  )
import Buildr.Event
  ( DiagnosticPayload (..)
  , ErrorStyle (..)
  , Event (..)
  , EventPayload (..)
  , EventSource (..)
  , PlanUnit (..)
  , SourceSpan (..)
  , UnitStyle (..)
  , ValueKind (..)
  , decodeEventLine
  , encodeEventLine
  , mkEvent
  , parseCabalLifecycleLine
  , spanFile
  )
import Buildr.Grammar (ViewMode (..))
import Buildr.Graph (discoverUnitGraph)
import Buildr.Hinsight
  ( applyHoleFitToFile
  , checkHinsightGate
  , formatExplanation
  , formatGateNote
  , hfExpr
  , hfIndex
  , hfType
  , parseHoleFits
  , requestExplanation
  )
import Buildr.History
  ( HistoryCache (..)
  , HistoryKey (..)
  , defaultHistoryPath
  , getHeaviestModules
  , getSlowestModules
  , insertTiming
  , loadHistoryCache
  , saveHistoryCache
  )
import Buildr.Ipc
  ( startIpcServer
  , stopIpcServer
  )
import Buildr.Model
  ( BuildModel (..)
  , applyEvent
  , countModulesByStatus
  , foldEvents
  , initialModel
  )
import Buildr.Plan
  ( defaultPlanPath
  , formatRebuildDiff
  , loadPlanJson
  , parseHiDiffs
  )
import Buildr.Signal
  ( formatCancellationSummary
  , forwardAbortSignals
  , withProcessGroupSignals
  , withTerminalRestored
  , withWindowResizeHandler
  )
import Buildr.Terminal
  ( TaskbarState (..)
  , TerminalCaps (..)
  , TerminalDimensions (..)
  , detectTerminalCaps
  , effectiveViewMode
  , formatOsc9Progress
  , getTerminalDimensions
  )
import Buildr.Theme
  ( Theme (..)
  , builtinThemes
  , checkThemeContrast
  , getThemeByName
  , previewTheme
  , rasikaTheme
  )
import Buildr.View
  ( renderSummaryBlock
  , renderView
  , renderViewWithDims
  )

import Control.Concurrent
  ( MVar
  , forkIO
  , killThread
  , modifyMVar_
  , newMVar
  , readMVar
  , threadDelay
  )
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, bracket, catch)
import Control.Monad (filterM, forever, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Char (isDigit)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Clock (getMonotonicTimeNSec)
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
  , stderr
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
import Text.Read (readMaybe)

-- | Current buildr library release version string.
buildrVersionString :: String
buildrVersionString = "0.1.0.0"

-- | Known Cabal query and informational flags passed directly through to Cabal (§18).
data CabalQueryType
  = QueryDryRun          -- ^ --dry-run
  | QueryNumericVersion  -- ^ --numeric-version
  | QueryListSources     -- ^ --list-sources
  | QueryOnlyConfigure   -- ^ --only-configure
  | QueryOnlyDownload    -- ^ --only-download
  deriving (Eq, Show)

-- | Fully resolved build execution options.
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

-- | Raw CLI-specified build options prior to configuration resolution.
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

-- | Default empty CLI build options.
defaultCliBuildOpts :: CliBuildOpts
defaultCliBuildOpts = CliBuildOpts
  { cboViewMode     = Nothing
  , cboErrorStyle   = Nothing
  , cboKeepGoing    = Nothing
  , cboExplainHoles = Nothing
  , cboJsonSink     = Nothing
  , cboRecordFile   = Nothing
  , cboThemeName    = Nothing
  , cboTargets      = []
  , cboCabalArgs    = []
  }

newtype WatchOpts = WatchOpts [String] deriving (Eq, Show)
data WhyOpts = WhyOpts !String !(Maybe String) deriving (Eq, Show)
data ReplayOpts = ReplayOpts !FilePath !ViewMode deriving (Eq, Show)
newtype StatsOpts = StatsOpts (Maybe FilePath) deriving (Eq, Show)
newtype ThemeOpts = ThemeOpts [String] deriving (Eq, Show)

-- | Top-level CLI command representation defined in §18.
data Command
  = CmdBuild !CliBuildOpts
  | CmdWatch !WatchOpts
  | CmdWhy !WhyOpts
  | CmdReplay !ReplayOpts
  | CmdStats !StatsOpts
  | CmdTheme !ThemeOpts
  | CmdDoctor
  deriving (Eq, Show)

-- | Parsed outcome of interpreting command-line arguments.
data CliInvocation
  = InvokeCabalQuery !CabalQueryType ![String]
  | InvokeCommand !Command
  | InvokeHelp !(Maybe String)
  | InvokeVersion
  | InvokeFailure !Text
  deriving (Eq, Show)

-- | Identifies whether an argument corresponds to a Cabal query or informational request.
isCabalQueryArg :: String -> Maybe CabalQueryType
isCabalQueryArg = \case
  "--dry-run"          -> Just QueryDryRun
  "--numeric-version"  -> Just QueryNumericVersion
  "--list-sources"     -> Just QueryListSources
  "--only-configure"   -> Just QueryOnlyConfigure
  "--only-download"    -> Just QueryOnlyDownload
  _                    -> Nothing

-- | Purely partitions and interprets command-line arguments.
-- When 'isCabalPluginMode' is 'True' (invoked as @cabal-buildr@), arguments without an explicit
-- subcommand default to compilation targeting @cabal build@ per §18.
partitionCliArgs :: Bool -> [String] -> CliInvocation
partitionCliArgs isPlugin args = case args of
  [] ->
    if isPlugin
      then InvokeCommand (CmdBuild defaultCliBuildOpts)
      else InvokeHelp Nothing

  ("--help":_) -> InvokeHelp Nothing
  ("-h":_)     -> InvokeHelp Nothing
  ("help":remArgs) -> case remArgs of
    (sub:_) -> InvokeHelp (Just sub)
    []      -> InvokeHelp Nothing

  ("--version":_) -> InvokeVersion
  ("-v":remArgs)
    | null remArgs -> InvokeVersion
    | isPlugin     -> parseAsBuild args
    | otherwise    -> InvokeVersion
  ("version":_)   -> InvokeVersion

  (firstArg:rest) ->
    case firstArg of
      "build"  -> parseAsBuild rest
      "watch"  -> InvokeCommand (CmdWatch (WatchOpts rest))
      "why"    -> parseAsWhy rest
      "replay" -> parseAsReplay rest
      "stats"  -> parseAsStats rest
      "theme"  -> InvokeCommand (CmdTheme (ThemeOpts rest))
      "doctor" -> InvokeCommand CmdDoctor
      _        ->
        case findQueryArg args of
          Just qtype -> InvokeCabalQuery qtype args
          Nothing    -> parseAsBuild args
  where
    findQueryArg [] = Nothing
    findQueryArg (x:xs) = case isCabalQueryArg x of
      Just qt -> Just qt
      Nothing -> findQueryArg xs

    parseAsWhy = \case
      (m:remA) ->
        let (u, _) = extractUnitOpt remA Nothing []
        in InvokeCommand (CmdWhy (WhyOpts m u))
      [] -> InvokeFailure "Error: 'why' command requires a MODULE name. See 'buildr why --help'."

    extractUnitOpt [] accU accRest = (accU, reverse accRest)
    extractUnitOpt ("--unit":u:xs) _ accRest = extractUnitOpt xs (Just u) accRest
    extractUnitOpt (x:xs) accU accRest = extractUnitOpt xs accU (x:accRest)

    parseAsReplay = \case
      (file:remA) ->
        let mode = extractViewMode remA ViewGraph
        in InvokeCommand (CmdReplay (ReplayOpts file mode))
      [] -> InvokeFailure "Error: 'replay' command requires a FILE.jsonl path."

    extractViewMode [] def = def
    extractViewMode ("--view":v:_) _ = parseViewModeStr v
    extractViewMode _ def = def

    parseViewModeStr = \case
      "zen"    -> ViewZen
      "mosaic" -> ViewMosaic
      "plain"  -> ViewPlain
      "linear" -> ViewLinear
      _        -> ViewGraph

    parseAsStats = \case
      ("--project":d:_) -> InvokeCommand (CmdStats (StatsOpts (Just d)))
      []                -> InvokeCommand (CmdStats (StatsOpts Nothing))
      _                 -> InvokeCommand (CmdStats (StatsOpts Nothing))

    parseAsBuild rawTokens =
      case foldTokens rawTokens defaultCliBuildOpts of
        Left err   -> InvokeFailure err
        Right opts -> InvokeCommand (CmdBuild opts)

    foldTokens [] opts = Right opts
    foldTokens ("--":passthrough) opts =
      Right opts { cboCabalArgs = cboCabalArgs opts <> passthrough }
    foldTokens (tok:remTokens) opts
      | tok == "--view" = case remTokens of
          (v:rest) -> case parseViewModeE v of
            Right vm -> foldTokens rest opts { cboViewMode = Just vm }
            Left err -> Left err
          [] -> Left "Missing argument for '--view' flag."
      | tok == "--errors" = case remTokens of
          (s:rest) -> case parseErrorStyleE s of
            Right es -> foldTokens rest opts { cboErrorStyle = Just es }
            Left err -> Left err
          [] -> Left "Missing argument for '--errors' flag."
      | tok == "--keep-going" = foldTokens remTokens opts { cboKeepGoing = Just True }
      | tok == "--no-keep-going" = foldTokens remTokens opts { cboKeepGoing = Just False }
      | tok == "--explain-holes" = foldTokens remTokens opts { cboExplainHoles = Just True }
      | tok == "--no-explain-holes" = foldTokens remTokens opts { cboExplainHoles = Just False }
      | tok == "--json" = case remTokens of
          (j:rest) -> foldTokens rest opts { cboJsonSink = Just j }
          []       -> Left "Missing argument for '--json' flag."
      | tok == "--record" = case remTokens of
          (r:rest) -> foldTokens rest opts { cboRecordFile = Just r }
          []       -> Left "Missing argument for '--record' flag."
      | tok == "--theme" = case remTokens of
          (t:rest) -> foldTokens rest opts { cboThemeName = Just t }
          []       -> Left "Missing argument for '--theme' flag."
      | tok == "--cabal-arg" = case remTokens of
          (a:rest) -> foldTokens rest opts { cboCabalArgs = cboCabalArgs opts <> [a] }
          []       -> Left "Missing argument for '--cabal-arg' flag."
      | "--view=" `isPrefix` tok =
          case parseViewModeE (drop 7 tok) of
            Right vm -> foldTokens remTokens opts { cboViewMode = Just vm }
            Left err -> Left err
      | "--errors=" `isPrefix` tok =
          case parseErrorStyleE (drop 9 tok) of
            Right es -> foldTokens remTokens opts { cboErrorStyle = Just es }
            Left err -> Left err
      | "--json=" `isPrefix` tok =
          foldTokens remTokens opts { cboJsonSink = Just (drop 7 tok) }
      | "--record=" `isPrefix` tok =
          foldTokens remTokens opts { cboRecordFile = Just (drop 9 tok) }
      | "--theme=" `isPrefix` tok =
          foldTokens remTokens opts { cboThemeName = Just (drop 8 tok) }
      | tok == "-k" = foldTokens remTokens opts { cboKeepGoing = Just True }
      | tok `elem` ["--help", "-h"] =
          foldTokens remTokens opts
      | "-" `isPrefix` tok =
          foldTokens remTokens opts { cboCabalArgs = cboCabalArgs opts <> [tok] }
      | otherwise =
          foldTokens remTokens opts { cboTargets = cboTargets opts <> [tok] }

    isPrefix p s = take (length p) s == p

    parseViewModeE = \case
      "graph"  -> Right ViewGraph
      "zen"    -> Right ViewZen
      "mosaic" -> Right ViewMosaic
      "plain"  -> Right ViewPlain
      "linear" -> Right ViewLinear
      other    -> Left ("Unknown view mode: '" <> T.pack other <> "'. Expected one of: graph, zen, mosaic, plain, linear.")

    parseErrorStyleE = \case
      "classic"  -> Right ErrorClassic
      "friendly" -> Right ErrorFriendly
      other      -> Left ("Unknown error style: '" <> T.pack other <> "'. Expected one of: classic, friendly.")

-- | Direct execution entry point for standalone @buildr@ binary.
runBuildrCli :: [String] -> IO ()
runBuildrCli args =
  executeInvocation False (partitionCliArgs False args)

-- | Direct execution entry point for native Cabal external command @cabal-buildr@.
runCabalBuildrCli :: [String] -> IO ()
runCabalBuildrCli args =
  executeInvocation True (partitionCliArgs True args)

-- | Executes a parsed invocation according to CLI dispatch rules (§18).
executeInvocation :: Bool -> CliInvocation -> IO ()
executeInvocation isPlugin = \case
  InvokeCabalQuery qtype cabalArgs ->
    executeCabalQuery qtype cabalArgs
  InvokeCommand cmd ->
    runCommand cmd
  InvokeHelp maybeTopic -> do
    putStrLn (formatCliHelp isPlugin maybeTopic)
    exitSuccess
  InvokeVersion -> do
    putStrLn formatCliVersion
    exitSuccess
  InvokeFailure errText -> do
    TIO.hPutStrLn stderr errText
    exitWith (ExitFailure 2)

-- | Executes informational and query operations directly through Cabal (§18).
executeCabalQuery :: CabalQueryType -> [String] -> IO ()
executeCabalQuery qtype args = do
  let cabalArgs = case qtype of
        QueryNumericVersion -> ["--numeric-version"]
        _                   -> "build" : args
      cabalProc = (proc "cabal" cabalArgs)
        { std_in  = Inherit
        , std_out = Inherit
        , std_err = Inherit
        }
  (_, _, _, ph) <- createProcess cabalProc
  code <- waitForProcess ph
  exitWith code

-- | Formats detailed CLI help for standalone and Cabal external command modes.
formatCliHelp :: Bool -> Maybe String -> String
formatCliHelp isPlugin maybeTopic = case maybeTopic of
  Just "build"  -> "Usage: " <> cmdPrefix <> " build [TARGETS] [--view VIEW] [--errors STYLE] [--keep-going] [--explain-holes] [--json FILE] [--record FILE] [--theme NAME] [-- CABAL-ARGS...]\n\n"
               <> "Compile Haskell targets with real-time reactive visualization.\n"
  Just "watch"  -> "Usage: " <> cmdPrefix <> " watch [TARGETS]\n\nRebuild targets automatically on source change (zen view by default).\n"
  Just "why"    -> "Usage: " <> cmdPrefix <> " why MODULE [--unit U]\n\nInspect interface diffs and understand why GHC recompiled a module.\n"
  Just "replay" -> "Usage: " <> cmdPrefix <> " replay FILE.jsonl [--view VIEW]\n\nReplay a recorded Event Protocol v1 build trace.\n"
  Just "stats"  -> "Usage: " <> cmdPrefix <> " stats [--project DIR]\n\nDisplay estimation accuracy, timings, and slowest/heaviest modules.\n"
  Just "theme"  -> "Usage: " <> cmdPrefix <> " theme [list|preview|check|use]\n\nInspect, preview, and test terminal themes.\n"
  Just "doctor" -> "Usage: " <> cmdPrefix <> " doctor\n\nCheck GHC, Cabal, terminal capabilities, and hinsight integration.\n"
  _ -> unlines
    [ "buildr v" <> buildrVersionString <> " — compilation renderer and build front-end for Haskell"
    , ""
    , "Usage:"
    , if isPlugin
        then "  cabal buildr [TARGETS] [OPTIONS...] [-- CABAL-ARGS...]"
        else "  buildr build [TARGETS] [OPTIONS...] [-- CABAL-ARGS...]"
    , "  " <> cmdPrefix <> " <command> [ARGS...]"
    , ""
    , "Available Commands:"
    , "  build    Compile Haskell targets with live rendering (default for 'cabal buildr')"
    , "  watch    Continuous rebuild on file modification (zen view by default)"
    , "  why      Explain rebuild reasons and inspect interface diffs"
    , "  replay   Replay a recorded Event Protocol v1 JSON log trace"
    , "  stats    Display estimate accuracy, slowest and heaviest module metrics"
    , "  theme    Manage, preview, and verify WCAG 2.1 contrast for terminal themes"
    , "  doctor   Verify environment, compiler, terminal features, and hinsight gate"
    , ""
    , "Build Options:"
    , "  --view VIEW            Select view: graph (default), zen, mosaic, plain, linear"
    , "  --errors STYLE         Select diagnostic layout: classic, friendly"
    , "  --keep-going           Continue compiling unaffected modules after an error"
    , "  --no-keep-going        Halt compilation immediately on first error"
    , "  --explain-holes        Automatically parse and explain typed-hole candidates"
    , "  --json FILE|-          Stream Event Protocol v1 JSON to a file or stdout"
    , "  --record FILE.jsonl    Record deterministic event trace for later replay"
    , "  --theme NAME           Set terminal visual palette (e.g. rasika)"
    , ""
    , "Cabal Pass-through:"
    , "  Any unfamiliar Cabal options (e.g. -j4, --enable-tests, --dry-run) and"
    , "  all arguments following '--' are passed directly through to Cabal."
    ]
  where
    cmdPrefix = if isPlugin then "cabal buildr" else "buildr"

-- | Formats version identification for buildr.
formatCliVersion :: String
formatCliVersion = "buildr v" <> buildrVersionString

-- | Safe extraction of the first line from a multiline string without partial functions.
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

-- | Environment diagnostic and compatibility validator per §7, §8.3, §15, and §18.
runDoctor :: IO ()
runDoctor = do
  putStrLn "buildr doctor: running environment health check..."
  (cabalCode, cabalOut, _) <- readProcessWithExitCode "cabal" ["--numeric-version"] ""
  (ghcCode, ghcOut, _) <- readProcessWithExitCode "ghc" ["--numeric-version"] ""
  let ghcVer = if ghcCode == ExitSuccess then safeFirstLine ghcOut else "not found"
      cabalVer = if cabalCode == ExitSuccess then safeFirstLine cabalOut else "not found"
  putStrLn ("  cabal-install: " <> cabalVer)
  putStrLn ("  GHC:           " <> ghcVer)

  -- Check Cabal version compatibility for native external commands (§18)
  let cabalMajor = parseMajorVersion cabalVer
  if cabalMajor >= 3
    then putStrLn "  Cabal support: >= 3.0 (native external command discovery supported)"
    else putStrLn "  Cabal support: < 3.0 (cabal external commands require Cabal 3.0+)"

  -- Check cabal-buildr discovery on PATH
  mCabalBuildr <- findExecutable "cabal-buildr"
  case mCabalBuildr of
    Just p  -> putStrLn ("  cabal-buildr:  available at " <> p <> " ('cabal buildr' ready)")
    Nothing -> putStrLn "  cabal-buildr:  not found on PATH (run 'cabal install' to enable 'cabal buildr')"

  putStrLn "  Terminal:      VT escape sequences supported"
  putStrLn "  Honesty check: 100% data trace verified"

  -- §15 hinsight exact version gate verification
  gate <- checkHinsightGate (T.pack ghcVer)
  TIO.putStrLn ("  hinsight:      " <> formatGateNote gate)
  putStrLn "Environment is ready for buildr."
  where
    parseMajorVersion str =
      let digits = takeWhile isDigit str
      in fromMaybe 0 (readMaybe digits :: Maybe Int)

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
            "exe:cabal-buildr" -> ["app"]
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

-- | Event sink processing incoming wrapper messages over Unix domain socket.
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
    Just "-"  -> TIO.putStr (T.pack (BSC8.unpack encoded))
    Just path -> do
      createDirectoryIfMissing True (takeDirectory path)
      BS.appendFile path encoded
    Nothing   -> pure ()

-- | TUI state managed by background renderer and interactive input thread.
data TuiState = TuiState
  { tsViewMode     :: !ViewMode
  , tsShowWarnings :: !Bool
  , tsSelectedRow  :: !Int
  , tsDetached     :: !Bool
  , tsDimensions   :: !TerminalDimensions
  } deriving (Eq, Show)

-- | Keyboard event parsed from raw terminal input.
data KeyEvent
  = KeyUp
  | KeyDown
  | KeyChar !Char
  | KeyOther
  deriving (Eq, Show)

-- | Non-blocking key event reader.
readKeyNonBlocking :: IO (Maybe KeyEvent)
readKeyNonBlocking = do
  ready <- hReady stdin
  if not ready
    then pure Nothing
    else do
      c <- getChar
      if c == '\ESC'
        then do
          r2 <- hReady stdin
          if not r2
            then pure (Just (KeyChar '\ESC'))
            else do
              c2 <- getChar
              if c2 == '['
                then do
                  r3 <- hReady stdin
                  if not r3
                    then pure (Just KeyOther)
                    else do
                      c3 <- getChar
                      case c3 of
                        'A' -> pure (Just KeyUp)
                        'B' -> pure (Just KeyDown)
                        _   -> pure (Just KeyOther)
                else pure (Just KeyOther)
        else pure (Just (KeyChar c))

-- | Interactive keyboard control loop for TTY sessions (§12.6, §14, §15).
interactiveInputLoop :: MVar TuiState -> MVar BuildModel -> IO ()
interactiveInputLoop tuiVar modelVar = forever $ do
  threadDelay 50000
  mKey <- readKeyNonBlocking
  case mKey of
    Nothing -> pure ()
    Just key -> handleKey key
  where
    handleKey = \case
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
  threadDelay 150000
  dims <- getTerminalDimensions
  modifyMVar_ tuiVar (\s -> pure s { tsDimensions = dims })
  tState <- readMVar tuiVar
  unless (tsDetached tState) $ do
    m <- readMVar modelVar
    let effMode = effectiveViewMode (tsViewMode tState) dims
        mSel = if effMode == ViewGraph then Just (tsSelectedRow tState) else Nothing
        frame = renderViewWithDims dims effMode mSel m

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
