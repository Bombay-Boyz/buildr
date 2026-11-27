{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Buildr.Event
-- Description : Event Protocol v1 specification, types, and JSON streaming codecs.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Published Event Protocol v1 specification for build front-ends and compilers.
-- Every event in the stream is a self-contained, typed record with monotonic sequence
-- numbering and nanosecond timestamps.
module Buildr.Event
  ( -- * Core Envelope & Kinds
    ProtocolVersion
  , currentProtocolVersion
  , Event (..)
  , EventSource (..)
  , ValueKind (..)
  , EventPayload (..)
    -- * Domain Payload Structures
  , PlanUnit (..)
  , UnitStyle (..)
  , ModuleNode (..)
  , GraphEdge (..)
  , DiagnosticSeverity (..)
  , SourceSpan (..)
  , DiagnosticPayload (..)
  , BuildOutcome (..)
  , BuildCounts (..)
    -- * Codec and Smart Construction
  , mkEvent
  , encodeEventLine
  , decodeEventLine
    -- * Stream Recognisers & Diagnostic Formatting (§13, §22 G4, G5)
  , ErrorStyle (..)
  , parseTimingLine
  , parseCabalLifecycleLine
  , parseGhcDiagnosticJson
  , formatDiagnostic
  , formatDiagnosticWithStyle
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=), object, withObject, encode, decodeStrict)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Text.Read (readMaybe)

-- | Current protocol major version (fixed at 1 for Event Protocol v1).
type ProtocolVersion = Int

-- | Literal 1 for Protocol Version 1.
currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = 1

-- | The origin subsystem that generated the event.
data EventSource
  = SrcCabal
  | SrcGhc
  | SrcPlan
  | SrcGraph
  | SrcHistory
  | SrcBuildr
  deriving (Eq, Show, Generic)

instance ToJSON EventSource where
  toJSON = \case
    SrcCabal   -> "cabal"
    SrcGhc     -> "ghc"
    SrcPlan    -> "plan"
    SrcGraph   -> "graph"
    SrcHistory -> "history"
    SrcBuildr  -> "buildr"

instance FromJSON EventSource where
  parseJSON = Aeson.withText "EventSource" $ \case
    "cabal"   -> pure SrcCabal
    "ghc"     -> pure SrcGhc
    "plan"    -> pure SrcPlan
    "graph"   -> pure SrcGraph
    "history" -> pure SrcHistory
    "buildr"  -> pure SrcBuildr
    "clang"   -> pure SrcGhc
    other     -> fail ("Unknown event source: " <> T.unpack other)

-- | The epistemological kind of the event payload, enforcing the Honesty Contract.
data ValueKind
  = ValueObserved  -- ^ Measured wall-clock / monotonic duration at arrival.
  | ValueReported  -- ^ Directly stated by GHC, Cabal, or compiler flags.
  | ValueDerived   -- ^ Computed solely from reported and observed facts.
  | ValueEstimate  -- ^ Predicted from history cache; never shown as fact.
  deriving (Eq, Show, Generic)

instance ToJSON ValueKind where
  toJSON = \case
    ValueObserved -> "observed"
    ValueReported -> "reported"
    ValueDerived  -> "derived"
    ValueEstimate -> "estimate"

instance FromJSON ValueKind where
  parseJSON = Aeson.withText "ValueKind" $ \case
    "observed" -> pure ValueObserved
    "reported" -> pure ValueReported
    "derived"  -> pure ValueDerived
    "estimate" -> pure ValueEstimate
    other      -> fail ("Unknown value kind: " <> T.unpack other)

-- | Whether a Cabal unit belongs to the local workspace or the immutable store.
data UnitStyle = StyleLocal | StyleStore deriving (Eq, Show, Generic)

instance ToJSON UnitStyle where
  toJSON StyleLocal = "local"
  toJSON StyleStore = "store"

instance FromJSON UnitStyle where
  parseJSON = Aeson.withText "UnitStyle" $ \case
    "local" -> pure StyleLocal
    "store" -> pure StyleStore
    other   -> fail ("Unknown unit style: " <> T.unpack other)

-- | A unit planned by Cabal in plan.json.
data PlanUnit = PlanUnit
  { puId            :: !Text
  , puPackage       :: !Text
  , puVersion       :: !Text
  , puComponent     :: !Text
  , puStyle         :: !UnitStyle
  , puDependencies  :: ![Text]
  , puRebuildReason :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON PlanUnit
instance FromJSON PlanUnit

-- | A module within a unit's dependency graph.
data ModuleNode = ModuleNode
  { mnName       :: !Text
  , mnSourcePath :: !FilePath
  , mnIsBoot     :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON ModuleNode
instance FromJSON ModuleNode

-- | A directed edge in the module dependency graph.
data GraphEdge = GraphEdge
  { geSource         :: !(Maybe Text)
  , geTarget         :: !Text
  , geIsSourceImport :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON GraphEdge
instance FromJSON GraphEdge

-- | Source code span for diagnostics.
data SourceSpan = SourceSpan
  { spanFile   :: !FilePath
  , spanStartLine :: !Int
  , spanStartCol  :: !Int
  , spanEndLine   :: !Int
  , spanEndCol    :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON SourceSpan
instance FromJSON SourceSpan

-- | Severity classification for compiler diagnostics.
data DiagnosticSeverity = SeverityError | SeverityWarning | SeverityInfo deriving (Eq, Show, Generic)

instance ToJSON DiagnosticSeverity where
  toJSON = \case
    SeverityError   -> "error"
    SeverityWarning -> "warning"
    SeverityInfo    -> "info"

instance FromJSON DiagnosticSeverity where
  parseJSON = Aeson.withText "DiagnosticSeverity" $ \t -> case T.toLower t of
    "error"   -> pure SeverityError
    "warning" -> pure SeverityWarning
    "info"    -> pure SeverityInfo
    other     -> fail ("Unknown severity: " <> T.unpack other)

-- | Detailed payload for GHC diagnostics parsed via JSON.
data DiagnosticPayload = DiagnosticPayload
  { diagSeverity :: !DiagnosticSeverity
  , diagCode     :: !(Maybe Int)
  , diagSpan     :: !SourceSpan
  , diagMessage  :: ![Text]
  , diagHints    :: ![Text]
  , diagFlag     :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON DiagnosticPayload
instance FromJSON DiagnosticPayload

-- | Terminal outcome of a build run.
data BuildOutcome = BuildSuccess | BuildFailed | BuildCancelled deriving (Eq, Show, Generic)

instance ToJSON BuildOutcome where
  toJSON = \case
    BuildSuccess   -> "success"
    BuildFailed    -> "failed"
    BuildCancelled -> "cancelled"

instance FromJSON BuildOutcome where
  parseJSON = Aeson.withText "BuildOutcome" $ \case
    "success"        -> pure BuildSuccess
    "BuildSuccess"   -> pure BuildSuccess
    "failed"         -> pure BuildFailed
    "BuildFailed"    -> pure BuildFailed
    "cancelled"      -> pure BuildCancelled
    "BuildCancelled" -> pure BuildCancelled
    other            -> fail ("Unknown build outcome: " <> T.unpack other)

-- | Summary module counts at build completion.
data BuildCounts = BuildCounts
  { countDone       :: !Int
  , countFailed     :: !Int
  , countBlocked    :: !Int
  , countNotReached :: !Int
  , countSkipped    :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON BuildCounts where
  toJSON bc = object
    [ "countDone"       .= countDone bc
    , "countFailed"     .= countFailed bc
    , "countBlocked"    .= countBlocked bc
    , "countNotReached" .= countNotReached bc
    , "countSkipped"    .= countSkipped bc
    ]

instance FromJSON BuildCounts where
  parseJSON = withObject "BuildCounts" $ \o ->
    BuildCounts
      <$> (o .: "countDone" <|> o .: "bcDone" <|> o .: "done")
      <*> (o .: "countFailed" <|> o .: "bcFailed" <|> o .: "failed")
      <*> (o .: "countBlocked" <|> o .: "bcBlocked" <|> o .: "blocked")
      <*> (o .: "countNotReached" <|> o .: "bcNotReached" <|> o .: "notReached")
      <*> (o .: "countSkipped" <|> o .: "bcSkipped" <|> o .: "skipped")

-- | The closed set of event payload schemas supported in Protocol v1.
data EventPayload
  = PayloadBuildStarted !Text !Text !Text
  | PayloadPlanReady ![PlanUnit]
  | PayloadGraphKnown !Text ![ModuleNode] ![GraphEdge]
  | PayloadUnitStarted !Text
  | PayloadUnitCompleted !Text !Int
  | PayloadUnitFailed !Text !Int !(Maybe FilePath)
  | PayloadUnitStopped !Text
  | PayloadModuleStarted !Text !Text !Int !Int !FilePath !FilePath !(Maybe Text)
  | PayloadModulePhase !Text !Text !Text !Int64 !Int64
  | PayloadModuleFinished !Text !Text
  | PayloadModuleSkipped !Text !Text !Text
  | PayloadDiagnostic !(Maybe Text) !DiagnosticPayload
  | PayloadOpaque !(Maybe Text) !Text ![Text] !Text
  | PayloadSlotsChanged !Int !Int
  | PayloadLinking !Text !Text
  | PayloadBuildDone !BuildOutcome !BuildCounts !Int64
  deriving (Eq, Show, Generic)

instance ToJSON EventPayload where
  toJSON = \case
    PayloadBuildStarted b c g -> object
      [ "kind" .= ("BuildStarted" :: Text), "buildr" .= b, "cabal" .= c, "ghc" .= g ]
    PayloadPlanReady units -> object
      [ "kind" .= ("PlanReady" :: Text), "units" .= units ]
    PayloadGraphKnown u mods edges -> object
      [ "kind" .= ("GraphKnown" :: Text), "unit" .= u, "modules" .= mods, "edges" .= edges ]
    PayloadUnitStarted u -> object
      [ "kind" .= ("UnitStarted" :: Text), "unit" .= u ]
    PayloadUnitCompleted u code -> object
      [ "kind" .= ("UnitCompleted" :: Text), "unit" .= u, "exitCode" .= code ]
    PayloadUnitFailed u code logPath -> object
      [ "kind" .= ("UnitFailed" :: Text), "unit" .= u, "exitCode" .= code, "log" .= logPath ]
    PayloadUnitStopped u -> object
      [ "kind" .= ("UnitStopped" :: Text), "unit" .= u ]
    PayloadModuleStarted u m k n src obj reas -> object
      [ "kind" .= ("ModuleStarted" :: Text), "unit" .= u, "module" .= m, "k" .= k, "n" .= n
      , "src" .= src, "obj" .= obj, "reason" .= reas ]
    PayloadModulePhase u m phase ms alloc -> object
      [ "kind" .= ("ModulePhase" :: Text), "unit" .= u, "module" .= m, "phase" .= phase
      , "ms" .= ms, "alloc" .= alloc ]
    PayloadModuleFinished u m -> object
      [ "kind" .= ("ModuleFinished" :: Text), "unit" .= u, "module" .= m ]
    PayloadModuleSkipped u m reas -> object
      [ "kind" .= ("ModuleSkipped" :: Text), "unit" .= u, "module" .= m, "reason" .= reas ]
    PayloadDiagnostic mu diag -> object
      [ "kind" .= ("Diagnostic" :: Text), "unit" .= mu, "diag" .= diag ]
    PayloadOpaque mu strm lines' cls -> object
      [ "kind" .= ("Opaque" :: Text), "unit" .= mu, "stream" .= strm, "lines" .= lines', "class" .= cls ]
    PayloadSlotsChanged busy total -> object
      [ "kind" .= ("SlotsChanged" :: Text), "busy" .= busy, "total" .= total ]
    PayloadLinking u tgt -> object
      [ "kind" .= ("Linking" :: Text), "unit" .= u, "target" .= tgt ]
    PayloadBuildDone outcome counts timeNs -> object
      [ "kind" .= ("BuildDone" :: Text), "outcome" .= outcome, "counts" .= counts, "wallTimeNs" .= timeNs ]

instance FromJSON EventPayload where
  parseJSON = withObject "EventPayload" $ \o -> do
    kind <- o .: "kind"
    case (kind :: Text) of
      "BuildStarted"   -> PayloadBuildStarted <$> (o .: "buildr" <|> o .: "buildrVersion")
                                              <*> (o .: "cabal" <|> o .: "cabalVersion")
                                              <*> (o .: "ghc" <|> o .: "ghcVersion")
      "PlanReady"      -> PayloadPlanReady <$> o .: "units"
      "GraphKnown"     -> PayloadGraphKnown <$> o .: "unit" <*> o .: "modules" <*> o .: "edges"
      "UnitStarted"    -> PayloadUnitStarted <$> o .: "unit"
      "UnitCompleted"  -> PayloadUnitCompleted <$> o .: "unit" <*> o .: "exitCode"
      "UnitFailed"     -> PayloadUnitFailed <$> o .: "unit" <*> o .: "exitCode" <*> o .:? "log"
      "UnitStopped"    -> PayloadUnitStopped <$> o .: "unit"
      "ModuleStarted"  -> PayloadModuleStarted <$> o .: "unit" <*> o .: "module" <*> o .: "k" <*> o .: "n"
                            <*> (o .: "source" <|> o .: "sourcePath" <|> o .: "srcPath" <|> o .: "src")
                            <*> o .: "obj" <*> o .:? "reason"
      "ModulePhase"    -> PayloadModulePhase <$> o .: "unit" <*> o .: "module" <*> o .: "phase"
                            <*> o .: "ms" <*> o .: "alloc"
      "ModuleFinished" -> PayloadModuleFinished <$> o .: "unit" <*> o .: "module"
      "ModuleSkipped"  -> PayloadModuleSkipped <$> o .: "unit" <*> o .: "module" <*> o .: "reason"
      "Diagnostic"     -> PayloadDiagnostic <$> o .:? "unit" <*> (o .: "diag" <|> o .: "diagnostic")
      "Opaque"         -> PayloadOpaque <$> o .:? "unit" <*> o .: "stream" <*> o .: "lines" <*> o .: "class"
      "SlotsChanged"   -> PayloadSlotsChanged <$> o .: "busy" <*> o .: "total"
      "Linking"        -> PayloadLinking <$> o .: "unit" <*> o .: "target"
      "BuildDone"      -> PayloadBuildDone <$> o .: "outcome" <*> o .: "counts" <*> (o .: "wallTimeNs" <|> o .: "wallNs")
      other            -> fail ("Unrecognised event kind: " <> T.unpack other)

-- | The canonical Event Protocol v1 envelope.
data Event = Event
  { eventVersion   :: !ProtocolVersion
  , eventSeq       :: !Int64
  , eventTimestamp :: !Int64
  , eventSource    :: !EventSource
  , eventValKind   :: !ValueKind
  , eventPayload   :: !EventPayload
  } deriving (Eq, Show, Generic)

instance ToJSON Event where
  toJSON ev =
    let payloadVal = toJSON (eventPayload ev)
        envelopePairs =
          [ "v"         .= eventVersion ev
          , "seq"       .= eventSeq ev
          , "t"         .= eventTimestamp ev
          , "src"       .= eventSource ev
          , "valueKind" .= eventValKind ev
          ]
    in case payloadVal of
         Aeson.Object payloadMap ->
           case object envelopePairs of
             Aeson.Object envMap -> Aeson.Object (envMap <> payloadMap)
             _                   -> payloadVal
         _ -> object envelopePairs

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    v <- o .: "v"
    seq' <- o .: "seq"
    t <- o .: "t"
    src <- o .: "src"
    vk <- o .: "valueKind"
    payload <- parseJSON (Aeson.Object o)
    pure Event
      { eventVersion   = v
      , eventSeq       = seq'
      , eventTimestamp = t
      , eventSource    = src
      , eventValKind   = vk
      , eventPayload   = payload
      }

-- | Constructs a valid Event envelope stamping Protocol Version 1.
mkEvent :: Int64 -> Int64 -> EventSource -> ValueKind -> EventPayload -> Event
mkEvent s t src vk payload = Event
  { eventVersion   = currentProtocolVersion
  , eventSeq       = s
  , eventTimestamp = t
  , eventSource    = src
  , eventValKind   = vk
  , eventPayload   = payload
  }

-- | Serializes an Event to a single JSON line without internal newlines.
encodeEventLine :: Event -> BS.ByteString
encodeEventLine ev = BSL.toStrict (encode ev) <> "\n"

-- | Deserializes a single JSON line into an Event.
decodeEventLine :: BS.ByteString -> Maybe Event
decodeEventLine = decodeStrict

-- | Purely parses a GHC -ddump-timings line into (phaseName, ms, allocBytes) (§22 G4).
parseTimingLine :: Text -> Maybe (Text, Int64, Int64)
parseTimingLine line =
  let ws = T.words line
      findPref pref = case filter (T.isPrefixOf pref) ws of
        (w:_) -> Just (T.drop (T.length pref) w)
        []    -> Nothing
  in case (findPref "alloc=", findPref "time=") of
       (Just allocTxt, Just timeTxt) ->
         case (readMaybe (T.unpack allocTxt) :: Maybe Int64, readMaybe (T.unpack timeTxt) :: Maybe Double) of
           (Just alloc, Just timeD) ->
             let phaseWords = takeWhile (\w -> not (T.isPrefixOf "alloc=" w)) ws
                 rawPhase = T.unwords phaseWords
                 cleanPhase = T.dropWhileEnd (== ':') (T.takeWhile (/= '[') rawPhase)
                 phaseName = if T.null (T.strip cleanPhase) then "phase" else T.strip cleanPhase
                 timeMs = max 1 (round timeD :: Int64)
             in Just (phaseName, timeMs, alloc)
           _ -> Nothing
       _ -> Nothing

-- | Purely parses Cabal stdout lifecycle lines to detect unit progress (§22 G5).
parseCabalLifecycleLine :: Text -> Maybe EventPayload
parseCabalLifecycleLine rawLine =
  let line = T.strip rawLine
      ws = T.words line
  in case ws of
       ("Starting" : pkg : _) ->
         Just (PayloadUnitStarted (cleanPkg pkg))
       ("Building" : "library" : "for" : pkg : _) ->
         Just (PayloadUnitStarted (cleanPkg pkg))
       ("Building" : "executable" : _exe : "for" : pkg : _) ->
         Just (PayloadUnitStarted (cleanPkg pkg))
       ("Building" : "test" : "suite" : _tst : "for" : pkg : _) ->
         Just (PayloadUnitStarted (cleanPkg pkg))
       ("Building" : pkg : _) ->
         Just (PayloadUnitStarted (cleanPkg pkg))
       ("Installing" : pkg : _) ->
         Just (PayloadUnitCompleted (cleanPkg pkg) 0)
       ("Completed" : pkg : _) ->
         Just (PayloadUnitCompleted (cleanPkg pkg) 0)
       ("Failed" : pkg : _) ->
         Just (PayloadUnitFailed (cleanPkg pkg) 1 Nothing)
       _ -> Nothing
  where
    cleanPkg p = T.dropWhileEnd (== '.') (T.takeWhile (/= '(') p)

-- | Intermediate representation for point coordinates in GHC JSON diagnostics.
data RawGhcPoint = RawGhcPoint
  { rgpLine   :: !(Maybe Int)
  , rgpColumn :: !(Maybe Int)
  } deriving (Eq, Show, Generic)

instance FromJSON RawGhcPoint where
  parseJSON = withObject "RawGhcPoint" $ \o ->
    RawGhcPoint <$> o .:? "line" <*> o .:? "column"

-- | Intermediate representation for source spans in GHC JSON diagnostics.
data RawGhcSpan = RawGhcSpan
  { rgsFile  :: !(Maybe FilePath)
  , rgsStart :: !(Maybe RawGhcPoint)
  , rgsEnd   :: !(Maybe RawGhcPoint)
  } deriving (Eq, Show, Generic)

instance FromJSON RawGhcSpan where
  parseJSON = withObject "RawGhcSpan" $ \o ->
    RawGhcSpan <$> o .:? "file" <*> o .:? "start" <*> o .:? "end"

-- | Raw GHC compiler diagnostic payload emitted under @-fdiagnostics-as-json@.
data RawGhcDiagnostic = RawGhcDiagnostic
  { rgdSeverity :: !DiagnosticSeverity
  , rgdCode     :: !(Maybe Int)
  , rgdSpan     :: !(Maybe RawGhcSpan)
  , rgdMessage  :: ![Text]
  , rgdHints    :: !(Maybe [Text])
  , rgdFlag     :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON RawGhcDiagnostic where
  parseJSON = withObject "RawGhcDiagnostic" $ \o -> do
    sev <- o .: "severity"
    code <- o .:? "code"
    mSpan <- o .:? "span"
    msgVal <- o .: "message"
    msgs <- case msgVal of
      Aeson.Array arr -> mapM parseJSON (toList arr)
      Aeson.String s  -> pure [s]
      _               -> pure []
    mHints <- o .:? "hints"
    mFlag <- o .:? "flag"
    pure RawGhcDiagnostic
      { rgdSeverity = sev
      , rgdCode     = code
      , rgdSpan     = mSpan
      , rgdMessage  = msgs
      , rgdHints    = mHints
      , rgdFlag     = mFlag
      }

-- | Purely translates a raw GHC JSON diagnostic into the canonical domain 'DiagnosticPayload'.
rawGhcToPayload :: RawGhcDiagnostic -> DiagnosticPayload
rawGhcToPayload raw =
  let (f, sLine, sCol, eLine, eCol) = case rgdSpan raw of
        Nothing -> ("<unknown>", 1, 1, 1, 1)
        Just sp ->
          let file = fromMaybe "<unknown>" (rgsFile sp)
              (sl, sc) = case rgsStart sp of
                Just pt -> (fromMaybe 1 (rgpLine pt), fromMaybe 1 (rgpColumn pt))
                Nothing -> (1, 1)
              (el, ec) = case rgsEnd sp of
                Just pt -> (fromMaybe sl (rgpLine pt), fromMaybe sc (rgpColumn pt))
                Nothing -> (sl, sc)
          in (file, sl, sc, el, ec)
      hints = fromMaybe [] (rgdHints raw)
  in DiagnosticPayload
       { diagSeverity = rgdSeverity raw
       , diagCode     = rgdCode raw
       , diagSpan     = SourceSpan f sLine sCol eLine eCol
       , diagMessage  = rgdMessage raw
       , diagHints    = hints
       , diagFlag     = rgdFlag raw
       }

-- | Purely parses a raw GHC JSON diagnostic line emitted under @-fdiagnostics-as-json@ (§13, §22).
-- Accepts both official GHC 9.10 JSON format and buildr's internal 'DiagnosticPayload' JSON.
parseGhcDiagnosticJson :: Text -> Maybe DiagnosticPayload
parseGhcDiagnosticJson txt =
  let bs = TE.encodeUtf8 (T.strip txt)
  in case decodeStrict bs :: Maybe DiagnosticPayload of
       Just dp -> Just dp
       Nothing -> case decodeStrict bs :: Maybe RawGhcDiagnostic of
         Just raw -> Just (rawGhcToPayload raw)
         Nothing  -> Nothing

-- | Diagnostic layout formatting style (§13, §18).
data ErrorStyle
  = ErrorClassic
  | ErrorFriendly
  deriving (Eq, Show, Generic)

instance FromJSON ErrorStyle
instance ToJSON ErrorStyle

-- | Formats a GHC diagnostic according to Visual Grammar v1 and §13 using classic style:
-- Header with clickable location, severity, error code, warning flag, verbatim message lines, and derived doc links.
formatDiagnostic :: DiagnosticPayload -> Text
formatDiagnostic = formatDiagnosticWithStyle ErrorClassic

-- | Formats a GHC diagnostic using the requested 'ErrorStyle' (§13, §18).
-- The friendly layout rearranges GHC's fields with clear sectioning without ever rewording message content.
formatDiagnosticWithStyle :: ErrorStyle -> DiagnosticPayload -> Text
formatDiagnosticWithStyle ErrorClassic diag =
  let span' = diagSpan diag
      loc = T.pack (spanFile span') <> ":" <> T.pack (show (spanStartLine span')) <> ":" <> T.pack (show (spanStartCol span'))
      sevStr = case diagSeverity diag of
        SeverityError   -> "error"
        SeverityWarning -> "warning"
        SeverityInfo    -> "info"
      codeStr = case diagCode diag of
        Just c  -> " [GHC-" <> T.pack (show c) <> "]"
        Nothing -> ""
      flagStr = case diagFlag diag of
        Just f  -> " [" <> f <> "]"
        Nothing -> ""
      header = loc <> ": " <> sevStr <> ":" <> codeStr <> flagStr
      msgLines = map ("    " <>) (diagMessage diag)
      hintLines = map (\h -> "    • Help: " <> h) (diagHints diag)
      docLine = case diagCode diag of
        Just c  -> ["    • Documentation: https://errors.haskell.org/messages/GHC-" <> T.pack (show c) <> " (derived)"]
        Nothing -> []
  in T.unlines ([header] <> msgLines <> hintLines <> docLine)
formatDiagnosticWithStyle ErrorFriendly diag =
  let span' = diagSpan diag
      sevStr = case diagSeverity diag of
        SeverityError   -> "ERROR"
        SeverityWarning -> "WARNING"
        SeverityInfo    -> "INFO"
      codeStr = case diagCode diag of
        Just c  -> " [GHC-" <> T.pack (show c) <> "]"
        Nothing -> ""
      flagStr = case diagFlag diag of
        Just f  -> " [" <> f <> "]"
        Nothing -> ""
      header = "┌─ " <> sevStr <> codeStr <> flagStr <> "\n"
            <> "│  Location: " <> T.pack (spanFile span') <> ":" <> T.pack (show (spanStartLine span')) <> ":" <> T.pack (show (spanStartCol span')) <> "\n"
            <> "│"
      msgLines = map ("│  " <>) (diagMessage diag)
      hintLines = if null (diagHints diag)
                    then []
                    else "│" : map (\h -> "│  • Suggestion: " <> h) (diagHints diag)
      docLine = case diagCode diag of
        Just c  -> ["│  • Error docs: https://errors.haskell.org/messages/GHC-" <> T.pack (show c) <> " (derived)"]
        Nothing -> []
      footer = "└─────────────────────────────────────────────────────────────"
  in T.unlines ([header] <> msgLines <> hintLines <> docLine <> [footer])
