{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Buildr.Editor
-- Description : Editor and LSP Event Consumer Interface based on Event Protocol v1.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Published Editor & LSP Protocol interface complying with Phase 3 of the Roadmap (§23)
-- and the non-goal contract (§25: "Being an IDE or LSP. buildr provides events to them").
--
-- Provides:
--   * Full Language Server Protocol (LSP 3.17) diagnostic models (0-based lines/columns).
--   * WorkDoneProgress tokens and notifications for build lifecycle tracking.
--   * Lossless translation from Event Protocol v1 payloads into editor-consumable events.
--   * Streaming JSON Lines codecs for IDE plugins (VS Code, Neovim, Helix, Emacs).
module Buildr.Editor
  ( -- * LSP 3.17 Position & Range
    LspPosition (..)
  , LspRange (..)
  , spanToLspRange
    -- * LSP Diagnostic Representation
  , LspSeverity (..)
  , LspDiagnosticCode (..)
  , LspDiagnostic (..)
  , LspPublishDiagnosticsParams (..)
  , diagnosticToLsp
    -- * LSP WorkDoneProgress Notifications
  , LspWorkDoneProgress (..)
    -- * Editor Event Envelope
  , EditorMessage (..)
  , eventToEditorMessages
  , encodeEditorMessageLine
  , decodeEditorMessageLine
  ) where

import Buildr.Event
  ( BuildOutcome (..)
  , DiagnosticPayload (..)
  , DiagnosticSeverity (..)
  , Event (..)
  , EventPayload (..)
  , SourceSpan (..)
  )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | 0-indexed position within a text document conforming to LSP specification.
data LspPosition = LspPosition
  { lpLine      :: !Int  -- ^ Zero-based line number
  , lpCharacter :: !Int  -- ^ Zero-based character offset
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON LspPosition where
  toJSON p = Aeson.object
    [ "line"      .= lpLine p
    , "character" .= lpCharacter p
    ]

instance Aeson.FromJSON LspPosition where
  parseJSON = Aeson.withObject "LspPosition" $ \o ->
    LspPosition <$> o .: "line" <*> o .: "character"

-- | Text document range expressed via start and end positions.
data LspRange = LspRange
  { lrStart :: !LspPosition
  , lrEnd   :: !LspPosition
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON LspRange where
  toJSON r = Aeson.object
    [ "start" .= lrStart r
    , "end"   .= lrEnd r
    ]

instance Aeson.FromJSON LspRange where
  parseJSON = Aeson.withObject "LspRange" $ \o ->
    LspRange <$> o .: "start" <*> o .: "end"

-- | Converts a 1-based buildr 'SourceSpan' into a 0-based LSP 'LspRange'.
spanToLspRange :: SourceSpan -> LspRange
spanToLspRange sp = LspRange
  { lrStart = LspPosition
      { lpLine      = max 0 (spanStartLine sp - 1)
      , lpCharacter = max 0 (spanStartCol sp - 1)
      }
  , lrEnd   = LspPosition
      { lpLine      = max 0 (spanEndLine sp - 1)
      , lpCharacter = max 0 (spanEndCol sp - 1)
      }
  }

-- | Standard LSP DiagnosticSeverity values (1: Error, 2: Warning, 3: Information, 4: Hint).
data LspSeverity
  = LspSeverityError
  | LspSeverityWarning
  | LspSeverityInformation
  | LspSeverityHint
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON LspSeverity where
  toJSON = \case
    LspSeverityError       -> Aeson.toJSON (1 :: Int)
    LspSeverityWarning     -> Aeson.toJSON (2 :: Int)
    LspSeverityInformation -> Aeson.toJSON (3 :: Int)
    LspSeverityHint        -> Aeson.toJSON (4 :: Int)

instance Aeson.FromJSON LspSeverity where
  parseJSON v = do
    code <- Aeson.parseJSON v
    case (code :: Int) of
      1 -> pure LspSeverityError
      2 -> pure LspSeverityWarning
      3 -> pure LspSeverityInformation
      4 -> pure LspSeverityHint
      _ -> fail ("Unknown LSP DiagnosticSeverity: " <> show code)

-- | LSP diagnostic code: integer or string identifier.
data LspDiagnosticCode
  = LspCodeInt !Int
  | LspCodeText !Text
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON LspDiagnosticCode where
  toJSON = \case
    LspCodeInt n  -> Aeson.toJSON n
    LspCodeText t -> Aeson.toJSON t

instance Aeson.FromJSON LspDiagnosticCode where
  parseJSON v =
    (LspCodeInt <$> Aeson.parseJSON v)
    <|> (LspCodeText <$> Aeson.parseJSON v)

-- | Standard LSP Diagnostic record.
data LspDiagnostic = LspDiagnostic
  { ldRange    :: !LspRange
  , ldSeverity :: !LspSeverity
  , ldCode     :: !(Maybe LspDiagnosticCode)
  , ldSource   :: !Text
  , ldMessage  :: !Text
  , ldHints    :: ![Text]
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON LspDiagnostic where
  toJSON d = Aeson.object
    [ "range"    .= ldRange d
    , "severity" .= ldSeverity d
    , "code"     .= ldCode d
    , "source"   .= ldSource d
    , "message"  .= ldMessage d
    , "hints"    .= ldHints d
    ]

instance Aeson.FromJSON LspDiagnostic where
  parseJSON = Aeson.withObject "LspDiagnostic" $ \o ->
    LspDiagnostic
      <$> o .: "range"
      <*> o .: "severity"
      <*> o .:? "code"
      <*> o .: "source"
      <*> o .: "message"
      <*> o .: "hints"

-- | Standard LSP textDocument/publishDiagnostics notification parameter.
data LspPublishDiagnosticsParams = LspPublishDiagnosticsParams
  { lpUri         :: !Text
  , lpDiagnostics :: ![LspDiagnostic]
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON LspPublishDiagnosticsParams where
  toJSON p = Aeson.object
    [ "uri"         .= lpUri p
    , "diagnostics" .= lpDiagnostics p
    ]

instance Aeson.FromJSON LspPublishDiagnosticsParams where
  parseJSON = Aeson.withObject "LspPublishDiagnosticsParams" $ \o ->
    LspPublishDiagnosticsParams <$> o .: "uri" <*> o .: "diagnostics"

-- | Translates a compiler 'DiagnosticPayload' into a standard 'LspDiagnostic'.
diagnosticToLsp :: DiagnosticPayload -> LspDiagnostic
diagnosticToLsp diag = LspDiagnostic
  { ldRange    = spanToLspRange (diagSpan diag)
  , ldSeverity = case diagSeverity diag of
      SeverityError   -> LspSeverityError
      SeverityWarning -> LspSeverityWarning
      SeverityInfo    -> LspSeverityInformation
  , ldCode     = fmap LspCodeInt (diagCode diag)
  , ldSource   = "ghc"
  , ldMessage  = T.intercalate "\n" (diagMessage diag)
  , ldHints    = diagHints diag
  }

-- | LSP $/progress work done notifications for IDE build indicators.
data LspWorkDoneProgress
  = LspProgressBegin !Text !Bool !(Maybe Int)
  | LspProgressReport !(Maybe Int) !(Maybe Text)
  | LspProgressEnd !(Maybe Text)
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON LspWorkDoneProgress where
  toJSON = \case
    LspProgressBegin title canc pct -> Aeson.object
      [ "kind"        .= ("begin" :: Text)
      , "title"       .= title
      , "cancellable" .= canc
      , "percentage"  .= pct
      ]
    LspProgressReport pct msg -> Aeson.object
      [ "kind"        .= ("report" :: Text)
      , "percentage"  .= pct
      , "message"     .= msg
      ]
    LspProgressEnd msg -> Aeson.object
      [ "kind"    .= ("end" :: Text)
      , "message" .= msg
      ]

instance Aeson.FromJSON LspWorkDoneProgress where
  parseJSON = Aeson.withObject "LspWorkDoneProgress" $ \o -> do
    kind <- o .: "kind"
    case (kind :: Text) of
      "begin"  -> LspProgressBegin <$> o .: "title" <*> o .: "cancellable" <*> o .:? "percentage"
      "report" -> LspProgressReport <$> o .:? "percentage" <*> o .:? "message"
      "end"    -> LspProgressEnd <$> o .:? "message"
      other    -> fail ("Unknown workDoneProgress kind: " <> T.unpack other)

-- | Top-level message emitted by the editor protocol interface.
data EditorMessage
  = EditorPublishDiagnostics !LspPublishDiagnosticsParams
  | EditorProgress !Text !LspWorkDoneProgress
  | EditorBuildOutcome !BuildOutcome !Int64
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON EditorMessage where
  toJSON = \case
    EditorPublishDiagnostics p -> Aeson.object
      [ "method" .= ("textDocument/publishDiagnostics" :: Text)
      , "params" .= p
      ]
    EditorProgress token prog -> Aeson.object
      [ "method" .= ("$/progress" :: Text)
      , "token"  .= token
      , "value"  .= prog
      ]
    EditorBuildOutcome outcome wallNs -> Aeson.object
      [ "method"  .= ("buildr/buildOutcome" :: Text)
      , "outcome" .= outcome
      , "wallNs"  .= wallNs
      ]

instance Aeson.FromJSON EditorMessage where
  parseJSON = Aeson.withObject "EditorMessage" $ \o -> do
    method <- o .: "method"
    case (method :: Text) of
      "textDocument/publishDiagnostics" -> EditorPublishDiagnostics <$> o .: "params"
      "$/progress"                      -> EditorProgress <$> o .: "token" <*> o .: "value"
      "buildr/buildOutcome"             -> EditorBuildOutcome <$> o .: "outcome" <*> o .: "wallNs"
      other                             -> fail ("Unknown editor message method: " <> T.unpack other)

-- | Translates a generic Event Protocol v1 record into corresponding editor notifications.
eventToEditorMessages :: Event -> [EditorMessage]
eventToEditorMessages ev = case eventPayload ev of
  PayloadDiagnostic _ diag ->
    let uri = "file://" <> T.pack (spanFile (diagSpan diag))
        lspDiag = diagnosticToLsp diag
        params = LspPublishDiagnosticsParams uri [lspDiag]
    in [EditorPublishDiagnostics params]

  PayloadBuildStarted bVer cVer gVer ->
    let title = "Haskell Build (" <> gVer <> ", Cabal " <> cVer <> ", buildr " <> bVer <> ")"
        prog = LspProgressBegin title True (Just 0)
    in [EditorProgress "buildr-build" prog]

  PayloadUnitCompleted uId code ->
    let msg = "Completed unit: " <> uId <> " (exit " <> T.pack (show code) <> ")"
        prog = LspProgressReport Nothing (Just msg)
    in [EditorProgress "buildr-build" prog]

  PayloadBuildDone outcome _ wallNs ->
    let outMsg = case outcome of
          BuildSuccess   -> "Build finished successfully"
          BuildFailed    -> "Build failed"
          BuildCancelled -> "Build cancelled"
        prog = LspProgressEnd (Just outMsg)
    in [ EditorProgress "buildr-build" prog
       , EditorBuildOutcome outcome wallNs
       ]

  _ -> []

-- | Encodes an EditorMessage into a JSON Lines byte sequence.
encodeEditorMessageLine :: EditorMessage -> BS.ByteString
encodeEditorMessageLine msg = BSL.toStrict (Aeson.encode msg) <> "\n"

-- | Decodes an EditorMessage from a JSON Lines byte sequence.
decodeEditorMessageLine :: BS.ByteString -> Maybe EditorMessage
decodeEditorMessageLine = Aeson.decodeStrict
