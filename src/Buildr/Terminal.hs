-- |
-- Module      : Buildr.Terminal
-- Description : Terminal dimensions, SIGWINCH resizing, capability detection, and advanced VT sequences.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Implements dynamic terminal dimension recalculation, SIGWINCH handling, Zen-view
-- collapse for small windows (< 10 rows), and detection/generation of modern VT sequences
-- (OSC 8 hyperlinks, SGR 4:3 undercurl, SGR 58 underline colour, and OSC 9;4 progress)
-- per §7, §19, and §20.
module Buildr.Terminal
  ( -- * Terminal Dimensions & Resizing
    TerminalDimensions (..)
  , defaultDimensions
  , getTerminalDimensions
  , effectiveViewMode
  , calculateDynamicBarWidth
  , truncateToCols
    -- * Terminal Capabilities
  , TerminalCaps (..)
  , defaultCaps
  , detectTerminalCaps
  , pureDetectCaps
    -- * Advanced VT Sequences
  , TaskbarState (..)
  , formatOsc8Link
  , formatFileHyperlink
  , formatDocHyperlink
  , formatUndercurl
  , formatOsc9Progress
    -- * Enhanced Diagnostic Rendering
  , formatDiagnosticWithCaps
  ) where

import Buildr.Event
  ( DiagnosticPayload (..)
  , DiagnosticSeverity (..)
  , ErrorStyle (..)
  , SourceSpan (..)
  )
import Buildr.Grammar (ViewMode (..))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Terminal.Size (Window (..))
import qualified System.Console.Terminal.Size as TS
import System.Environment (getEnvironment)
import Text.Read (readMaybe)

-- | Current terminal row and column count.
data TerminalDimensions = TerminalDimensions
  { tdRows :: !Int
  , tdCols :: !Int
  } deriving (Eq, Show)

-- | Sensible fallback terminal geometry (24 rows x 80 columns).
defaultDimensions :: TerminalDimensions
defaultDimensions = TerminalDimensions 24 80

-- | Reads current terminal dimensions dynamically via the ioctl/TIOCGWINSZ boundary.
getTerminalDimensions :: IO TerminalDimensions
getTerminalDimensions = do
  mWin <- (TS.size :: IO (Maybe (Window Int)))
  case mWin of
    Just (Window h w) -> pure (TerminalDimensions h w)
    Nothing           -> pure defaultDimensions

-- | Collapses rendering into the Zen view if terminal height drops below 10 rows (§19).
effectiveViewMode :: ViewMode -> TerminalDimensions -> ViewMode
effectiveViewMode activeMode dims
  | tdRows dims < 10 = ViewZen
  | otherwise        = activeMode

-- | Recalculates progress bar dimensions to match available terminal column width.
calculateDynamicBarWidth :: Int -> Int
calculateDynamicBarWidth cols
  | cols < 50  = 8
  | cols < 70  = 12
  | cols < 100 = 16
  | cols < 140 = 20
  | otherwise  = 28

-- | Truncates text cleanly with an ellipsis if it exceeds the specified terminal column width.
truncateToCols :: Int -> Text -> Text
truncateToCols cols txt
  | cols <= 0            = ""
  | T.length txt <= cols = txt
  | otherwise            = T.take (max 0 (cols - 1)) txt <> "…"

-- | Terminal emulator feature support matrix detected from environment and TERM capabilities.
data TerminalCaps = TerminalCaps
  { tcOsc8Hyperlinks :: !Bool  -- ^ OSC 8 clickable file:// and https:// links
  , tcUndercurl      :: !Bool  -- ^ SGR 4:3 wavy undercurl and SGR 58 color
  , tcOsc9Progress   :: !Bool  -- ^ OSC 9;4 taskbar/tab progress updates
  , tcTrueColor      :: !Bool  -- ^ 24-bit RGB truecolor support
  , tcColorEnabled   :: !Bool  -- ^ General ANSI escape sequence enablement
  } deriving (Eq, Show)

-- | Minimalist baseline capabilities with no advanced sequences.
defaultCaps :: TerminalCaps
defaultCaps = TerminalCaps False False False False True

-- | Detects terminal capabilities from environment variables.
detectTerminalCaps :: IO TerminalCaps
detectTerminalCaps = pureDetectCaps <$> getEnvironment

-- | Pure capability evaluation from an environment association list.
pureDetectCaps :: [(String, String)] -> TerminalCaps
pureDetectCaps env
  | hasEnv "NO_COLOR" = TerminalCaps False False False False False
  | otherwise =
      let termProg = getEnv "TERM_PROGRAM"
          term = getEnv "TERM"
          colorTerm = getEnv "COLORTERM"
          hasWt = hasEnv "WT_SESSION"
          vteVer = parseVte (getEnv "VTE_VERSION")
          isGhostty = termProg `elem` ["ghostty", "Ghostty"] || term == "xterm-ghostty"
          isIterm = termProg `elem` ["iTerm.app", "WezTerm"]
          isVscode = termProg == "vscode"
          isKitty = term == "xterm-kitty"
          isModernVte = vteVer >= 5000
          hasTrueColor = colorTerm `elem` ["truecolor", "24bit"] || isGhostty || isKitty || isIterm || hasWt

          supportsOsc8 = isGhostty || isIterm || isVscode || isKitty || hasWt || isModernVte || termProg == "alacritty"
          supportsUndercurl = isGhostty || isIterm || isKitty || isVscode || hasWt || isModernVte
          supportsOsc9 = isGhostty || isIterm || hasWt || isVscode
      in TerminalCaps
           { tcOsc8Hyperlinks = supportsOsc8
           , tcUndercurl      = supportsUndercurl
           , tcOsc9Progress   = supportsOsc9
           , tcTrueColor      = hasTrueColor
           , tcColorEnabled   = True
           }
  where
    getEnv k = case lookup k env of
      Just v  -> v
      Nothing -> ""
    hasEnv k = case lookup k env of
      Just v  -> not (null v)
      Nothing -> False
    parseVte str =
      let digits = takeWhile isDigit str
      in if null digits then 0 else fromMaybe 0 (readMaybe digits :: Maybe Int)

-- | Generates an OSC 8 clickable hyperlink sequence.
formatOsc8Link :: Text -> Text -> Text
formatOsc8Link url text =
  "\ESC]8;;" <> url <> "\ESC\\" <> text <> "\ESC]8;;\ESC\\"

-- | Formats a clickable file URL hyperlink if supported by the terminal emulator.
formatFileHyperlink :: TerminalCaps -> FilePath -> Text -> Text
formatFileHyperlink caps path label
  | tcOsc8Hyperlinks caps = formatOsc8Link ("file://" <> T.pack path) label
  | otherwise             = label

-- | Formats a clickable documentation URL hyperlink if supported.
formatDocHyperlink :: TerminalCaps -> Text -> Text
formatDocHyperlink caps url
  | tcOsc8Hyperlinks caps = formatOsc8Link url url
  | otherwise             = url

-- | Emits SGR 4:3 undercurl with SGR 58 underline RGB colour if supported.
formatUndercurl :: TerminalCaps -> (Int, Int, Int) -> Text -> Text
formatUndercurl caps (r, g, b) text
  | not (tcColorEnabled caps) = text
  | tcUndercurl caps =
      let rS = T.pack (show r)
          gS = T.pack (show g)
          bS = T.pack (show b)
      in "\ESC[4:3m\ESC[58;2;" <> rS <> ";" <> gS <> ";" <> bS <> "m" <> text <> "\ESC[58;58m\ESC[4:0m"
  | otherwise = "\ESC[4m" <> text <> "\ESC[24m"

-- | Taskbar/tab progress states for modern terminal emulators (OSC 9;4).
data TaskbarState
  = TaskbarClear
  | TaskbarNormal
  | TaskbarError
  | TaskbarIndeterminate
  | TaskbarPaused
  deriving (Eq, Show)

-- | Generates an OSC 9;4 progress sequence to stream taskbar/window tab progress state.
formatOsc9Progress :: TerminalCaps -> TaskbarState -> Int -> Text
formatOsc9Progress caps st pct
  | not (tcOsc9Progress caps) = ""
  | otherwise =
      let clamped = max 0 (min 100 pct)
          cmdCode = case st of
            TaskbarClear         -> "0;0"
            TaskbarNormal        -> "1;" <> T.pack (show clamped)
            TaskbarError         -> "2;" <> T.pack (show clamped)
            TaskbarIndeterminate -> "3;0"
            TaskbarPaused        -> "4;" <> T.pack (show clamped)
      in "\ESC]9;4;" <> cmdCode <> "\ESC\\"

-- | Formats a diagnostic using detected terminal capabilities for hyperlinks and undercurls.
formatDiagnosticWithCaps :: TerminalCaps -> ErrorStyle -> DiagnosticPayload -> Text
formatDiagnosticWithCaps caps style diag =
  let span' = diagSpan diag
      rawLoc = T.pack (spanFile span') <> ":" <> T.pack (show (spanStartLine span')) <> ":" <> T.pack (show (spanStartCol span'))
      loc = formatFileHyperlink caps (spanFile span') rawLoc

      sevStr = case diagSeverity diag of
        SeverityError   -> formatUndercurl caps (255, 95, 95) (if style == ErrorFriendly then "ERROR" else "error")
        SeverityWarning -> formatUndercurl caps (255, 175, 0) (if style == ErrorFriendly then "WARNING" else "warning")
        SeverityInfo    -> if style == ErrorFriendly then "INFO" else "info"

      codeStr = case diagCode diag of
        Just c  -> " [GHC-" <> T.pack (show c) <> "]"
        Nothing -> ""
      flagStr = case diagFlag diag of
        Just f  -> " [" <> f <> "]"
        Nothing -> ""

      docUrl c = "https://errors.haskell.org/messages/GHC-" <> T.pack (show c)
      docLink c = formatDocHyperlink caps (docUrl c)
  in case style of
       ErrorClassic ->
         let header = loc <> ": " <> sevStr <> ":" <> codeStr <> flagStr
             msgLines = map ("    " <>) (diagMessage diag)
             hintLines = map (\h -> "    • Help: " <> h) (diagHints diag)
             docLine = case diagCode diag of
               Just c  -> ["    • Documentation: " <> docLink c <> " (derived)"]
               Nothing -> []
         in T.unlines ([header] <> msgLines <> hintLines <> docLine)
       ErrorFriendly ->
         let header = "┌─ " <> sevStr <> codeStr <> flagStr <> "\n"
                   <> "│  Location: " <> loc <> "\n"
                   <> "│"
             msgLines = map ("│  " <>) (diagMessage diag)
             hintLines = if null (diagHints diag)
                           then []
                           else "│" : map (\h -> "│  • Suggestion: " <> h) (diagHints diag)
             docLine = case diagCode diag of
               Just c  -> ["│  • Error docs: " <> docLink c <> " (derived)"]
               Nothing -> []
             footer = "└─────────────────────────────────────────────────────────────"
         in T.unlines ([header] <> msgLines <> hintLines <> docLine <> [footer])
