{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : Buildr.Theme
-- Description : Terminal visual theme contract, semantic roles, contrast checks, and Rasika palette.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Published Theme specification complying with §17 of the Vision Document:
--   * Semantic roles for States, Phases, Diagnostics, and Chrome.
--   * Distinction without colour: themes cannot remove glyph or position differences.
--   * WCAG 2.1 relative luminance and 4.5:1 minimum contrast verification.
--   * Built-in Rasika theme and Plain monochrome fallback.
module Buildr.Theme
  ( -- * Color Representation
    RgbColor (..)
  , hexColor
  , rgbToAnsi
  , rgbLuminance
  , contrastRatio
    -- * Theme Specification
  , SemanticRole (..)
  , Theme (..)
    -- * Built-in Themes
  , rasikaTheme
  , plainTheme
  , builtinThemes
  , getThemeByName
    -- * Verification & Preview
  , checkThemeContrast
  , previewTheme
  ) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Char (digitToInt, isHexDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Numeric (showHex)

-- | Truecolor 24-bit RGB color.
data RgbColor = RgbColor
  { rgbRed   :: !Int
  , rgbGreen :: !Int
  , rgbBlue  :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON RgbColor
instance FromJSON RgbColor

-- | Safely parses a hex color string (e.g. \"#A3BE8C\" or \"#2E3440\").
hexColor :: Text -> Maybe RgbColor
hexColor txt =
  let clean = T.unpack (fromMaybe txt (T.stripPrefix "#" txt))
  in if length clean == 6 && all isHexDigit clean
       then case map digitToInt clean of
              [r1, r2, g1, g2, b1, b2] ->
                Just (RgbColor (r1 * 16 + r2) (g1 * 16 + g2) (b1 * 16 + b2))
              _ -> Nothing
       else Nothing

-- | Formats RGB color as VT truecolor escape sequence for foreground text.
rgbToAnsi :: RgbColor -> Text
rgbToAnsi (RgbColor r g b) =
  "\ESC[38;2;" <> T.pack (show r) <> ";" <> T.pack (show g) <> ";" <> T.pack (show b) <> "m"

-- | Computes WCAG 2.1 relative luminance for an sRGB component.
channelLuminance :: Int -> Double
channelLuminance c =
  let s = fromIntegral (max 0 (min 255 c)) / 255.0
  in if s <= 0.04045
       then s / 12.92
       else ((s + 0.055) / 1.055) ** 2.4

-- | Computes the relative luminance of an RGB color (0.0 to 1.0).
rgbLuminance :: RgbColor -> Double
rgbLuminance (RgbColor r g b) =
  0.2126 * channelLuminance r + 0.7152 * channelLuminance g + 0.0722 * channelLuminance b

-- | Computes WCAG 2.1 contrast ratio between two colors: (L1 + 0.05) / (L2 + 0.05) >= 1.0.
contrastRatio :: RgbColor -> RgbColor -> Double
contrastRatio c1 c2 =
  let l1 = rgbLuminance c1
      l2 = rgbLuminance c2
      bright = max l1 l2
      dark = min l1 l2
  in (bright + 0.05) / (dark + 0.05)

-- | Semantic roles partitioned per §17.
data SemanticRole
  -- States
  = RoleStateDone
  | RoleStateDoneDim
  | RoleStateCompiling
  | RoleStateOverrun
  | RoleStateWaiting
  | RoleStateSkipped
  | RoleStateFailed
  | RoleStateBlocked
  | RoleStateNotReached
  -- Phases
  | RolePhaseParse
  | RolePhaseTypecheck
  | RolePhaseSimplify
  | RolePhaseCodegen
  | RolePhaseOther
  -- Diagnostics
  | RoleDiagError
  | RoleDiagWarning
  | RoleDiagAdvice
  | RoleDiagLabelPrimary
  | RoleDiagLabelSecondary
  | RoleDiagCode
  | RoleDiagFlag
  | RoleDiagHelp
  | RoleDiagLink
  | RoleDiagGutter
  | RoleDiagFrame
  | RoleDiagFilename
  -- Chrome
  | RoleChromeText
  | RoleChromeTextStrong
  | RoleChromeMuted
  | RoleChromeMutedStrong
  | RoleChromeAccent
  | RoleChromeBackground
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SemanticRole
instance FromJSON SemanticRole
instance ToJSONKey SemanticRole
instance FromJSONKey SemanticRole

-- | A complete terminal theme with semantic role color assignments.
data Theme = Theme
  { themeName        :: !Text
  , themeDescription :: !Text
  , themeBackground  :: !RgbColor
  , themePalette     :: !(Map SemanticRole RgbColor)
  } deriving (Eq, Show, Generic)

instance ToJSON Theme
instance FromJSON Theme

-- | The canonical Bombay-Boyz Rasika theme with rich dark background and distinctive accents.
rasikaTheme :: Theme
rasikaTheme = Theme
  { themeName        = "rasika"
  , themeDescription = "Bombay-Boyz Rasika: high-contrast dark theme with natural saturation"
  , themeBackground  = RgbColor 24 26 31       -- #181A1F
  , themePalette     = Map.fromList
      [ (RoleStateDone,          RgbColor 152 195 121) -- #98C379 soft green
      , (RoleStateDoneDim,       RgbColor 92 118 73)   -- dim green
      , (RoleStateCompiling,     RgbColor 97 175 239)  -- #61AFEF cyan blue
      , (RoleStateOverrun,       RgbColor 229 192 123) -- #E5C07B amber gold
      , (RoleStateWaiting,       RgbColor 171 178 191) -- #ABB2BF light grey
      , (RoleStateSkipped,       RgbColor 198 120 221) -- #C678DD purple
      , (RoleStateFailed,        RgbColor 224 108 117) -- #E06C75 crimson red
      , (RoleStateBlocked,       RgbColor 209 154 102) -- #D19A66 orange
      , (RoleStateNotReached,    RgbColor 92 99 112)   -- #5C6370 slate grey

      , (RolePhaseParse,         RgbColor 86 182 194)  -- teal
      , (RolePhaseTypecheck,     RgbColor 97 175 239)  -- sky blue
      , (RolePhaseSimplify,      RgbColor 198 120 221) -- violet
      , (RolePhaseCodegen,       RgbColor 229 192 123) -- amber
      , (RolePhaseOther,         RgbColor 171 178 191) -- grey

      , (RoleDiagError,          RgbColor 224 108 117)
      , (RoleDiagWarning,        RgbColor 229 192 123)
      , (RoleDiagAdvice,         RgbColor 86 182 194)
      , (RoleDiagLabelPrimary,   RgbColor 255 255 255)
      , (RoleDiagLabelSecondary, RgbColor 171 178 191)
      , (RoleDiagCode,           RgbColor 224 108 117)
      , (RoleDiagFlag,           RgbColor 92 99 112)
      , (RoleDiagHelp,           RgbColor 152 195 121)
      , (RoleDiagLink,           RgbColor 97 175 239)
      , (RoleDiagGutter,         RgbColor 75 82 99)
      , (RoleDiagFrame,          RgbColor 62 68 82)
      , (RoleDiagFilename,       RgbColor 229 192 123)

      , (RoleChromeText,         RgbColor 220 223 228) -- near white
      , (RoleChromeTextStrong,   RgbColor 255 255 255)
      , (RoleChromeMuted,        RgbColor 150 155 165)
      , (RoleChromeMutedStrong,  RgbColor 110 115 125)
      , (RoleChromeAccent,       RgbColor 97 175 239)
      , (RoleChromeBackground,   RgbColor 24 26 31)
      ]
  }

-- | Plain monochrome theme ensuring distinction without color.
plainTheme :: Theme
plainTheme = Theme
  { themeName        = "plain"
  , themeDescription = "Monochrome plain theme for NO_COLOR and CI pipelines"
  , themeBackground  = RgbColor 0 0 0
  , themePalette     = Map.fromList
      [ (RoleStateDone,          RgbColor 255 255 255)
      , (RoleStateDoneDim,       RgbColor 180 180 180)
      , (RoleStateCompiling,     RgbColor 255 255 255)
      , (RoleStateOverrun,       RgbColor 255 255 255)
      , (RoleStateWaiting,       RgbColor 180 180 180)
      , (RoleStateSkipped,       RgbColor 180 180 180)
      , (RoleStateFailed,        RgbColor 255 255 255)
      , (RoleStateBlocked,       RgbColor 180 180 180)
      , (RoleStateNotReached,    RgbColor 140 140 140)

      , (RolePhaseParse,         RgbColor 255 255 255)
      , (RolePhaseTypecheck,     RgbColor 255 255 255)
      , (RolePhaseSimplify,      RgbColor 255 255 255)
      , (RolePhaseCodegen,       RgbColor 255 255 255)
      , (RolePhaseOther,         RgbColor 180 180 180)

      , (RoleDiagError,          RgbColor 255 255 255)
      , (RoleDiagWarning,        RgbColor 220 220 220)
      , (RoleDiagAdvice,         RgbColor 200 200 200)
      , (RoleDiagLabelPrimary,   RgbColor 255 255 255)
      , (RoleDiagLabelSecondary, RgbColor 180 180 180)
      , (RoleDiagCode,           RgbColor 255 255 255)
      , (RoleDiagFlag,           RgbColor 160 160 160)
      , (RoleDiagHelp,           RgbColor 220 220 220)
      , (RoleDiagLink,           RgbColor 220 220 220)
      , (RoleDiagGutter,         RgbColor 140 140 140)
      , (RoleDiagFrame,          RgbColor 100 100 100)
      , (RoleDiagFilename,       RgbColor 255 255 255)

      , (RoleChromeText,         RgbColor 255 255 255)
      , (RoleChromeTextStrong,   RgbColor 255 255 255)
      , (RoleChromeMuted,        RgbColor 180 180 180)
      , (RoleChromeMutedStrong,  RgbColor 140 140 140)
      , (RoleChromeAccent,       RgbColor 255 255 255)
      , (RoleChromeBackground,   RgbColor 0 0 0)
      ]
  }

-- | List of all registered built-in themes.
builtinThemes :: [Theme]
builtinThemes = [rasikaTheme, plainTheme]

-- | Finds a theme by canonical name.
getThemeByName :: Text -> Maybe Theme
getThemeByName name = case filter (\t -> themeName t == name) builtinThemes of
  (t:_) -> Just t
  []    -> Nothing

-- | Formats a role name cleanly.
roleLabel :: SemanticRole -> Text
roleLabel = \case
  RoleStateDone          -> "state.done"
  RoleStateDoneDim       -> "state.done-dim"
  RoleStateCompiling     -> "state.compiling"
  RoleStateOverrun       -> "state.overrun"
  RoleStateWaiting       -> "state.waiting"
  RoleStateSkipped       -> "state.skipped"
  RoleStateFailed        -> "state.failed"
  RoleStateBlocked       -> "state.blocked"
  RoleStateNotReached    -> "state.not-reached"
  RolePhaseParse         -> "phase.parse"
  RolePhaseTypecheck     -> "phase.typecheck"
  RolePhaseSimplify      -> "phase.simplify"
  RolePhaseCodegen       -> "phase.codegen"
  RolePhaseOther         -> "phase.other"
  RoleDiagError          -> "diag.error"
  RoleDiagWarning        -> "diag.warning"
  RoleDiagAdvice         -> "diag.advice"
  RoleDiagLabelPrimary   -> "diag.label-primary"
  RoleDiagLabelSecondary -> "diag.label-secondary"
  RoleDiagCode           -> "diag.code"
  RoleDiagFlag           -> "diag.flag"
  RoleDiagHelp           -> "diag.help"
  RoleDiagLink           -> "diag.link"
  RoleDiagGutter         -> "diag.gutter"
  RoleDiagFrame          -> "diag.frame"
  RoleDiagFilename       -> "diag.filename"
  RoleChromeText         -> "chrome.text"
  RoleChromeTextStrong   -> "chrome.text-strong"
  RoleChromeMuted        -> "chrome.muted"
  RoleChromeMutedStrong  -> "chrome.muted-strong"
  RoleChromeAccent       -> "chrome.accent"
  RoleChromeBackground   -> "chrome.background"

-- | Checks WCAG 2.1 contrast ratio for all text and label roles against theme background.
-- Per §17, required minimum contrast is 4.5:1 for readable text roles.
checkThemeContrast :: Theme -> [(Text, Double, Bool)]
checkThemeContrast theme =
  let bg = themeBackground theme
      textRoles =
        [ RoleChromeText
        , RoleChromeTextStrong
        , RoleDiagLabelPrimary
        , RoleDiagLabelSecondary
        , RoleStateDone
        , RoleStateCompiling
        , RoleStateOverrun
        , RoleStateFailed
        ]
      checkRole r =
        let color = Map.findWithDefault (RgbColor 255 255 255) r (themePalette theme)
            ratio = contrastRatio color bg
            passes = ratio >= 4.5
        in (roleLabel r, ratio, passes)
  in map checkRole textRoles

-- | Generates a terminal preview frame demonstrating all theme roles.
previewTheme :: Theme -> Text
previewTheme theme =
  let bg = themeBackground theme
      colorOf r = Map.findWithDefault (RgbColor 255 255 255) r (themePalette theme)
      paint r str = rgbToAnsi (colorOf r) <> str <> "\ESC[0m"
      hexOf (RgbColor r g b) =
        let pad s = if length s == 1 then "0" ++ s else s
        in "#" <> T.pack (pad (showHex r "") <> pad (showHex g "") <> pad (showHex b ""))
  in T.unlines
       [ "Theme: " <> themeName theme <> " (" <> themeDescription theme <> ")"
       , "Background: " <> hexOf bg
       , "--- States ---"
       , "  " <> paint RoleStateDone "● done" <> "       " <> paint RoleStateCompiling "◐ compiling"
       , "  " <> paint RoleStateOverrun "◐ +3s overrun" <> " " <> paint RoleStateWaiting "○ waiting"
       , "  " <> paint RoleStateSkipped "◇ skipped" <> "    " <> paint RoleStateFailed "✖ failed"
       , "  " <> paint RoleStateBlocked "⊘ blocked" <> "    " <> paint RoleStateNotReached "◌ not reached"
       , "--- Phases ---"
       , "  [" <> paint RolePhaseParse "parse" <> "|"
              <> paint RolePhaseTypecheck "typecheck" <> "|"
              <> paint RolePhaseSimplify "simplify" <> "|"
              <> paint RolePhaseCodegen "codegen" <> "]"
       , "--- Diagnostics ---"
       , "  " <> paint RoleDiagError "error [GHC-12345]" <> " in " <> paint RoleDiagFilename "src/Main.hs:12:4"
       , "  " <> paint RoleDiagWarning "warning [-Wunused-imports]" <> " " <> paint RoleDiagHelp "Consider removing"
       ]
