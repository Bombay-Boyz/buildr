-- |
-- Module      : Buildr.Grammar
-- Description : Visual Grammar v1 specification for states, glyphs, and 20-cell progress bars.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Published Visual Grammar v1 implementation. Guarantees:
--   * Distinction without colour: every state is unique by glyph and position.
--   * Exact 20-cell progress bars with unicode eighth-block fractional resolution.
--   * Strict totality without partial indexing or runtime exceptions (Standard 1.1, 5.2).
module Buildr.Grammar
  ( -- * Visual States & Glyphs
    VisualState (..)
  , stateGlyph
  , stateName
    -- * Progress Bar Specification
  , defaultBarWidth
  , renderBar
  , renderSegmentedBar
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Semantic visual states recognized in Visual Grammar v1.
data VisualState
  = VisDone
  | VisCompiling
  | VisOverrun
  | VisNoHistory
  | VisWaiting
  | VisSkipped
  | VisFolded
  | VisFailed
  | VisBlocked
  | VisNotReached
  deriving (Eq, Show)

-- | Fixed width of progress bars across all standard views.
defaultBarWidth :: Int
defaultBarWidth = 20

-- | Assigns a unique Unicode glyph to each visual state (§11).
stateGlyph :: VisualState -> Char
stateGlyph = \case
  VisDone       -> '●'
  VisCompiling  -> '◐'
  VisOverrun    -> '◐'
  VisNoHistory  -> '◐'
  VisWaiting    -> '○'
  VisSkipped    -> '◇'
  VisFolded     -> '▸'
  VisFailed     -> '✖'
  VisBlocked    -> '⊘'
  VisNotReached -> '◌'

-- | Canonical human-readable label for a visual state.
stateName :: VisualState -> Text
stateName = \case
  VisDone       -> "done"
  VisCompiling  -> "compiling"
  VisOverrun    -> "overrun"
  VisNoHistory  -> "no history"
  VisWaiting    -> "waiting"
  VisSkipped    -> "skipped"
  VisFolded     -> "folded"
  VisFailed     -> "failed"
  VisBlocked    -> "blocked"
  VisNotReached -> "not reached"

-- | Maps eighth-block remainder (1-7) to its unicode character totally.
lookupEighth :: Int -> Char
lookupEighth = \case
  1 -> '▏'
  2 -> '▎'
  3 -> '▍'
  4 -> '▌'
  5 -> '▋'
  6 -> '▊'
  7 -> '▉'
  _ -> ' '

-- | Formats a fractional progress bar of exact width with eighth-block precision.
-- Clamps ratio to [0.0, 1.0]. Total and free of partial indexing.
renderBar :: Int -> Double -> Text
renderBar width ratio
  | width <= 0 = ""
  | otherwise =
      let clamped = max 0.0 (min 1.0 ratio)
          totalEighths = round (clamped * fromIntegral (width * 8)) :: Int
          fullBlocks = min width (totalEighths `div` 8)
          remEighth = totalEighths `mod` 8
          emptyBlocks = max 0 (width - fullBlocks - if remEighth > 0 then 1 else 0)
          fractionChar = if remEighth > 0 && fullBlocks < width
                           then [lookupEighth remEighth]
                           else ""
          fullStr = replicate fullBlocks '█'
          emptyStr = replicate emptyBlocks ' '
      in "[" <> T.pack (fullStr <> fractionChar <> emptyStr) <> "]"

-- | Renders a composite segmented bar showing completed, blocked, and remaining segments.
renderSegmentedBar :: Int -> Int -> Int -> Int -> Text
renderSegmentedBar width doneCount blockedCount totalCount
  | totalCount <= 0 || width <= 0 = "[" <> T.replicate width " " <> "]"
  | otherwise =
      let doneCells = (doneCount * width) `div` totalCount
          blockedCells = (blockedCount * width) `div` totalCount
          remCells = max 0 (width - doneCells - blockedCells)
      in "[" <> T.replicate doneCells "█"
             <> T.replicate blockedCells "╳"
             <> T.replicate remCells " " <> "]"
