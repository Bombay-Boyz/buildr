module Buildr.Grammar
  ( ModuleVisualState (..)
  , stateGlyph
  , barWidth
  ) where

data ModuleVisualState
  = StateDone
  | StateCompiling
  | StateOverrun
  | StateNoHistory
  | StateWaiting
  | StateSkipped
  | StateFolded
  | StateFailed
  | StateBlocked
  | StateNotReached
  deriving (Eq, Show)

barWidth :: Int
barWidth = 20

stateGlyph :: ModuleVisualState -> Char
stateGlyph = \case
  StateDone       -> '●'
  StateCompiling  -> '◐'
  StateOverrun    -> '◐'
  StateNoHistory  -> '◐'
  StateWaiting    -> '○'
  StateSkipped    -> '◇'
  StateFolded     -> '▸'
  StateFailed     -> '✖'
  StateBlocked    -> '⊘'
  StateNotReached -> '◌'
