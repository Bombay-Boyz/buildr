module Buildr.View
  ( ViewMode (..)
  , renderModel
  ) where

import Buildr.Model
import Data.Text (Text)

data ViewMode
  = ViewGraph
  | ViewZen
  | ViewMosaic
  | ViewPlain
  | ViewLinear
  deriving (Eq, Show)

renderModel :: ViewMode -> BuildModel -> Text
renderModel mode _model = case mode of
  ViewGraph  -> "buildr [graph view]"
  ViewZen    -> "buildr [zen view]"
  ViewMosaic -> "buildr [mosaic view]"
  ViewPlain  -> "buildr [plain view]"
  ViewLinear -> "buildr [linear view]"
