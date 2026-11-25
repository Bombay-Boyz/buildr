module Buildr.Core
  ( module Buildr.Event
  , module Buildr.Model
  , module Buildr.Grammar
  , module Buildr.View
  , module Buildr.History
  , buildrVersion
  ) where

import Buildr.Event
import Buildr.Grammar
import Buildr.History
import Buildr.Model
import Buildr.View
import Data.Text (Text)

buildrVersion :: Text
buildrVersion = "0.1.0.0"
