-- |
-- Module      : Buildr.Core
-- Description : Re-exports the core buildr types, model, events, grammar, and views.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
module Buildr.Core
  ( -- * Core Re-exports
    module Buildr.Benchmark
  , module Buildr.Config
  , module Buildr.Cli
  , module Buildr.Editor
  , module Buildr.Error
  , module Buildr.Event
  , module Buildr.Grammar
  , module Buildr.Graph
  , module Buildr.Hinsight
  , module Buildr.History
  , module Buildr.Ipc
  , module Buildr.Model
  , module Buildr.Plan
  , module Buildr.Signal
  , module Buildr.Terminal
  , module Buildr.Theme
  , module Buildr.View
    -- * Versioning
  , buildrVersion
  ) where

import Buildr.Benchmark
import Buildr.Config
import Buildr.Cli
import Buildr.Editor
import Buildr.Error
import Buildr.Event
import Buildr.Grammar
import Buildr.Graph
import Buildr.Hinsight
import Buildr.History
import Buildr.Ipc
import Buildr.Model
import Buildr.Plan
import Buildr.Signal
import Buildr.Terminal
import Buildr.Theme
import Buildr.View
import Data.Text (Text)

-- | Current buildr version string.
buildrVersion :: Text
buildrVersion = "0.1.0.0"
