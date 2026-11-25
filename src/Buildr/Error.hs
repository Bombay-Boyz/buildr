-- |
-- Module      : Buildr.Error
-- Description : Closed, layered error vocabulary for all buildr pipeline stages.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- This module defines the complete, closed taxonomy of failure modes in buildr,
-- adhering strictly to the Haskell Engineering Standard:
--   * Staged and layered sums (Standard 2.3)
--   * Dedicated error module with descriptive messages (Standard 2.15)
--   * Zero catch-alls or stringly-typed errors (Standard 1.5)
--   * Full totality with explicit exports (Standard 1.1, 2.5)
module Buildr.Error
  ( -- * Top-Level Error Hierarchy
    BuildrError (..)
  , PlanError (..)
  , EventError (..)
  , ModelError (..)
  , IpcError (..)
  , ConfigError (..)
    -- * Error Formatting
  , describeError
  , describePlanError
  , describeEventError
  , describeModelError
  , describeIpcError
  , describeConfigError
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

-- | Closed sum of failures encountered when discovering and parsing Cabal's plan.json.
data PlanError
  = PlanJsonNotFound !FilePath
  -- ^ Raised when 'dist-newstyle/cache/plan.json' cannot be found on disk.
  | PlanJsonParseFailure !FilePath !Text
  -- ^ Raised when JSON decoding of plan.json fails at the given path with a parser message.
  | PlanUnitNotFound !Text
  -- ^ Raised when a unit ID referenced by compiler output is missing from the declared plan.
  deriving (Eq, Show)

-- | Closed sum of failures occurring within the Event Protocol v1 stream.
data EventError
  = EventJsonDecodeFailure !Int64 !Text
  -- ^ Raised when a line in an event log fails JSON deserialization at the specified line number.
  | EventSequenceAnomaly !Int64 !Int64
  -- ^ Raised when an event sequence breaks monotonicity (expected sequence, observed sequence).
  | EventTimestampAnomaly !Int64 !Int64
  -- ^ Raised when an event timestamp regresses backwards in monotonic time (previous, current).
  deriving (Eq, Show)

-- | Closed sum of domain state-machine violations.
data ModelError
  = ModelModuleFinishedWithoutStart !Text
  -- ^ Raised if a module completion is reported for a module never observed starting.
  | ModelUnknownModuleTransition !Text !Text !Text
  -- ^ Raised on an invalid state transition (module, current state, transition event).
  | ModelCyclicDependency !Text !Text
  -- ^ Raised if an impossible dependency cycle is reported in the module graph.
  deriving (Eq, Show)

-- | Closed sum of Unix domain socket / named pipe transport failures.
data IpcError
  = IpcSocketCreationFailed !FilePath !Text
  -- ^ Raised when creating the IPC server socket fails at the given path.
  | IpcSocketAcceptFailed !Text
  -- ^ Raised when accepting a client connection on the socket fails.
  | IpcClientConnectionFailed !FilePath !Text
  -- ^ Raised when a wrapper process fails to connect to the parent socket.
  deriving (Eq, Show)

-- | Closed sum of configuration, environment, and CLI validation errors.
data ConfigError
  = ConfigUnsupportedGhcVersion !Text
  -- ^ Raised when the project GHC version is older than the required 9.10 base.
  | ConfigUnsupportedCabalVersion !Text
  -- ^ Raised when the detected cabal-install version falls outside supported bounds.
  | ConfigInvalidOption !Text
  -- ^ Raised when an unrecognised or mutually exclusive configuration option is provided.
  deriving (Eq, Show)

-- | The canonical top-level error type partitioning failures by pipeline stage.
data BuildrError
  = BuildrPlanError !PlanError
  | BuildrEventError !EventError
  | BuildrModelError !ModelError
  | BuildrIpcError !IpcError
  | BuildrConfigError !ConfigError
  deriving (Eq, Show)

-- | Renders a human-readable, descriptive explanation of any 'PlanError'.
describePlanError :: PlanError -> Text
describePlanError = \case
  PlanJsonNotFound path ->
    "Cabal build plan not found at path: " <> T.pack path <> ". Run 'cabal build --dry-run' first."
  PlanJsonParseFailure path reason ->
    "Failed to parse plan.json at " <> T.pack path <> ": " <> reason
  PlanUnitNotFound unitId ->
    "Compiler emitted event for unit not present in plan.json: " <> unitId

-- | Renders a human-readable, descriptive explanation of any 'EventError'.
describeEventError :: EventError -> Text
describeEventError = \case
  EventJsonDecodeFailure lineNo reason ->
    "Event Protocol v1 decode error at line " <> T.pack (show lineNo) <> ": " <> reason
  EventSequenceAnomaly expected actual ->
    "Event sequence non-monotonic: expected " <> T.pack (show expected)
      <> ", but received " <> T.pack (show actual)
  EventTimestampAnomaly prev curr ->
    "Event timestamp regressed backwards: previous " <> T.pack (show prev)
      <> " ns, current " <> T.pack (show curr) <> " ns"

-- | Renders a human-readable, descriptive explanation of any 'ModelError'.
describeModelError :: ModelError -> Text
describeModelError = \case
  ModelModuleFinishedWithoutStart modName ->
    "Module finished without ever starting: " <> modName
  ModelUnknownModuleTransition modName curSt targetEv ->
    "Invalid state transition for module " <> modName <> " from " <> curSt <> " on " <> targetEv
  ModelCyclicDependency modA modB ->
    "Cyclic dependency detected between modules " <> modA <> " and " <> modB

-- | Renders a human-readable, descriptive explanation of any 'IpcError'.
describeIpcError :: IpcError -> Text
describeIpcError = \case
  IpcSocketCreationFailed path reason ->
    "Failed to bind buildr IPC socket at " <> T.pack path <> ": " <> reason
  IpcSocketAcceptFailed reason ->
    "Failed to accept incoming wrapper connection: " <> reason
  IpcClientConnectionFailed path reason ->
    "buildr-ghc wrapper failed to connect to buildr socket at " <> T.pack path <> ": " <> reason

-- | Renders a human-readable, descriptive explanation of any 'ConfigError'.
describeConfigError :: ConfigError -> Text
describeConfigError = \case
  ConfigUnsupportedGhcVersion v ->
    "Unsupported GHC version " <> v <> ". buildr requires GHC 9.10 or newer."
  ConfigUnsupportedCabalVersion v ->
    "Unsupported cabal-install version " <> v <> ". buildr requires cabal-install 3.12 through 3.16."
  ConfigInvalidOption opt ->
    "Invalid configuration option: " <> opt

-- | | The top-level error describer dispatching to stage-specific describers.
-- Guarantees complete, informative error output without stringly typing.
describeError :: BuildrError -> Text
describeError = \case
  BuildrPlanError e   -> describePlanError e
  BuildrEventError e  -> describeEventError e
  BuildrModelError e  -> describeModelError e
  BuildrIpcError e    -> describeIpcError e
  BuildrConfigError e -> describeConfigError e
