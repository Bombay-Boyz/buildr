-- |
-- Module      : Buildr.Config
-- Description : Configuration file resolution, parsing, and precedence merging.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Implements configuration file discovery, TOML decoding, and hierarchical precedence
-- merging per §18:
--   CLI flags > Project configuration (buildr.toml) > User configuration (config.toml) > Built-in defaults.
module Buildr.Config
  ( -- * Configuration Types
    BuildConfig (..)
  , ResolvedBuildConfig (..)
  , emptyBuildConfig
  , defaultBuildConfig
    -- * Precedence Resolution
  , resolveBuildConfig
    -- * TOML Parsing
  , parseTomlConfig
  , parseTomlLine
    -- * Configuration Discovery & Loading
  , loadConfigFile
  , findProjectConfigFile
  , findUserConfigFile
  , resolveEffectiveConfig
  ) where

import Buildr.Error (ConfigError (..))
import Buildr.Event (ErrorStyle (..))
import Buildr.View (ViewMode (..))
import Control.Applicative ((<|>))
import Control.Exception (IOException, catch)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( canonicalizePath
  , doesFileExist
  , getCurrentDirectory
  , getHomeDirectory
  , getXdgDirectory
  , XdgDirectory (XdgConfig)
  )
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))

-- | Partial configuration values collected from CLI or configuration files.
data BuildConfig = BuildConfig
  { cfgViewMode     :: !(Maybe ViewMode)
  , cfgErrorStyle   :: !(Maybe ErrorStyle)
  , cfgKeepGoing    :: !(Maybe Bool)
  , cfgExplainHoles :: !(Maybe Bool)
  , cfgThemeName    :: !(Maybe String)
  } deriving (Eq, Show)

-- | Monoidal composition representing left-biased precedence override:
-- @a <> b@ yields @a@'s setting if present, otherwise falling back to @b@'s.
instance Semigroup BuildConfig where
  a <> b = BuildConfig
    { cfgViewMode     = cfgViewMode a <|> cfgViewMode b
    , cfgErrorStyle   = cfgErrorStyle a <|> cfgErrorStyle b
    , cfgKeepGoing    = cfgKeepGoing a <|> cfgKeepGoing b
    , cfgExplainHoles = cfgExplainHoles a <|> cfgExplainHoles b
    , cfgThemeName    = cfgThemeName a <|> cfgThemeName b
    }

instance Monoid BuildConfig where
  mempty = BuildConfig Nothing Nothing Nothing Nothing Nothing

-- | An empty 'BuildConfig' with all fields unset.
emptyBuildConfig :: BuildConfig
emptyBuildConfig = mempty

-- | Fully resolved configuration with all defaults applied.
data ResolvedBuildConfig = ResolvedBuildConfig
  { resViewMode     :: !ViewMode
  , resErrorStyle   :: !ErrorStyle
  , resKeepGoing    :: !Bool
  , resExplainHoles :: !Bool
  , resThemeName    :: !(Maybe String)
  } deriving (Eq, Show)

-- | Built-in fallback defaults per §18.
defaultBuildConfig :: ResolvedBuildConfig
defaultBuildConfig = ResolvedBuildConfig
  { resViewMode     = ViewGraph
  , resErrorStyle   = ErrorClassic
  , resKeepGoing    = False
  , resExplainHoles = False
  , resThemeName    = Nothing
  }

-- | Merges CLI flags, project configuration, and user configuration against built-in defaults.
-- Enforces strict precedence order:
-- @CLI flags > Project configuration (buildr.toml) > User configuration (config.toml) > Built-in defaults@.
resolveBuildConfig :: BuildConfig -> BuildConfig -> BuildConfig -> ResolvedBuildConfig
resolveBuildConfig cli proj user =
  let merged = cli <> proj <> user
  in ResolvedBuildConfig
    { resViewMode     = fromMaybe (resViewMode defaultBuildConfig) (cfgViewMode merged)
    , resErrorStyle   = fromMaybe (resErrorStyle defaultBuildConfig) (cfgErrorStyle merged)
    , resKeepGoing    = fromMaybe (resKeepGoing defaultBuildConfig) (cfgKeepGoing merged)
    , resExplainHoles = fromMaybe (resExplainHoles defaultBuildConfig) (cfgExplainHoles merged)
    , resThemeName    = cfgThemeName merged <|> resThemeName defaultBuildConfig
    }

-- | Strips inline TOML comments (@#@) while respecting quoted string literals.
stripComment :: Text -> Text
stripComment = go False False
  where
    go inDouble inSingle txt = case T.uncons txt of
      Nothing -> ""
      Just ('"', rest)
        | inSingle  -> T.cons '"' (go inDouble inSingle rest)
        | otherwise -> T.cons '"' (go (not inDouble) inSingle rest)
      Just ('\'', rest)
        | inDouble  -> T.cons '\'' (go inDouble inSingle rest)
        | otherwise -> T.cons '\'' (go inDouble (not inSingle) rest)
      Just ('#', _)
        | not inDouble && not inSingle -> ""
      Just (c, rest) ->
        T.cons c (go inDouble inSingle rest)

-- | Strips surrounding single or double quotes from a string value.
unquote :: Text -> Text
unquote t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"' =
      T.drop 1 (T.dropEnd 1 t)
  | T.length t >= 2 && T.head t == '\'' && T.last t == '\'' =
      T.drop 1 (T.dropEnd 1 t)
  | otherwise = t

-- | Normalizes key names by stripping quotes, lowering case, and converting underscores to dashes.
normalizeKey :: Text -> Text
normalizeKey = T.replace "_" "-" . T.toLower . T.strip . unquote

-- | Parses a single TOML key-value pair into a field updater for 'BuildConfig'.
parseTomlLine :: Text -> Text -> Either Text (BuildConfig -> BuildConfig)
parseTomlLine rawKey rawVal =
  let key = normalizeKey rawKey
      val = T.strip (unquote (T.strip rawVal))
      valLower = T.toLower val
  in case key of
    "view" -> case valLower of
      "graph"  -> Right (\c -> c { cfgViewMode = Just ViewGraph })
      "zen"    -> Right (\c -> c { cfgViewMode = Just ViewZen })
      "mosaic" -> Right (\c -> c { cfgViewMode = Just ViewMosaic })
      "plain"  -> Right (\c -> c { cfgViewMode = Just ViewPlain })
      "linear" -> Right (\c -> c { cfgViewMode = Just ViewLinear })
      other    -> Left ("Unknown view: " <> other)
    "errors" -> case valLower of
      "classic"  -> Right (\c -> c { cfgErrorStyle = Just ErrorClassic })
      "friendly" -> Right (\c -> c { cfgErrorStyle = Just ErrorFriendly })
      other      -> Left ("Unknown error style: " <> other)
    "keep-going" -> case valLower of
      "true"  -> Right (\c -> c { cfgKeepGoing = Just True })
      "false" -> Right (\c -> c { cfgKeepGoing = Just False })
      other   -> Left ("Expected boolean for keep-going, got: " <> other)
    "explain-holes" -> case valLower of
      "true"  -> Right (\c -> c { cfgExplainHoles = Just True })
      "false" -> Right (\c -> c { cfgExplainHoles = Just False })
      other   -> Left ("Expected boolean for explain-holes, got: " <> other)
    "theme" ->
      Right (\c -> c { cfgThemeName = Just (T.unpack val) })
    _ ->
      -- Unrecognized keys are ignored for forward-compatibility
      Right id

-- | Pure TOML parser converting configuration file text into 'BuildConfig'.
-- Recognizes top-level keys as well as sections named @[build]@, @[buildr]@, or @[defaults]@.
parseTomlConfig :: Text -> Either Text BuildConfig
parseTomlConfig input = foldl parseLine (Right (emptyBuildConfig, True)) (T.lines input) >>= (Right . fst)
  where
    parseLine (Left err) _ = Left err
    parseLine (Right (acc, inActiveSection)) line =
      let clean = T.strip (stripComment line)
      in if T.null clean
           then Right (acc, inActiveSection)
           else if T.isPrefixOf "[" clean && T.isSuffixOf "]" clean
                  then
                    let sec = T.toLower (T.strip (T.drop 1 (T.dropEnd 1 clean)))
                        isActive = sec `elem` ["", "build", "buildr", "defaults"]
                    in Right (acc, isActive)
                  else if not inActiveSection
                         then Right (acc, inActiveSection)
                         else
                           let (k, rest) = T.breakOn "=" clean
                           in if T.null rest
                                then Left ("Invalid TOML syntax (missing '='): " <> clean)
                                else
                                  let v = T.drop 1 rest
                                  in case parseTomlLine k v of
                                       Left err -> Left err
                                       Right f  -> Right (f acc, inActiveSection)

-- | Loads and parses a TOML configuration file from disk.
loadConfigFile :: FilePath -> IO (Either ConfigError BuildConfig)
loadConfigFile path = do
  mContent <- (Just <$> TIO.readFile path) `catch` handleIoError
  case mContent of
    Nothing -> pure (Left (ConfigFileReadError path "Failed to read file"))
    Just content -> case parseTomlConfig content of
      Left err  -> pure (Left (ConfigParseError path err))
      Right cfg -> pure (Right cfg)
  where
    handleIoError :: IOException -> IO (Maybe Text)
    handleIoError _ = pure Nothing

-- | Discovers the project-level @buildr.toml@ starting from the current directory
-- and searching up to project markers or root.
findProjectConfigFile :: IO (Maybe FilePath)
findProjectConfigFile = do
  cwd <- getCurrentDirectory
  searchDir cwd
  where
    searchDir dir = do
      let candidate = dir </> "buildr.toml"
      exists <- doesFileExist candidate
      if exists
        then Just <$> canonicalizePath candidate
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure Nothing
            else do
              let isBoundary = dir </> "cabal.project"
              hasBoundary <- doesFileExist isBoundary
              if hasBoundary
                then pure Nothing
                else searchDir parent

-- | Discovers the user-level @config.toml@ per §18:
-- Checks @$XDG_CONFIG_HOME/buildr/config.toml@, falling back to @~/.config/buildr/config.toml@.
findUserConfigFile :: IO (Maybe FilePath)
findUserConfigFile = do
  mXdgEnv <- lookupEnv "XDG_CONFIG_HOME"
  case mXdgEnv of
    Just xdgDir | not (null xdgDir) -> do
      let path = xdgDir </> "buildr" </> "config.toml"
      exists <- doesFileExist path
      if exists
        then Just <$> canonicalizePath path
        else fallbackHomeConfig
    _ -> fallbackHomeConfig
  where
    fallbackHomeConfig = do
      stdXdg <- getXdgDirectory XdgConfig "buildr/config.toml"
      existsStd <- doesFileExist stdXdg
      if existsStd
        then Just <$> canonicalizePath stdXdg
        else do
          home <- getHomeDirectory
          let legacyPath = home </> ".config" </> "buildr" </> "config.toml"
          existsLegacy <- doesFileExist legacyPath
          if existsLegacy
            then Just <$> canonicalizePath legacyPath
            else pure Nothing

-- | Orchestrates complete configuration resolution:
-- Searches and parses project and user configuration files, then merges them with CLI options.
resolveEffectiveConfig :: BuildConfig -> IO (Either ConfigError ResolvedBuildConfig)
resolveEffectiveConfig cliCfg = do
  mProjPath <- findProjectConfigFile
  mProjCfg <- case mProjPath of
    Just path -> do
      res <- loadConfigFile path
      case res of
        Left err  -> pure (Left err)
        Right cfg -> pure (Right cfg)
    Nothing -> pure (Right emptyBuildConfig)

  case mProjCfg of
    Left err -> pure (Left err)
    Right projCfg -> do
      mUserPath <- findUserConfigFile
      mUserCfg <- case mUserPath of
        Just path -> do
          res <- loadConfigFile path
          case res of
            Left err  -> pure (Left err)
            Right cfg -> pure (Right cfg)
        Nothing -> pure (Right emptyBuildConfig)

      case mUserCfg of
        Left err -> pure (Left err)
        Right userCfg ->
          pure (Right (resolveBuildConfig cliCfg projCfg userCfg))
