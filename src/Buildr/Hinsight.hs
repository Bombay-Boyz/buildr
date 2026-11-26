-- |
-- Module      : Buildr.Hinsight
-- Description : Integration and version-gate checks for the hinsight explanation engine.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Implements the exact-version gate and explanation protocol defined in §15 of
-- the vision document. All analyses run in a separate process, never pinned by
-- buildr, and only proceed on an exact string match with hinsight's supported GHC.
module Buildr.Hinsight
  ( -- * Version Gate (§15)
    HinsightGate (..)
  , checkHinsightGate
  , formatGateNote
    -- * Typed Holes & Fits (§14, §15)
  , HoleFit (..)
  , DiagnosticExplanation (..)
  , parseHoleFits
  , parseHoleFitsFromText
  , formatExplanation
  , applyHoleFitToFile
    -- * On-Demand Explanation (§15)
  , requestExplanation
  ) where

import Buildr.Event (DiagnosticPayload (..), SourceSpan (..))
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Char (isDigit, isSpace)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Outcome of evaluating the exact GHC version gate (§15).
data HinsightGate
  = GateMatched !Text
    -- ^ Exact string match between project GHC and hinsight's supported version.
  | GateMismatched !Text !Text
    -- ^ Mismatch: (hinsight supported version, project GHC version).
  | GateMissing
    -- ^ The 'hinsight' executable was not found on PATH.
  | GateFailed !Text
    -- ^ An IO or execution error occurred during gate verification.
  deriving (Eq, Show, Generic)

instance ToJSON HinsightGate
instance FromJSON HinsightGate

-- | Renders the honest gate note required by §6 and §15.
formatGateNote :: HinsightGate -> Text
formatGateNote = \case
  GateMatched ver ->
    "hinsight ready (exact match: GHC " <> ver <> ")"
  GateMismatched supported project ->
    "explanations unavailable: hinsight supports GHC " <> supported <> ", this project uses " <> project
  GateMissing ->
    "explanations unavailable: hinsight not found on PATH"
  GateFailed err ->
    "explanations unavailable: " <> err

-- | A typed-hole suggestion candidate (§14, §15).
data HoleFit = HoleFit
  { hfIndex :: !Int
    -- ^ 1..9 keyboard selector key.
  , hfExpr  :: !Text
    -- ^ Suggested expression candidate.
  , hfType  :: !Text
    -- ^ Inferred type signature.
  } deriving (Eq, Show, Generic)

instance ToJSON HoleFit
instance FromJSON HoleFit

-- | Detailed diagnostic explanation produced on demand (§15).
data DiagnosticExplanation = DiagnosticExplanation
  { deSpan     :: !SourceSpan
  , deCode     :: !(Maybe Int)
  , deSummary  :: !Text
  , deHoleFits :: ![HoleFit]
  , deTypeDiff :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON DiagnosticExplanation
instance FromJSON DiagnosticExplanation

-- | Extracts the first line safely without partial functions.
safeHeadLine :: String -> Text
safeHeadLine s = case lines s of
  (l:_) -> T.strip (T.pack l)
  []    -> ""

-- | Queries hinsight --supported-ghc and compares against the project's exact GHC version (§15).
checkHinsightGate :: Text -> IO HinsightGate
checkHinsightGate projectGhc = do
  mExe <- findExecutable "hinsight"
  case mExe of
    Nothing -> pure GateMissing
    Just exePath -> do
      result <- try (readProcessWithExitCode exePath ["--supported-ghc"] "") :: IO (Either SomeException (ExitCode, String, String))
      case result of
        Left err -> pure (GateFailed (T.pack (show err)))
        Right (ExitSuccess, stdoutStr, _) ->
          let supported = safeHeadLine stdoutStr
              proj = T.strip projectGhc
          in if not (T.null supported) && supported == proj
               then pure (GateMatched supported)
               else pure (GateMismatched supported proj)
        Right (ExitFailure c, _, stderrStr) ->
          let msg = if null stderrStr then "exit code " <> show c else safeFirstLine stderrStr
          in pure (GateFailed (T.pack msg))
  where
    safeFirstLine s = case lines s of
      (l:_) -> l
      []    -> "unknown error"

-- | Parses typed-hole fit lines from GHC 9.10 compiler diagnostic messages.
parseHoleFits :: [Text] -> [HoleFit]
parseHoleFits msgs =
  let fullText = T.unlines msgs
  in parseHoleFitsFromText fullText

-- | Parses numbered or unnumbered typed hole fits from formatted text.
parseHoleFitsFromText :: Text -> [HoleFit]
parseHoleFitsFromText fullText =
  let allLines = map T.strip (T.lines fullText)
      candidateLines = filter isFitLine allLines
      fits = mapMaybe parseSingleFit (zip [1 .. 9] candidateLines)
  in take 9 fits
  where
    isFitLine l =
      "::" `T.isInfixOf` l
        && not ("Found hole:" `T.isPrefixOf` l)
        && not ("Valid hole fits include" `T.isPrefixOf` l)
        && not ("Valid substitutions include" `T.isPrefixOf` l)

    parseSingleFit (idx, line) =
      let clean = stripLeadingNumber line
          parts = T.splitOn "::" clean
      in case parts of
        (expr:rest) ->
          let exprClean = T.strip expr
              typeClean = T.strip (T.intercalate "::" rest)
              -- Drop trailing annotations like "(bound at ...)"
              typeNoBound = case T.splitOn "(bound at" typeClean of
                (t:_) -> T.strip t
                []    -> typeClean
          in if not (T.null exprClean) && not (T.null typeNoBound)
               then Just (HoleFit idx exprClean typeNoBound)
               else Nothing
        [] -> Nothing

    stripLeadingNumber l =
      let s = T.stripStart l
      in case T.uncons s of
        Just (d, rest) | isDigit d ->
          let restNoPunct = T.dropWhile (\c -> c == '.' || c == ')' || isSpace c) rest
          in restNoPunct
        _ -> s

-- | Formats diagnostic explanation and any available hole fits for rendering.
formatExplanation :: DiagnosticExplanation -> Text
formatExplanation expl =
  let header = "  [Explanation" <> maybe "" (\c -> " (GHC-" <> T.pack (show c) <> ")") (deCode expl) <> "]"
      summaryLines = map ("    " <>) (T.lines (deSummary expl))
      fitLines = if null (deHoleFits expl)
                   then []
                   else "    Typed-hole suggestions (press 1-9 to accept):"
                          : map formatFit (deHoleFits expl)
  in T.unlines ([header] <> summaryLines <> fitLines)
  where
    formatFit (HoleFit idx expr ty) =
      "      [" <> T.pack (show idx) <> "] " <> expr <> " :: " <> ty

-- | Requests an on-demand error explanation respecting the §15 exact-version gate.
requestExplanation :: HinsightGate -> DiagnosticPayload -> IO (Either Text DiagnosticExplanation)
requestExplanation gate diag = case gate of
  GateMatched _ -> do
    mExe <- findExecutable "hinsight"
    case mExe of
      Nothing -> pure (Left (formatGateNote GateMissing))
      Just exe -> do
        let sp = diagSpan diag
            codeArgs = case diagCode diag of
              Just c  -> ["--code", show c]
              Nothing -> []
            args = [ "explain"
                   , "--file", spanFile sp
                   , "--line", show (spanStartLine sp)
                   , "--col", show (spanStartCol sp)
                   ] <> codeArgs
        res <- try (readProcessWithExitCode exe args "") :: IO (Either SomeException (ExitCode, String, String))
        case res of
          Right (ExitSuccess, stdoutStr, _) ->
            let fits = parseHoleFits (diagMessage diag <> diagHints diag)
                summary = T.pack stdoutStr
            in pure (Right (DiagnosticExplanation sp (diagCode diag) summary fits Nothing))
          _ ->
            let fits = parseHoleFits (diagMessage diag <> diagHints diag)
                fallbackSummary = T.intercalate "\n" (diagMessage diag)
            in pure (Right (DiagnosticExplanation sp (diagCode diag) fallbackSummary fits Nothing))
  otherGate ->
    -- When gate is not matched, policy requires NEVER invoking hinsight.
    -- However, GHC's own typed-hole fits are safely extracted.
    let fits = parseHoleFits (diagMessage diag <> diagHints diag)
        note = formatGateNote otherGate
    in if null fits
         then pure (Left note)
         else pure (Right (DiagnosticExplanation (diagSpan diag) (diagCode diag) note fits Nothing))

-- | Replaces a typed-hole span in a source file with an accepted fit candidate (§12.6, §15).
applyHoleFitToFile :: FilePath -> SourceSpan -> HoleFit -> IO (Either Text ())
applyHoleFitToFile path sp fit = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("Source file not found: " <> T.pack path))
    else do
      contentRes <- try (TIO.readFile path) :: IO (Either SomeException Text)
      case contentRes of
        Left err -> pure (Left ("Failed to read file: " <> T.pack (show err)))
        Right content -> do
          let fileLines = T.lines content
              targetLineIdx = spanStartLine sp - 1
          if targetLineIdx < 0 || targetLineIdx >= length fileLines
            then pure (Left "Invalid line index for typed-hole substitution")
            else
              let (before, targetLineAndAfter) = splitAt targetLineIdx fileLines
              in case targetLineAndAfter of
                (targetLine:after) ->
                  let startCol = max 0 (spanStartCol sp - 1)
                      endCol = max startCol (spanEndCol sp - 1)
                      linePrefix = T.take startCol targetLine
                      lineSuffix = T.drop endCol targetLine
                      newLine = linePrefix <> hfExpr fit <> lineSuffix
                      newContent = T.unlines (before <> [newLine] <> after)
                  in do
                    writeRes <- try (TIO.writeFile path newContent) :: IO (Either SomeException ())
                    case writeRes of
                      Left wErr -> pure (Left ("Failed to write file: " <> T.pack (show wErr)))
                      Right ()  -> pure (Right ())
                [] -> pure (Left "Unexpected empty line split")
