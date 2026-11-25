-- |
-- Module      : Buildr.Graph
-- Description : Upfront module dependency discovery, import parsing, and topological sorting.
-- Copyright   : (c) bombay boyz, 2026
-- License     : MIT
-- Maintainer  : 259497327+Bombay-Boyz@users.noreply.github.com
-- Stability   : experimental
--
-- Pure module graph analysis and dependency ordering adhering to:
--   * Named published algorithm: Kahn's algorithm (1962) for topological order (Standard 4.2).
--   * Upfront dependency frontier discovery (§8.2, §10, §14).
--   * Totality: 100% stack-safe and free of partial indexing or exceptions (Standard 1.1).
module Buildr.Graph
  ( -- * Pure Parsing & Dependency Analysis
    parseModuleHeader
  , parseImports
  , kahnTopologicalSort
    -- * Upfront Graph Discovery
  , discoverSourceFiles
  , discoverUnitGraph
  ) where

import Buildr.Event (GraphEdge (..), ModuleNode (..))
import Control.Exception (SomeException, try)
import Data.List (isSuffixOf, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | Purely extracts the module name declared in a Haskell source file header.
-- Returns 'Nothing' if the file is an anonymous Main or unparseable.
parseModuleHeader :: Text -> Maybe Text
parseModuleHeader content =
  let ls = map T.strip (T.lines content)
      nonComment = filter (\l -> not (T.null l || T.isPrefixOf "--" l || T.isPrefixOf "{-" l)) ls
      findMod = \case
        [] -> Nothing
        (l:rest)
          | "module " `T.isPrefixOf` l ->
              let afterMod = T.drop 7 l
                  name = T.takeWhile (\c -> c /= '(' && c /= ' ' && c /= '\t' && c /= '\n') (T.strip afterMod)
              in if T.null name then Nothing else Just name
          | otherwise -> findMod rest
  in findMod nonComment

-- | Purely extracts imported module names and whether each import is a source import ({-# SOURCE #-}).
parseImports :: Text -> [(Text, Bool)]
parseImports content =
  let ls = map T.strip (T.lines content)
      isImportLine l = "import " `T.isPrefixOf` l && not ("--" `T.isPrefixOf` l)
      extractImport l =
        let rest0 = T.drop 7 l
            (isSrc, rest1) =
              if "{-# SOURCE #-}" `T.isPrefixOf` rest0
                then (True, T.strip (T.drop 15 rest0))
                else (False, rest0)
            rest2 = if "qualified " `T.isPrefixOf` rest1
                      then T.strip (T.drop 10 rest1)
                      else rest1
            modName = T.takeWhile (\c -> c /= '(' && c /= ' ' && c /= '\t') (T.strip rest2)
        in if T.null modName then Nothing else Just (modName, isSrc)
  in mapMaybe extractImport (filter isImportLine ls)

-- | Implements Kahn's algorithm (1962) to compute the topological compilation order
-- of modules given their nodes and dependency edges (Standard 4.2).
--
-- Guaranteed properties:
--   * Total and deterministic.
--   * If a module A depends on module B, B precedes A in the returned list.
--   * Cycles (if any) are broken deterministically by sorting.
kahnTopologicalSort :: [ModuleNode] -> [GraphEdge] -> [Text]
kahnTopologicalSort nodes edges =
  let allNames = sort (map mnName nodes)
      nodeSet = Set.fromList allNames

      -- Map: dependent module -> set of local dependencies it must wait on
      depMap :: Map Text (Set Text)
      depMap =
        let initMap = Map.fromList [ (n, Set.empty) | n <- allNames ]
            insertEdge acc e = case geSource e of
              Just srcMod
                | Set.member srcMod nodeSet && Set.member (geTarget e) nodeSet && not (geIsSourceImport e) ->
                    Map.adjust (Set.insert (geTarget e)) srcMod acc
              _ -> acc
        in foldl insertEdge initMap edges

      -- Map: dependency module -> list of modules that depend on it
      adjMap :: Map Text [Text]
      adjMap =
        let initAdj = Map.fromList [ (n, []) | n <- allNames ]
            addAdj acc (depMod, deps) =
              foldl (\a d -> Map.adjust (depMod :) d a) acc (Set.toList deps)
        in foldl addAdj initAdj (Map.toList depMap)

      -- Initial in-degrees: count of dependencies each module waits on
      inDegrees :: Map Text Int
      inDegrees = Map.map Set.size depMap

      -- Nodes with zero in-degree (ready to compile immediately)
      initialQueue = sort [ n | (n, deg) <- Map.toList inDegrees, deg == 0 ]

      -- Pure loop draining queue
      drainLoop [] _ acc = reverse acc
      drainLoop (n:qs) degs acc =
        let dependents = Map.findWithDefault [] n adjMap
            updateDeg (dMap, newQ) dep =
              let curDeg = Map.findWithDefault 1 dep dMap
                  newDeg = max 0 (curDeg - 1)
                  nextDMap = Map.insert dep newDeg dMap
              in if newDeg == 0
                   then (nextDMap, dep : newQ)
                   else (nextDMap, newQ)
            (nextDegs, newlyFree) = foldl updateDeg (degs, []) dependents
            nextQueue = qs <> sort newlyFree
        in drainLoop nextQueue nextDegs (n : acc)

      sortedOrder = drainLoop initialQueue inDegrees []
      -- Include any remaining nodes if cycles exist
      remaining = filter (\n -> not (n `elem` sortedOrder)) allNames
  in sortedOrder <> remaining

-- | Recursively traverses a directory path to discover all Haskell source files (.hs and .lhs).
discoverSourceFiles :: FilePath -> IO [FilePath]
discoverSourceFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entriesRes <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
      case entriesRes of
        Left _ -> pure []
        Right entries -> do
          let validEntries = filter (\e -> not ("." `isSuffixOf` e || ".git" == e || "dist-newstyle" == e)) entries
          paths <- mapM (\e -> do
            let fullPath = dir </> e
            isDir <- doesDirectoryExist fullPath
            isFile <- doesFileExist fullPath
            if isDir
              then discoverSourceFiles fullPath
              else if isFile && (".hs" `isSuffixOf` e || ".lhs" `isSuffixOf` e)
                     then pure [fullPath]
                     else pure []
            ) validEntries
          pure (concat paths)

-- | Scans a source directory upfront to discover all modules and their mutual dependency edges.
discoverUnitGraph :: FilePath -> IO ([ModuleNode], [GraphEdge])
discoverUnitGraph sourceDir = do
  files <- discoverSourceFiles sourceDir
  nodesWithImports <- mapM readNodeAndImports files
  let validNodesWithImports = mapMaybe id nodesWithImports
      nodes = map fst validNodesWithImports
      knownModuleNames = Set.fromList (map mnName nodes)
      buildEdges (node, imps) =
        [ GraphEdge
            { geSource         = Just (mnName node)
            , geTarget         = impName
            , geIsSourceImport = isSrc
            }
        | (impName, isSrc) <- imps
        , Set.member impName knownModuleNames
        , impName /= mnName node
        ]
      edges = concatMap buildEdges validNodesWithImports
  pure (nodes, edges)
  where
    readNodeAndImports filePath = do
      contentRes <- try (TIO.readFile filePath) :: IO (Either SomeException Text)
      case contentRes of
        Left _ -> pure Nothing
        Right content -> do
          let mName = case parseModuleHeader content of
                Just n  -> n
                Nothing -> "Main"
              isBoot = ".hs-boot" `isSuffixOf` filePath
              node = ModuleNode
                { mnName       = mName
                , mnSourcePath = filePath
                , mnIsBoot     = isBoot
                }
              imports = parseImports content
          pure (Just (node, imports))
