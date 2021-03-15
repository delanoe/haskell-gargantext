{-|
Module      : Gargantext.Core.Viz.Phylo.Main
Description : Phylomemy Main
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ViewPatterns      #-}

module Gargantext.Core.Viz.Phylo.Legacy.LegacyMain
  where


-- import Data.GraphViz
-- import qualified Data.ByteString as DB
import qualified Data.List as List
import qualified Data.Map  as Map
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Debug.Trace (trace)
import GHC.IO (FilePath)

import Gargantext.API.Ngrams.Tools (getTermsWith)
import Gargantext.Core.Types
import Gargantext.Database.Action.Flow
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Query.Table.Node(defaultList)
import Gargantext.Database.Query.Table.NodeNode (selectDocs)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Text.Terms.WithList

import Gargantext.Core.Viz.LegacyPhylo hiding (Svg, Dot)
-- import Gargantext.Core.Viz.Phylo.LevelMaker (toPhylo)
-- import Gargantext.Core.Viz.Phylo.Tools
-- import Gargantext.Core.Viz.Phylo.View.Export
-- import Gargantext.Core.Viz.Phylo.View.ViewMaker    -- TODO Just Maker is fine


type MinSizeBranch = Int

flowPhylo :: FlowCmdM env err m
          => CorpusId
          -> m Phylo
flowPhylo cId = do

  list     <- defaultList cId
  termList <- Map.toList <$> getTermsWith Text.words [list] NgramsTerms MapTerm

  docs' <- catMaybes
        <$> map (\h -> (,) <$> _hd_publication_year h
                           <*> _hd_abstract h
                )
        <$> selectDocs cId

  let
    patterns = buildPatterns termList
    -- | To filter the Ngrams of a document based on the termList
    filterTerms :: Patterns -> (Date, Text) -> (Date, [Text])
    filterTerms patterns' (y,d) = (y,termsInText patterns' d)
      where
        --------------------------------------
        termsInText :: Patterns -> Text -> [Text]
        termsInText pats txt = List.nub
                             $ List.concat
                             $ map (map Text.unwords)
                             $ extractTermsWithList pats txt
        --------------------------------------

    docs = map ((\(y,t) -> Document y t) . filterTerms patterns) docs'

  --liftBase $ flowPhylo' (List.sortOn date docs) termList l m fp
  pure $ buildPhylo (List.sortOn date docs) termList


-- TODO SortedList Document
flowPhylo' :: [Document] -> TermList      -- ^Build
           -> Level       -> MinSizeBranch -- ^View
           -> FilePath
           -> IO FilePath
flowPhylo' corpus terms l m fp = do
  let
    phylo = buildPhylo corpus terms
    phVie = viewPhylo  l m phylo

  writePhylo fp phVie


defaultQuery :: PhyloQueryBuild
defaultQuery = undefined
-- defaultQuery = defaultQueryBuild'
--   "Default Title"
--   "Default Description"

buildPhylo :: [Document] -> TermList -> Phylo
buildPhylo = trace (show defaultQuery) $ buildPhylo' defaultQuery

buildPhylo' :: PhyloQueryBuild -> [Document] -> TermList -> Phylo
buildPhylo' _ _ _ = undefined
-- buildPhylo' q corpus termList = toPhylo q corpus termList Map.empty

-- refactor 2021
-- queryView :: Level -> MinSizeBranch -> PhyloQueryView
-- queryView level _minSizeBranch = PhyloQueryView level Merge False 2
--            [BranchAge]
--            []
--            -- [SizeBranch $ SBParams minSizeBranch]
--            [BranchPeakFreq,GroupLabelCooc]
--            (Just (ByBranchAge,Asc))
--            Json Flat True

queryView :: Level -> MinSizeBranch -> PhyloQueryView
queryView _level _minSizeBranch = undefined

viewPhylo :: Level -> MinSizeBranch -> Phylo -> PhyloView
viewPhylo _l _b _phylo = undefined
-- viewPhylo l b phylo = toPhyloView (queryView l b) phylo

writePhylo :: FilePath -> PhyloView -> IO FilePath
writePhylo _fp _phview = undefined
-- writePhylo fp phview = runGraphviz (viewToDot phview) Svg fp

-- refactor 2021
-- viewPhylo2Svg :: PhyloView -> IO DB.ByteString
-- viewPhylo2Svg p = graphvizWithHandle Dot (viewToDot p) Svg DB.hGetContents

