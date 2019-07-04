{-|
Module      : Gargantext.Viz.Phylo.Main
Description : Phylomemy Main
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module Gargantext.Viz.Phylo.Main
  where

import Debug.Trace (trace)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Maybe
import Servant
import GHC.IO (FilePath)
import Data.GraphViz
import Gargantext.Prelude
import Gargantext.Text.Context (TermList)
import qualified Data.Map  as Map
import qualified Data.List as List
import Gargantext.Viz.Phylo.View.Export
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.LevelMaker
import Gargantext.Core.Types
import Gargantext.Text.Terms.WithList
-- import Gargantext.Database.Config (userMaster)
import Gargantext.Database.Schema.Node (defaultList)
import Gargantext.Database.Schema.NodeNode (selectDocs)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
-- import Gargantext.Database.Metrics.NgramsByNode (getNodesByNgramsOnlyUser)
-- import Gargantext.Database.Node.Select (selectNodesWithUsername)
import Gargantext.Database.Flow
import Gargantext.API.Ngrams.Tools (getTermsWith)
-- TODO : git mv ViewMaker Maker
import Gargantext.Viz.Phylo.View.ViewMaker
import Gargantext.Viz.Phylo hiding (Svg, Dot)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as DB

type MinSizeBranch = Int

flowPhylo :: FlowCmdM env ServantErr m
          => CorpusId
          -> Level -> MinSizeBranch
          -> FilePath
          -> m FilePath
flowPhylo cId l m fp = do

  list       <- defaultList cId
  -- listMaster <- selectNodesWithUsername NodeList userMaster
  termList <- Map.toList <$> getTermsWith Text.words [list] NgramsTerms GraphTerm
  --printDebug "termList" termList
  
  --x <- mapTermListRoot [list] NgramsTerms
  --printDebug "mapTermListRoot" x
  
  -- TODO optimize unwords

  docs' <- catMaybes <$> map (\h -> (,) <$> _hyperdataDocument_publication_year h
                          <*> _hyperdataDocument_abstract h
                          ) <$> selectDocs cId
  
  let patterns = buildPatterns termList
  let docs = map ( (\(y,t) -> Document y t) . filterTerms patterns) docs'
  --printDebug "docs" docs
  --printDebug "docs" termList

  liftIO $ flowPhylo' (List.sortOn date docs) termList l m fp




parse :: TermList -> [(Date, Text)] -> IO [Document]
parse l c = do
  let patterns = buildPatterns l
  pure $ map ( (\(y,t) -> Document y t) . filterTerms patterns) c


-- | To filter the Ngrams of a document based on the termList
filterTerms :: Patterns -> (Date, Text) -> (Date, [Text])
filterTerms patterns (y,d) = (y,termsInText patterns d)
  where
    --------------------------------------
    termsInText :: Patterns -> Text -> [Text]
    termsInText pats txt = List.nub $ List.concat $ map (map Text.unwords) $ extractTermsWithList pats txt
    --------------------------------------


-- TODO SortedList Document
flowPhylo' :: [Document] -> TermList      -- ^Build
          -> Level      -> MinSizeBranch -- ^View
          -> FilePath
          -> IO FilePath
flowPhylo' corpus terms l m fp = do
  let
    phylo = buildPhylo corpus terms
    phVie = viewPhylo  l m phylo

  writePhylo fp phVie


defaultQuery :: PhyloQueryBuild
defaultQuery = defaultQueryBuild'
  "Default Title"
  "Default Description"

buildPhylo :: [Document] -> TermList -> Phylo
buildPhylo = trace (show defaultQuery) $ buildPhylo' defaultQuery

buildPhylo' :: PhyloQueryBuild -> [Document] -> TermList -> Phylo
buildPhylo' q corpus termList = toPhylo q corpus termList Map.empty

queryView :: Level -> MinSizeBranch -> PhyloQueryView
queryView level _minSizeBranch = PhyloQueryView level Merge False 2
           [BranchAge]
           []
           -- [SizeBranch $ SBParams minSizeBranch]
           [BranchPeakFreq,GroupLabelCooc]
           (Just (ByBranchAge,Asc))
           Json Flat True

viewPhylo :: Level -> MinSizeBranch -> Phylo -> PhyloView
viewPhylo l b phylo = toPhyloView (queryView l b) phylo

writePhylo :: FilePath -> PhyloView -> IO FilePath
writePhylo fp phview = runGraphviz (viewToDot phview) Svg fp

viewPhylo2Svg :: PhyloView -> IO DB.ByteString
viewPhylo2Svg p = graphvizWithHandle Dot (viewToDot p) Svg DB.hGetContents

