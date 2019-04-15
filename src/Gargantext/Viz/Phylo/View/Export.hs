{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Viz.Phylo.View.Export
  where

import Control.Lens  hiding (Level)   
import Control.Monad
import Data.GraphViz   hiding (DotGraph)
import Data.GraphViz.Attributes.Complete hiding (EdgeType) 
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic
import Data.List        ((++),unwords,concat,sortOn,nub)
import Data.Map         (Map,toList)
import Data.Maybe       (isNothing,fromJust)
import Data.Text.Lazy   (fromStrict, pack, unpack)

import qualified Data.Text as T
import qualified Data.Text.Lazy as T'
import qualified Data.GraphViz.Attributes.HTML as H

import Gargantext.Prelude
import Gargantext.Viz.Phylo hiding (Dot)
import Gargantext.Viz.Phylo.Tools


import Prelude (writeFile)
import System.FilePath

type DotId = T'.Text


---------------------
-- | Dot to File | --
---------------------

dotToFile :: FilePath -> FilePath -> DotGraph DotId -> IO ()
dotToFile filePath fileName dotG = writeFile (combine filePath fileName) $ unpack (printDotGraph dotG)


--------------------------
-- | PhyloView to DOT | --
--------------------------

-- | From http://haroldcarr.com/posts/2014-02-28-using-graphviz-via-haskell.html & https://hackage.haskell.org/package/graphviz


-- | To create a custom Graphviz's Attribute
setAttr :: AttributeName -> T'.Text -> CustomAttribute
setAttr k v = customAttribute k v


-- | To create customs Graphviz's Attributes out of some Metrics
setAttrFromMetrics :: Map T.Text [Double] -> [CustomAttribute]
setAttrFromMetrics a = map (\(k,v) -> setAttr (fromStrict k) 
                                    $ (pack . unwords) 
                                    $ map show v) $ toList a


-- | To transform a PhyloBranchId into a DotId
toBranchDotId :: PhyloBranchId -> DotId
toBranchDotId (lvl,idx) = fromStrict $ T.pack $ (show lvl) ++ (show idx)


-- | To transform a PhyloGroupId into a DotId
toNodeDotId :: PhyloGroupId -> DotId
toNodeDotId (((d,d'),lvl),idx) = fromStrict $ T.pack $ (show d) ++ (show d') ++ (show lvl) ++ (show idx)


-- | To transform a PhyloPeriodId into a DotId
toPeriodDotId :: PhyloPeriodId -> DotId
toPeriodDotId (d,d') = fromStrict $ T.pack $ (show d) ++ (show d')


-- | To transform a PhyloPeriodId into a Graphviz's label
toPeriodDotLabel ::PhyloPeriodId -> Label
toPeriodDotLabel (d,d') = toDotLabel $ T.pack $ (show d) ++ " " ++ (show d')


-- | To get all the Phyloperiods covered by a PhyloView
getViewPeriods :: PhyloView -> [PhyloPeriodId]
getViewPeriods pv = sortOn fst $ nub $ map (\pn -> (fst . fst) $ pn ^. pn_id) $ pv ^. pv_nodes


-- | To get for each PhyloBranch, their corresponding oldest PhyloNodes
getFirstNodes :: Level -> PhyloView -> [(PhyloBranchId,PhyloGroupId)]
getFirstNodes lvl pv = concat
                     $ map (\bId -> map (\pn -> (bId,pn ^. pn_id))
                                  $ filterNodesByFirstPeriod 
                                  $ filterNodesByBranch bId
                                  $ filterNodesByLevel lvl 
                                  $ pv ^. pv_nodes) bIds
    where
        --------------------------------------
        bIds :: [PhyloBranchId]
        bIds = map getBranchId $ filterBranchesByLevel lvl pv
        --------------------------------------


-- | To transform a Text into a Graphviz's Label
toDotLabel :: T.Text -> Label
toDotLabel lbl = StrLabel $ fromStrict lbl


-- | To set a Peak Node
setPeakDotNode :: PhyloBranch -> Dot DotId
setPeakDotNode pb = node (toBranchDotId $ pb ^. pb_id) 
                      ([FillColor [toWColor CornSilk], FontName "Arial", FontSize 40, Shape Egg, Style [SItem Bold []], Label (toDotLabel $ pb ^. pb_peak)]
                       <> (setAttrFromMetrics $ pb ^. pb_metrics))


-- | To set a Peak Edge
setPeakDotEdge ::  DotId -> DotId -> Dot DotId
setPeakDotEdge bId nId = edge bId nId [Width 3, Color [toWColor Black], ArrowHead (AType [(ArrMod FilledArrow RightSide,DotArrow)])]


-- | To set an HTML table
setHtmlTable :: PhyloNode -> H.Label
setHtmlTable pn = H.Table H.HTable
                    { H.tableFontAttrs = Just [H.PointSize 14, H.Align H.HLeft]
                    , H.tableAttrs = [H.Border 0, H.CellBorder 0, H.BGColor (toColor White)]
                    , H.tableRows = [header] <> (if isNothing $ pn ^. pn_ngrams
                                                 then []
                                                 else map ngramsToRow $ splitEvery 4 $ fromJust $ pn ^. pn_ngrams) }
    where
        --------------------------------------
        ngramsToRow :: [Ngrams] -> H.Row
        ngramsToRow ns = H.Cells $ map (\n -> H.LabelCell [H.BAlign H.HLeft] $ H.Text [H.Str $ fromStrict n]) ns
        --------------------------------------
        header :: H.Row
        header = H.Cells [H.LabelCell [H.Color (toColor Black), H.BGColor (toColor Chartreuse2)] 
                                      $ H.Text [H.Str $ (fromStrict . T.toUpper) $ pn ^. pn_label]]
        --------------------------------------


-- | To set a Node
setDotNode :: PhyloNode -> Dot DotId
setDotNode pn = node (toNodeDotId $ pn ^. pn_id)
                     ([FontName "Arial", Shape Square, toLabel (setHtmlTable pn)])


-- | To set an Edge
setDotEdge :: PhyloEdge -> Dot DotId
setDotEdge pe = edge (toNodeDotId $ pe ^. pe_source) (toNodeDotId $ pe ^. pe_target)  [Width 2, Color [toWColor Black]]


-- | To set a Period Edge
setDotPeriodEdge :: (PhyloPeriodId,PhyloPeriodId) -> Dot DotId
setDotPeriodEdge (prd,prd') = edge (toPeriodDotId prd) (toPeriodDotId prd') [Width 5, Color [toWColor Black]]


-- | To transform a given PhyloView into the corresponding GraphViz Graph (ie: Dot format)
viewToDot :: PhyloView -> DotGraph DotId
viewToDot pv = digraph ((Str . fromStrict) $ pv ^. pv_title) 
                 
             $ do

                -- set the global graph attributes

                graphAttrs ( [Label (toDotLabel $ pv ^. pv_title)]
                          <> [setAttr "description" $ fromStrict $ pv ^. pv_description]
                          <> [setAttr "filiation"   $ (pack . show) $ pv ^. pv_filiation]
                          <> (setAttrFromMetrics $ pv ^. pv_metrics)
                          <> [FontSize 30, LabelLoc VTop, Splines SplineEdges, Overlap ScaleOverlaps,
                              Ratio AutoRatio, Style [SItem Filled []],Color [toWColor White]])

                -- set the peaks

                subgraph (Str "Peaks") $ do 

                    graphAttrs [Rank SameRank]

                    mapM setPeakDotNode $ filterBranchesByLevel (pv ^. pv_level) pv

                -- set the nodes, period by period

                _ <- mapM (\prd ->
                        subgraph (Str $ fromStrict $ T.pack $ "subGraph " ++ (show $ (fst prd)) ++ (show $ (snd prd))) 

                        $ do
                            
                            graphAttrs [Rank SameRank]

                            -- set the period label
                            
                            node (toPeriodDotId prd) [Shape Square, FontSize 50, Label (toPeriodDotLabel prd)]

                            mapM setDotNode $ filterNodesByPeriod prd $ filterNodesByLevel (pv ^. pv_level) (pv ^.pv_nodes)
                          
                     ) $ (pv ^. pv_periods)

                -- set the edges : from peaks to nodes, from nodes to nodes, from periods to periods 

                _ <- mapM (\(bId,nId) -> setPeakDotEdge (toBranchDotId bId) (toNodeDotId nId)) $ getFirstNodes (pv ^. pv_level) pv

                _ <- mapM setDotEdge $ filterEdgesByLevel (pv ^. pv_level) $ filterEdgesByType PeriodEdge (pv ^. pv_edges)

                mapM setDotPeriodEdge $ listToSequentialCombi $ (pv ^. pv_periods)





                




