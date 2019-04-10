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
import Data.GraphViz.Types 
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic
import Data.List       ((++),unwords,concat,sortOn,nub,nubBy)
import Data.Map        (Map,mapWithKey,elems,toList)
import Data.Maybe      (isJust,isNothing,fromJust)
import Data.Text       (Text)
import Data.Text.Lazy  (Text, fromStrict, pack)
import GHC.TypeLits    (KnownNat)

import qualified Data.Text as T
import qualified Data.Text.Lazy as T'
import qualified Data.GraphViz.Attributes.HTML as H

import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

type DotId = T'.Text

--------------------------
-- | PhyloView to SVG | --
--------------------------


viewToSvg v = undefined


--------------------------
-- | PhyloView to DOT | --
--------------------------

-- From http://haroldcarr.com/posts/2014-02-28-using-graphviz-via-haskell.html

setAttr :: AttributeName -> T'.Text -> CustomAttribute
setAttr k v = customAttribute k v

setAttrFromMetrics :: Map T.Text [Double] -> [CustomAttribute]
setAttrFromMetrics attrs = map (\(k,v) -> setAttr (fromStrict k) 
                                                $ (pack . unwords) 
                                                $ map show v) $ toList attrs

getBranchDotId :: PhyloBranchId -> DotId
getBranchDotId (lvl,idx) = fromStrict $ T.pack $ (show lvl) ++ (show idx)

getNodeDotId :: PhyloGroupId -> DotId
getNodeDotId (((d,d'),lvl),idx) = fromStrict $ T.pack $ (show d) ++ (show d') ++ (show lvl) ++ (show idx)

getPeriodDotId :: PhyloPeriodId -> DotId
getPeriodDotId (d,d') = fromStrict $ T.pack $ (show d) ++ (show d')

getPeriodDotLabel ::PhyloPeriodId -> Label
getPeriodDotLabel (d,d') = toDotLabel $ T.pack $ (show d) ++ " " ++ (show d')

getBranchesByLevel :: Level -> PhyloView -> [PhyloBranch]
getBranchesByLevel lvl pv = filter (\pb -> lvl == (fst $ pb ^. pb_id)) 
                          $ pv ^. pv_branches

filterNodesByPeriod :: PhyloPeriodId -> [PhyloNode] -> [PhyloNode]
filterNodesByPeriod prd pns = filter (\pn -> prd == ((fst . fst) $ pn ^. pn_id)) pns

filterNodesByLevel :: Level -> [PhyloNode] -> [PhyloNode]
filterNodesByLevel lvl pns = filter (\pn -> lvl == ((snd . fst) $ pn ^. pn_id)) pns


filterNodesByBranch :: PhyloBranchId -> [PhyloNode] -> [PhyloNode]
filterNodesByBranch bId pns = filter (\pn -> if isJust $ pn ^. pn_bid
                                             then if bId == (fromJust $ pn ^. pn_bid)
                                                  then True
                                                  else False
                                             else False ) pns   


filterEdgesByType :: EdgeType -> [PhyloEdge] -> [PhyloEdge]
filterEdgesByType t pes = filter (\pe -> t == (pe ^. pe_type)) pes

filterEdgesByLevel :: Level -> [PhyloEdge] -> [PhyloEdge]
filterEdgesByLevel lvl pes = filter (\pe -> (lvl == ((snd . fst) $ pe ^. pe_source))
                                         && (lvl == ((snd . fst) $ pe ^. pe_target))) pes


filterNodesByFirstPeriod :: [PhyloNode] -> [PhyloNode]
filterNodesByFirstPeriod pns = filter (\pn -> fstPrd == ((fst . fst) $ pn ^. pn_id)) pns
    where 
        --------------------------------------
        fstPrd :: (Date,Date)
        fstPrd = (head' "filterNodesByFirstPeriod")
               $ sortOn fst 
               $ map (\pn -> (fst . fst) $ pn ^. pn_id) pns 
        --------------------------------------


getViewPeriods :: PhyloView -> [PhyloPeriodId]
getViewPeriods pv = sortOn fst $ nub $ map (\pn -> (fst . fst) $ pn ^. pn_id) $ pv ^. pv_nodes


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
        bIds = map getBranchId $ getBranchesByLevel lvl pv
        --------------------------------------


toDotLabel :: T.Text -> Label
toDotLabel lbl = StrLabel $ fromStrict lbl

setPeakDotNode :: PhyloBranch -> Dot DotId
setPeakDotNode pb = node (getBranchDotId $ pb ^. pb_id) 
                      ([FillColor [toWColor CornSilk], FontName "Arial", FontSize 40, Shape Egg, Style [SItem Bold []], Label (toDotLabel $ pb ^. pb_label)]
                       <> (setAttrFromMetrics $ pb ^. pb_metrics))

setPeakDotEdge ::  DotId -> DotId -> Dot DotId
setPeakDotEdge bId nId = edge bId nId [Width 3, Color [toWColor Black], ArrowHead (AType [(ArrMod FilledArrow RightSide,DotArrow)])]

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


setDotNode :: PhyloNode -> Dot DotId
setDotNode pn = node (getNodeDotId $ pn ^. pn_id)
                     ([FontName "Arial", Shape Square, toLabel (setHtmlTable pn)])


setDotEdge :: PhyloEdge -> Dot DotId
setDotEdge pe = edge (getNodeDotId $ pe ^. pe_source) (getNodeDotId $ pe ^. pe_target)  [Width 2, Color [toWColor Black]]

setDotPeriodEdge :: (PhyloPeriodId,PhyloPeriodId) -> Dot DotId
setDotPeriodEdge (prd,prd') = edge (getPeriodDotId prd) (getPeriodDotId prd') [Width 5, Color [toWColor Black]]


viewToDot :: PhyloView -> Level -> DotGraph DotId
viewToDot pv lvl = digraph ((Str . fromStrict) $ pv ^. pv_title) 
                 
                 $ do

                    -- set the global graph attributes

                    graphAttrs ( [Label (toDotLabel $ pv ^. pv_title)]
                              <> [setAttr "description" $ fromStrict $ pv ^. pv_description]
                              <> [setAttr "filiation"   $ (pack . show) $ pv ^. pv_filiation]
                              <> (setAttrFromMetrics $ pv ^. pv_metrics)
                              <> [FontSize (fromIntegral 30), LabelLoc VTop, Splines SplineEdges, Overlap ScaleOverlaps,
                                  Ratio AutoRatio, Style [SItem Filled []],Color [toWColor White]])

                    -- set the peaks

                    subgraph (Str "Peaks") 

                    $ do 

                        graphAttrs [Rank SameRank]

                        mapM setPeakDotNode $ getBranchesByLevel lvl pv

                    -- set the nodes, period by period

                    mapM (\prd ->
                            subgraph (Str $ fromStrict $ T.pack $ "subGraph " ++ (show $ (fst prd)) ++ (show $ (snd prd))) 

                            $ do
                                
                                graphAttrs [Rank SameRank]

                                -- set the period label
                                
                                node (getPeriodDotId prd) [Shape Square, FontSize 50, Label (getPeriodDotLabel prd)]

                                mapM setDotNode $ filterNodesByPeriod prd $ filterNodesByLevel lvl (pv ^.pv_nodes)
                              
                         ) $ getViewPeriods pv

                    -- set the edges : from peaks to nodes, from nodes to nodes, from periods to periods 

                    mapM (\(bId,nId) -> setPeakDotEdge (getBranchDotId bId) (getNodeDotId nId)) $ getFirstNodes lvl pv

                    mapM setDotEdge $ filterEdgesByLevel lvl $ filterEdgesByType PeriodEdge (pv ^. pv_edges)

                    mapM setDotPeriodEdge $ listToSequentialCombi $ getViewPeriods pv




                




