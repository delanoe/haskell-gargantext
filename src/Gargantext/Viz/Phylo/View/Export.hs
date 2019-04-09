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

import Data.GraphViz hiding (DotGraph)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types 
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic
import Data.List      ((++),unwords,concat,sortOn)
import Data.Map       (Map,mapWithKey,elems,toList)
import Data.Maybe     (isJust,fromJust)
import Data.Text      (Text)
import Data.Text.Lazy (Text, fromStrict, pack)

import qualified Data.Text as T
import qualified Data.Text.Lazy as T'

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
getBranchDotId (lvl,idx) = (pack . show) $ (idx + lvl * 1000) * 100000000

getBranchesByLevel :: Level -> PhyloView -> [PhyloBranch]
getBranchesByLevel lvl pv = filter (\pb -> lvl == (fst $ pb ^. pb_id)) 
                          $ pv ^. pv_branches


filterNodesByLevel :: Level -> [PhyloNode] -> [PhyloNode]
filterNodesByLevel lvl pns = filter (\pn -> lvl == ((snd . fst) $ pn ^. pn_id)) pns


filterNodesByBranch :: PhyloBranchId -> [PhyloNode] -> [PhyloNode]
filterNodesByBranch bId pns = filter (\pn -> if isJust $ pn ^. pn_bid
                                             then if bId == (fromJust $ pn ^. pn_bid)
                                                  then True
                                                  else False
                                             else False ) pns    

filterNodesByFirstPeriod :: [PhyloNode] -> [PhyloNode]
filterNodesByFirstPeriod pns = filter (\pn -> fstPrd == ((fst . fst) $ pn ^. pn_id)) pns
    where 
        --------------------------------------
        fstPrd :: (Date,Date)
        fstPrd = (head' "filterNodesByFirstPeriod")
               $ sortOn fst 
               $ map (\pn -> (fst . fst) $ pn ^. pn_id) pns 
        --------------------------------------


getFirstNodes :: Level -> PhyloView -> [(PhyloBranchId,[PhyloGroupId])]
getFirstNodes lvl pv = map (\bId -> (bId, map (\pn -> pn ^. pn_id)
                                        $ filterNodesByFirstPeriod 
                                        $ filterNodesByBranch bId
                                        $ filterNodesByLevel lvl 
                                        $ pv ^. pv_nodes)) bIds
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
setPeakDotEdge bId nId = edge bId nId 
                              [Width 3, Color [toWColor Black], ArrowHead (AType [(ArrMod FilledArrow RightSide,DotArrow)])]

setDotNode :: PhyloNode -> Dot DotId
setDotNode pn = undefined

setDotEdge :: PhyloEdge -> Dot DotId
setDotEdge pe = undefined

setDotTime :: Date -> Date -> DotId
setDotTime d d' = undefined


viewToDot :: PhyloView -> Level -> DotGraph DotId
viewToDot pv lvl = digraph ((Str . fromStrict) $ pv ^. pv_title) 
                 $ do
                    graphAttrs ( [Label (toDotLabel $ pv ^. pv_title)]
                              <> [setAttr "description" $ fromStrict $ pv ^. pv_description]
                              <> [setAttr "filiation"   $ (pack . show) $ pv ^. pv_filiation]
                              <> (setAttrFromMetrics $ pv ^. pv_metrics)
                              <> [FontSize (fromIntegral 30), LabelLoc VTop, Splines SplineEdges, Overlap ScaleOverlaps,
                                  Ratio AutoRatio, Style [SItem Filled []],Color [toWColor White]])

                    mapM setPeakDotNode $ getBranchesByLevel lvl pv




                




