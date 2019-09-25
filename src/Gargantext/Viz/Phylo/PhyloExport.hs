{-|
Module      : Gargantext.Viz.Phylo.PhyloExport
Description : Exportation module of a Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Gargantext.Viz.Phylo.PhyloExport where

import Data.Map (Map, fromList, empty, fromListWith, insert, (!), elems, unionWith, findWithDefault, toList)
import Data.List ((++), sort, nub, concat, sortOn, reverse, groupBy, union, (\\), (!!), init, partition, unwords, nubBy)
import Data.Vector (Vector)

import Prelude (writeFile)
import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools 

import Control.Lens
import Data.GraphViz hiding (DotGraph, Order)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Attributes.Complete hiding (EdgeType, Order) 
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy (fromStrict, pack, unpack)
import System.FilePath
import Debug.Trace (trace)

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.GraphViz.Attributes.HTML as H

--------------------
-- | Dot export | --
--------------------

dotToFile :: FilePath -> DotGraph DotId -> IO ()
dotToFile filePath dotG = writeFile filePath $ dotToString dotG

dotToString :: DotGraph DotId  -> [Char]
dotToString dotG = unpack (printDotGraph dotG)

dynamicToColor :: Double -> H.Attribute
dynamicToColor d 
  | d == 0    = H.BGColor (toColor LightCoral)
  | d == 1    = H.BGColor (toColor Khaki)
  | d == 2    = H.BGColor (toColor SkyBlue)
  | otherwise = H.Color   (toColor Black)

pickLabelColor :: [Double] -> H.Attribute
pickLabelColor lst
  | elem 0 lst = dynamicToColor 0
  | elem 2 lst = dynamicToColor 2
  | elem 1 lst = dynamicToColor 1
  | otherwise  = dynamicToColor 3  

toDotLabel :: Text.Text -> Label
toDotLabel lbl = StrLabel $ fromStrict lbl

toAttr :: AttributeName -> Lazy.Text -> CustomAttribute
toAttr k v = customAttribute k v

metaToAttr :: Map Text.Text [Double] -> [CustomAttribute]
metaToAttr meta = map (\(k,v) -> toAttr (fromStrict k) $ (pack . unwords) $ map show v) $ toList meta

groupIdToDotId :: PhyloGroupId -> DotId
groupIdToDotId (((d,d'),lvl),idx) = (fromStrict . Text.pack) $ ("group" <> (show d) <> (show d') <> (show lvl) <> (show idx))

branchIdToDotId :: PhyloBranchId -> DotId
branchIdToDotId bId = (fromStrict . Text.pack) $ ("branch" <> show (snd bId))

periodIdToDotId :: PhyloPeriodId -> DotId
periodIdToDotId prd = (fromStrict . Text.pack) $ ("period" <> show (fst prd) <> show (snd prd))

groupToTable :: Vector Ngrams -> PhyloGroup -> H.Label
groupToTable fdt g = H.Table H.HTable
                    { H.tableFontAttrs = Just [H.PointSize 14, H.Align H.HLeft]
                    , H.tableAttrs = [H.Border 0, H.CellBorder 0, H.BGColor (toColor White)]
                    , H.tableRows = [header]
                                 <> [H.Cells [H.LabelCell [H.Height 10] $ H.Text [H.Str $ fromStrict ""]]]
                                 <> ( map ngramsToRow $ splitEvery 4 
                                    $ reverse $ sortOn (snd . snd)
                                    $ zip (ngramsToText fdt (g ^. phylo_groupNgrams)) 
                                    $ zip ((g ^. phylo_groupMeta) ! "dynamics") ((g ^. phylo_groupMeta) ! "inclusion"))}
    where
        --------------------------------------
        ngramsToRow :: [(Ngrams,(Double,Double))] -> H.Row
        ngramsToRow ns = H.Cells $ map (\(n,(d,_)) -> 
                            H.LabelCell [H.Align H.HLeft,dynamicToColor d] $ H.Text [H.Str $ fromStrict n]) ns
        --------------------------------------
        header :: H.Row
        header = 
            H.Cells [ H.LabelCell [pickLabelColor ((g ^. phylo_groupMeta) ! "dynamics")] 
                    $ H.Text [H.Str $ (((fromStrict . Text.toUpper) $ g ^. phylo_groupLabel)
                                   <> (fromStrict " ( ")
                                   <> (pack $ show (fst $ g ^. phylo_groupPeriod))
                                   <> (fromStrict " , ")
                                   <> (pack $ show (snd $ g ^. phylo_groupPeriod))
                                   <> (fromStrict " ) "))]] 
        --------------------------------------

branchToDotNode :: PhyloBranch -> Dot DotId
branchToDotNode b = 
    node (branchIdToDotId $ b ^. branch_id)
         ([FillColor [toWColor CornSilk], FontName "Arial", FontSize 40, Shape Egg, Style [SItem Bold []], Label (toDotLabel $ b ^. branch_label)]
         <> (metaToAttr $ b ^. branch_meta)
         <> [ toAttr "nodeType" "branch"
            , toAttr "branchId" (pack $ show (snd $ b ^. branch_id)) ])
 
periodToDotNode :: (Date,Date) -> Dot DotId
periodToDotNode prd =
    node (periodIdToDotId prd)
         ([Shape Square, FontSize 50, Label (toDotLabel $ Text.pack (show (fst prd) <> " " <> show (snd prd)))]
         <> [ toAttr "nodeType" "period" 
            , toAttr "from" (fromStrict $ Text.pack $ (show $ fst prd))
            , toAttr "to"   (fromStrict $ Text.pack $ (show $ snd prd))])


groupToDotNode :: Vector Ngrams -> PhyloGroup -> Dot DotId
groupToDotNode fdt g = 
    node (groupIdToDotId $ getGroupId g)
                     ([FontName "Arial", Shape Square, toLabel (groupToTable fdt g)]
                      <> [ toAttr "nodeType" "group"
                         , toAttr "from" (pack $ show (fst $ g ^. phylo_groupPeriod))
                         , toAttr "to"   (pack $ show (snd $ g ^. phylo_groupPeriod))
                         , toAttr "branchId" (pack $ show (snd $ g ^. phylo_groupBranchId))])  


toDotEdge :: DotId -> DotId -> Text.Text -> EdgeType -> Dot DotId
toDotEdge source target lbl edgeType = edge source target
    (case edgeType of
        GroupToGroup   -> [ Width 2, Color [toWColor Black], Constraint True
                          , Label (StrLabel $ fromStrict lbl)]
        BranchToGroup  -> [ Width 3, Color [toWColor Black], ArrowHead (AType [(ArrMod FilledArrow RightSide,DotArrow)])
                          , Label (StrLabel $ fromStrict lbl)]
        BranchToBranch -> [ Width 2, Color [toWColor Black], Style [SItem Dashed []], ArrowHead (AType [(ArrMod FilledArrow BothSides,DotArrow)])
                          , Label (StrLabel $ fromStrict lbl)]
        PeriodToPeriod -> [ Width 5, Color [toWColor Black]])


mergePointers :: [PhyloGroup] -> Map (PhyloGroupId,PhyloGroupId) Double
mergePointers groups = 
    let toChilds  = fromList $ concat $ map (\g -> map (\(target,w) -> ((getGroupId g,target),w)) $ g ^. phylo_groupPeriodChilds) groups
        toParents = fromList $ concat $ map (\g -> map (\(target,w) -> ((target,getGroupId g),w)) $ g ^. phylo_groupPeriodParents) groups
    in  unionWith (\w w' -> max w w') toChilds toParents


exportToDot :: Phylo -> PhyloExport -> DotGraph DotId
exportToDot phylo export = 
    digraph ((Str . fromStrict) $ (phyloName $ getConfig phylo)) $ do 

        -- | 1) init the dot graph
        graphAttrs ( [ Label (toDotLabel $ (phyloName $ getConfig phylo))]
                  <> [ FontSize 30, LabelLoc VTop, NodeSep 1, RankSep [1], Rank SameRank, Splines SplineEdges, Overlap ScaleOverlaps
                     , Ratio FillRatio
                     , Style [SItem Filled []],Color [toWColor White]
                     , (toAttr (fromStrict "nbDocs") $ pack $ show (sum $ elems $ phylo ^. phylo_timeDocs))])


 -- toAttr (fromStrict k) $ (pack . unwords) $ map show v

        -- | 2) create a layer for the branches labels
        subgraph (Str "Branches peaks") $ do 

            graphAttrs [Rank SameRank]

            -- | 3) group the branches by hierarchy
            mapM (\branches -> 
                    subgraph (Str "Branches clade") $ do
                        graphAttrs [Rank SameRank]

                        -- | 4) create a node for each branch
                        mapM branchToDotNode branches
                ) $ elems $ fromListWith (++) $ map (\b -> ((init . snd) $ b ^. branch_id,[b])) $ export ^. export_branches

        -- | 5) create a layer for each period
        _ <- mapM (\period ->
                subgraph ((Str . fromStrict . Text.pack) $ ("Period" <> show (fst period) <> show (snd period))) $ do 
                    graphAttrs [Rank SameRank]
                    periodToDotNode period

                    -- | 6) create a node for each group 
                    mapM (\g -> groupToDotNode (getRoots phylo) g) (filter (\g -> g ^. phylo_groupPeriod == period) $ export ^. export_groups)
            ) $ getPeriodIds phylo

        -- | 7) create the edges between a branch and its first groups
        _ <- mapM (\(bId,groups) ->
                mapM (\g -> toDotEdge (branchIdToDotId bId) (groupIdToDotId $ getGroupId g) "" BranchToGroup) groups 
             )
           $ toList
           $ map (\groups -> head' "toDot" 
                           $ groupBy (\g g' -> g' ^. phylo_groupPeriod == g ^. phylo_groupPeriod)
                           $ sortOn (fst . _phylo_groupPeriod) groups) 
           $ fromListWith (++) $ map (\g -> (g ^. phylo_groupBranchId,[g])) $ export ^. export_groups

        -- | 8) create the edges between the groups
        _ <- mapM (\((k,k'),_) -> 
                toDotEdge (groupIdToDotId k) (groupIdToDotId k') "" GroupToGroup
            ) $ (toList . mergePointers) $ export ^. export_groups

        -- | 7) create the edges between the periods 
        _ <- mapM (\(prd,prd') ->
                toDotEdge (periodIdToDotId prd) (periodIdToDotId prd') "" PeriodToPeriod
            ) $ nubBy (\combi combi' -> fst combi == fst combi') $ listToCombi' $ getPeriodIds phylo

        -- | 8) create the edges between the branches 
        _ <- mapM (\(bId,bId') ->
                toDotEdge (branchIdToDotId bId) (branchIdToDotId bId') 
                (Text.pack $ show(branchIdsToProximity bId bId' 
                                    (getThresholdInit $ phyloProximity $ getConfig phylo)
                                    (getThresholdStep $ phyloProximity $ getConfig phylo))) BranchToBranch
            ) $ nubBy (\combi combi' -> fst combi == fst combi') $ listToCombi' $ map _branch_id $ export ^. export_branches


        graphAttrs [Rank SameRank]


        


----------------
-- | Filter | --
----------------

filterByBranchSize :: Double -> PhyloExport -> PhyloExport
filterByBranchSize thr export = 
    let branches' = partition (\b -> head' "filter" ((b ^. branch_meta) ! "size") >= thr) $ export ^. export_branches
    in  export & export_branches .~ (fst branches')
               & export_groups %~ (filter (\g -> not $ elem  (g ^. phylo_groupBranchId) (map _branch_id $ snd branches')))


processFilters :: [Filter] -> PhyloExport -> PhyloExport
processFilters filters export = 
    foldl (\export' f -> case f of 
                ByBranchSize thr -> filterByBranchSize thr export'
        ) export filters

--------------
-- | Sort | --
--------------

sortByHierarchy :: Int -> [PhyloBranch] -> [PhyloBranch]
sortByHierarchy depth branches =
    if (length branches == 1)
        then branches
        else concat 
           $ map (\branches' ->
                    let partitions = partition (\b -> depth + 1 == ((length . snd) $ b ^. branch_id)) branches'
                    in  (sortOn (\b -> (b ^. branch_meta) ! "birth") (fst partitions))
                    ++  (sortByHierarchy (depth + 1) (snd partitions))) 
            $ groupBy (\b b' -> ((take depth . snd) $ b ^. branch_id) == ((take depth . snd) $ b' ^. branch_id) )
            $ sortOn (\b -> (take depth . snd) $ b ^. branch_id) branches


sortByBirthDate :: Order -> PhyloExport -> PhyloExport
sortByBirthDate order export = 
    let branches  = sortOn (\b -> (b ^. branch_meta) ! "birth") $ export ^. export_branches
        branches' = case order of
                    Asc  -> branches
                    Desc -> reverse branches
    in  export & export_branches .~ branches'

processSort :: Sort -> PhyloExport -> PhyloExport
processSort sort' export = case sort' of
    ByBirthDate o -> sortByBirthDate o export 
    ByHierarchy   -> export & export_branches .~ sortByHierarchy 0 (export ^. export_branches)


-----------------
-- | Metrics | --
-----------------

-- | Return the conditional probability of i knowing j 
conditional :: Ord a => Map (a,a) Double -> a -> a -> Double
conditional m i j = (findWithDefault 0 (i,j) m) 
                  / (m ! (j,j))


-- | Return the genericity score of a given ngram
genericity :: Map (Int, Int) Double -> [Int] -> Int -> Double 
genericity m l i = ( (sum $ map (\j -> conditional m i j) l) 
                   - (sum $ map (\j -> conditional m j i) l)) / (fromIntegral $ (length l) + 1)


-- | Return the specificity score of a given ngram
specificity :: Map (Int, Int) Double -> [Int] -> Int -> Double 
specificity m l i = ( (sum $ map (\j -> conditional m j i) l)
                    - (sum $ map (\j -> conditional m i j) l)) / (fromIntegral $ (length l) + 1)                  


-- | Return the inclusion score of a given ngram
inclusion :: Map (Int, Int) Double -> [Int] -> Int -> Double 
inclusion m l i = ( (sum $ map (\j -> conditional m j i) l)
                  + (sum $ map (\j -> conditional m i j) l)) / (fromIntegral $ (length l) + 1)


ngramsMetrics :: PhyloExport -> PhyloExport
ngramsMetrics export =
    over ( export_groups
         .  traverse )
    (\g -> g & phylo_groupMeta %~ insert "genericity" 
                                  (map (\n -> genericity  (g ^. phylo_groupCooc) ((g ^. phylo_groupNgrams) \\ [n]) n) $ g ^. phylo_groupNgrams)
             & phylo_groupMeta %~ insert "specificity" 
                                  (map (\n -> specificity (g ^. phylo_groupCooc) ((g ^. phylo_groupNgrams) \\ [n]) n) $ g ^. phylo_groupNgrams)
             & phylo_groupMeta %~ insert "inclusion" 
                                  (map (\n -> inclusion   (g ^. phylo_groupCooc) ((g ^. phylo_groupNgrams) \\ [n]) n) $ g ^. phylo_groupNgrams)
        ) export


branchDating :: PhyloExport -> PhyloExport
branchDating export =
    over ( export_branches
         .  traverse )
    (\b -> 
        let groups = sortOn fst
                   $ foldl' (\acc g -> if (g ^. phylo_groupBranchId == b ^. branch_id)
                                      then acc ++ [g ^. phylo_groupPeriod]
                                      else acc ) [] $ export ^. export_groups
            birth = fst $ head' "birth" groups
            age   = (snd $ last' "age"  groups) - birth 
        in b & branch_meta %~ insert "birth" [fromIntegral birth] 
             & branch_meta %~ insert "age"   [fromIntegral age]
             & branch_meta %~ insert "size"  [fromIntegral $ length groups] ) export

processMetrics :: PhyloExport -> PhyloExport
processMetrics export = ngramsMetrics
                      $ branchDating export 


-----------------
-- | Taggers | --
----------------- 

getNthMostMeta :: Int -> [Double] -> [Int] -> [Int]
getNthMostMeta nth meta ns = map (\(idx,_) -> (ns !! idx))
                           $ take nth
                           $ reverse
                           $ sortOn snd $ zip [0..] meta 


mostInclusive :: Int -> Vector Ngrams -> PhyloExport -> PhyloExport
mostInclusive nth foundations export =
    over ( export_branches
         .  traverse )
         (\b -> 
            let groups = filter (\g -> g ^. phylo_groupBranchId == b ^. branch_id) $ export ^. export_groups
                cooc   = foldl (\acc g -> unionWith (+) acc (g ^. phylo_groupCooc)) empty groups
                ngrams = sort $ foldl (\acc g -> union acc (g ^. phylo_groupNgrams)) [] groups
                inc    = map (\n -> inclusion cooc (ngrams \\ [n]) n) ngrams
                lbl    = ngramsToLabel foundations $ getNthMostMeta nth inc ngrams
            in b & branch_label .~ lbl ) export


mostEmergentInclusive :: Int -> Vector Ngrams -> PhyloExport -> PhyloExport
mostEmergentInclusive nth foundations export =
    over ( export_groups
         .  traverse ) 
         (\g -> 
            let lbl = ngramsToLabel foundations
                    $ take nth 
                    $ map (\(_,(_,idx)) -> idx)
                    $ concat
                    $ map (\groups -> sortOn (fst . snd) groups)
                    $ groupBy ((==) `on` fst) $ reverse $ sortOn fst                
                    $ zip ((g ^. phylo_groupMeta) ! "inclusion")
                    $ zip ((g ^. phylo_groupMeta) ! "dynamics") (g ^. phylo_groupNgrams)
            in g & phylo_groupLabel .~ lbl ) export


processLabels :: [PhyloLabel] -> Vector Ngrams -> PhyloExport -> PhyloExport
processLabels labels foundations export =
    foldl (\export' label -> 
                case label of
                    GroupLabel  tagger nth -> 
                        case tagger of
                            MostEmergentInclusive -> mostEmergentInclusive nth foundations export' 
                            _ -> panic "[ERR][Viz.Phylo.PhyloExport] unknown tagger"
                    BranchLabel tagger nth ->
                        case tagger of
                            MostInclusive -> mostInclusive nth foundations export'
                            _ -> panic "[ERR][Viz.Phylo.PhyloExport] unknown tagger" ) export labels 


------------------
-- | Dynamics | --
------------------ 


toDynamics :: Int -> [PhyloGroup] -> PhyloGroup -> Map Int (Date,Date) -> Double
toDynamics n parents group m = 
    let prd = group ^. phylo_groupPeriod
        end = last' "dynamics" (sort $ map snd $ elems m)
    in  if (((snd prd) == (snd $ m ! n)) && (snd prd /= end))
            -- | decrease
            then 2
        else if ((fst prd) == (fst $ m ! n))
            -- | recombination
            then 0
        else if isNew
            -- | emergence
            then 1
        else 3
    where
        -------------------------------------- 
        isNew :: Bool
        isNew = not $ elem n $ concat $ map _phylo_groupNgrams parents


processDynamics :: [PhyloGroup] -> [PhyloGroup]
processDynamics groups = 
    map (\g ->
        let parents = filter (\g' -> (g ^. phylo_groupBranchId == g' ^. phylo_groupBranchId)
                                  && ((fst $ g ^. phylo_groupPeriod) > (fst $ g' ^. phylo_groupPeriod))) groups
        in  g & phylo_groupMeta %~ insert "dynamics" (map (\n -> toDynamics n parents g mapNgrams) $ g ^. phylo_groupNgrams) ) groups
    where
        --------------------------------------
        mapNgrams :: Map Int (Date,Date)
        mapNgrams = map (\dates -> 
                        let dates' = sort dates
                        in (head' "dynamics" dates', last' "dynamics" dates'))
                  $ fromListWith (++)
                  $ foldl (\acc g -> acc ++ ( map (\n -> (n,[fst $ g ^. phylo_groupPeriod, snd $ g ^. phylo_groupPeriod])) 
                                            $ (g ^. phylo_groupNgrams))) [] groups


---------------------
-- | phyloExport | --
---------------------   


toPhyloExport :: Phylo -> DotGraph DotId
toPhyloExport phylo = exportToDot phylo
                    $ processFilters (exportFilter $ getConfig phylo)
                    $ processSort    (exportSort   $ getConfig phylo)
                    $ processLabels  (exportLabel  $ getConfig phylo) (getRoots phylo)
                    $ processMetrics  export           
    where
        export :: PhyloExport
        export = PhyloExport groups branches
        --------------------------------------
        branches :: [PhyloBranch] 
        branches = traceExportBranches $ map (\bId -> PhyloBranch bId "" empty) $ nub $ map _phylo_groupBranchId groups
        --------------------------------------    
        groups :: [PhyloGroup]
        groups = processDynamics 
               $ getGroupsFromLevel (phyloLevel $ getConfig phylo) phylo


traceExportBranches :: [PhyloBranch] -> [PhyloBranch]
traceExportBranches branches = trace ("\n" <> "-- | Export " <> show(length branches) <> " branches") branches
