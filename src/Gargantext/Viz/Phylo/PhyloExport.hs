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

import Data.Map (Map, fromList, empty, fromListWith, insert, (!), elems, unionWith, findWithDefault)
import Data.List ((++), sort, nub, concat, sortOn, reverse, groupBy, union, (\\), (!!), init, partition)
import Data.Text (Text)
import Data.Vector (Vector)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools 

import Control.Lens
import Data.GraphViz hiding (DotGraph, Order)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Attributes.Complete hiding (EdgeType, Order) 
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy (fromStrict)

import qualified Data.Text as Text

--------------------
-- | Dot export | --
--------------------


toDotLabel :: Text.Text -> Label
toDotLabel lbl = StrLabel $ fromStrict lbl


exportToDot :: Phylo -> PhyloExport -> DotGraph DotId
exportToDot phylo export = 
    digraph ((Str . fromStrict) $ (phyloName $ getConfig phylo)) $ do 

    -- | set the global graph attributes

    graphAttrs ( [ Label (toDotLabel $ (phyloName $ getConfig phylo))]
              <> [ FontSize 30, LabelLoc VTop, NodeSep 1, RankSep [1], Rank SameRank, Splines SplineEdges, Overlap ScaleOverlaps
                 , Ratio FillRatio
                 , Style [SItem Filled []],Color [toWColor White]])

    -- | set the branches peaks layer

    subgraph (Str "Peaks") $ do 

        graphAttrs [Rank SameRank]

        -- | group branches by clusters


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
                _ -> export' 
        ) export filters

--------------
-- | Sort | --
--------------

sortByHierarchy :: Int -> [PhyloBranch] -> [PhyloBranch]
sortByHierarchy depth branches 
    | length branches == 1 = branches
    | depth >= ((length . snd) $ (head' "sort" branches) ^. branch_id) = branches
    | otherwise = concat 
                $ map (\branches' -> sortByHierarchy (depth + 1) branches')
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
                            MostInclusive -> undefined
                            _ -> panic "[ERR][Viz.Phylo.PhyloExport] unknown tagger" ) export labels 


------------------
-- | Dynamics | --
------------------ 


toDynamics :: Int -> [PhyloGroup] -> PhyloGroup -> Map Int (Date,Date) -> Double
toDynamics n parents group m = 
    let prd = group ^. phylo_groupPeriod
        bid = group ^. phylo_groupBranchId
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
        branches = map (\bId -> PhyloBranch bId "" empty ((init . snd) bId)) $ nub $ map _phylo_groupBranchId groups
        --------------------------------------    
        groups :: [PhyloGroup]
        groups = processDynamics 
               $ getGroupsFromLevel (phyloLevel $ getConfig phylo) phylo