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
import Data.List ((++), sort, nub, concat, sortOn, reverse, groupBy, union, (\\), (!!))
import Data.Text (Text)
import Data.Vector (Vector)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools 

import Control.Lens
import Data.GraphViz.Types.Generalised (DotGraph)

--------------------
-- | Dot export | --
--------------------


toDot :: PhyloExport -> DotGraph DotId
toDot export = undefined

-----------------
-- | Metrics | --
-----------------

-- | Return the conditional probability of i knowing j 
conditional :: Ord a => Map (a,a) Double -> a -> a -> Double
conditional m i j = (findWithDefault 0 (i,j) m) 
                  / (m ! (j,j))


-- | Return the inclusion score of a given ngram
inclusion :: Map (Int, Int) Double -> [Int] -> Int -> Double 
inclusion m l i = ( (sum $ map (\j -> conditional m j i) l)
                  + (sum $ map (\j -> conditional m i j) l)) / (fromIntegral $ (length l) + 1)


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


processLabels :: [Label] -> Vector Ngrams -> PhyloExport -> PhyloExport
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
toPhyloExport phylo = toDot 
                    $ processLabels (exportLabel $ getConfig phylo) (getRoots phylo) export           
    where
        export :: PhyloExport
        export = PhyloExport groups branches
        --------------------------------------
        branches :: [PhyloBranch] 
        branches = map (\bId -> PhyloBranch bId "") $ nub $ map _phylo_groupBranchId groups
        --------------------------------------    
        groups :: [PhyloGroup]
        groups = processDynamics 
               $ getGroupsFromLevel (phyloLevel $ getConfig phylo) phylo