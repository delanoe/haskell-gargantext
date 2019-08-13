{-|
Module      : Gargantext.Viz.Phylo.PhyloMaker
Description : Maker engine for rebuilding a Phylo
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

module Gargantext.Viz.Phylo.PhyloMaker where

import Data.List (concat, nub, partition, sort, (++))
import Data.Map (Map, fromListWith, keys, unionWith, fromList, empty, mapWithKey, toList, elems)
import Data.Set (size)
import Data.Vector (Vector)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools
import Gargantext.Text.Context (TermList)
import Gargantext.Text.Metrics.FrequentItemSet (fisWithSizePolyMap, Size(..))

import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Debug.Trace (trace)
import Control.Lens

import qualified Data.Vector as Vector
import qualified Data.Set as Set


------------------
-- | To Phylo | --
------------------


toPhylo :: [Document] -> TermList -> Config -> Phylo
toPhylo docs lst conf = phyloBase
    where
        --------------------------------------
        _phylo1 :: Phylo
        _phylo1 = toPhylo1 docs phyloBase
        --------------------------------------
        phyloBase :: Phylo 
        phyloBase = toPhyloBase docs lst conf
        --------------------------------------



--------------------
-- | To Phylo 1 | --
--------------------


toPhylo1 :: [Document] -> Phylo -> Phylo
toPhylo1 docs phyloBase = undefined
    where
        --------------------------------------
        _mFis :: Map (Date,Date) [PhyloFis]
        _mFis =  toPhyloFis _docs' (fisSupport $ getConfig phyloBase) (fisSize $ getConfig phyloBase)
        --------------------------------------
        _docs' :: Map (Date,Date) [Document]
        _docs' =  groupDocsByPeriod date (getPeriodIds phyloBase) docs
        --------------------------------------


---------------------------
-- | Frequent Item Set | --
---------------------------


-- | To apply a filter with the possibility of keeping some periods non empty (keep : True|False)
filterFis :: Bool -> Int -> (Int -> [PhyloFis] -> [PhyloFis]) -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFis keep thr f m = case keep of
  False -> map (\l -> f thr l) m
  True  -> map (\l -> keepFilled (f) thr l) m


-- | To filter Fis with small Support
filterFisBySupport :: Int -> [PhyloFis] -> [PhyloFis]
filterFisBySupport thr l = filter (\fis -> (fis ^. phyloFis_support) >= thr) l


-- | To filter Fis with small Clique size
filterFisByClique :: Int -> [PhyloFis] -> [PhyloFis]
filterFisByClique thr l = filter (\fis -> (size $ fis ^. phyloFis_clique) >= thr) l


-- | To filter nested Fis
filterFisByNested :: Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisByNested m = 
  let fis  = map (\l -> 
                foldl (\mem f -> if (any (\f' -> isNested (Set.toList $ f' ^. phyloFis_clique) (Set.toList $ f ^. phyloFis_clique)) mem)
                                 then mem
                                 else 
                                    let fMax = filter (\f' -> not $ isNested (Set.toList $ f ^. phyloFis_clique) (Set.toList $ f' ^. phyloFis_clique)) mem
                                    in  fMax ++ [f] ) [] l)
           $ elems m 
      fis' = fis `using` parList rdeepseq
  in  fromList $ zip (keys m) fis' 


-- | To transform a time map of docs innto a time map of Fis with some filters
toPhyloFis :: Map (Date, Date) [Document] -> Int -> Int -> Map (Date,Date) [PhyloFis]
toPhyloFis mDocs support clique = traceFis "Filtered Fis"
                                $ filterFisByNested 
                                $ traceFis "Filtered by clique size"
                                $ filterFis True clique (filterFisByClique)
                                $ traceFis "Filtered by support"
                                $ filterFis True support (filterFisBySupport)
                                $ traceFis "Unfiltered Fis" mFis
    where
        --------------------------------------
        -- | create the fis from the docs for each period
        mFis :: Map (Date,Date) [PhyloFis]
        mFis = mapWithKey (\prd docs -> let fis = toList $ fisWithSizePolyMap (Segment 1 20) 1 (map text docs)
                                        in  map (\f -> PhyloFis (fst f) (snd f) prd) fis ) mDocs
        -------------------------------------- 


--------------------
-- | Coocurency | --
--------------------


-- | To transform the docs into a time map of coocurency matrix 
docsToCoocByYear :: [Document] -> Vector Ngrams -> Config -> Map Date Cooc
docsToCoocByYear docs fdt conf = 
    let mCooc  = fromListWith sumCooc
               $ map (\(_d,l) -> (_d, listToMatrix l))
               $ map (\doc -> (date doc, sort $ ngramsToIdx (text doc) fdt)) docs
        mCooc' = fromList
               $ map (\t -> (t,empty))
               $ toTimeScale (map date docs) (timeUnit conf)
    in   trace ("\n" <> "-- | Build the coocurency matrix for " <> show (length $ keys mCooc') <> " unit of time" <> "\n")
       $ unionWith sumCooc mCooc mCooc'


-----------------------
-- | to Phylo Base | --
-----------------------


-- | To group a list of Documents by fixed periods
groupDocsByPeriod :: (NFData doc, Ord date, Enum date) => (doc -> date) -> [(date,date)] -> [doc] -> Map (date, date) [doc]
groupDocsByPeriod _ _   [] = panic "[ERR][Viz.Phylo.PhyloMaker] Empty [Documents] can not have any periods"
groupDocsByPeriod f pds es = 
  let periods  = map (inPeriode f es) pds
      periods' = periods `using` parList rdeepseq

  in  trace ("\n" <> "-- | Group " <> show(length es) <> " docs by " <> show(length pds) <> " periods" <> "\n") 
    $ fromList $ zip pds periods'
  where
    --------------------------------------
    inPeriode :: Ord b => (t -> b) -> [t] -> (b, b) -> [t]
    inPeriode f' h (start,end) =
      fst $ partition (\d -> f' d >= start && f' d <= end) h
    --------------------------------------   


-- | To count the number of docs by unit of time (like a year)
nbDocsByTime :: [Document] -> Int -> Map Date Double
nbDocsByTime docs step = 
    let docs' = fromListWith (+) $ map (\d -> (date d,1)) docs
        time  = fromList $ map (\t -> (t,0)) $ toTimeScale (keys docs') step
    in  trace ("\n" <> "-- | Group " <> show(length docs) <> " docs by " <> show(length time) <> " unit of time" <> "\n") 
      $ unionWith (+) time docs'


-- | To init the basic elements of a Phylo
toPhyloBase :: [Document] -> TermList -> Config -> Phylo
toPhyloBase docs lst conf = 
    let foundations  = PhyloFoundations (Vector.fromList $ nub $ concat $ map text docs) lst
        params = defaultPhyloParam { _phyloParam_config = conf }
        periods = toPeriods (sort $ nub $ map date docs) (timePeriod conf) (timeStep conf)
    in trace ("\n" <> "-- | Create PhyloBase out of " <> show(length docs) <> " docs \n") 
       $ Phylo foundations
               (docsToCoocByYear docs (foundations ^. foundations_roots) conf)
               (nbDocsByTime docs $ timeUnit conf)
               params
               (map (\prd -> PhyloPeriod prd []) periods)
