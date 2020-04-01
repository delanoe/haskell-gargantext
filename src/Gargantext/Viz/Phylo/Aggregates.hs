{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
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

module Gargantext.Viz.Phylo.Aggregates
  where

import Control.Parallel.Strategies

import Gargantext.Prelude hiding  (elem)
import Gargantext.Text.Context    (TermList)
import Gargantext.Text.Metrics.FrequentItemSet (fisWithSizePolyMap, Size(..))
import Gargantext.Text.Terms.Mono (monoTexts)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

import Debug.Trace (trace)

import Data.List    (partition, concat, nub, elem, sort, (++), null, union)
import Data.Map     (Map, fromList, fromListWith, adjust, filterWithKey, elems, keys, unionWith, mapWithKey)
import Data.Set     (size)
import Data.Text    (Text, unwords)
import Data.Vector  (Vector)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Vector as Vector



---------------------
-- | Foundations | --
---------------------

-- | Extract all the labels of a termList
termListToNgrams :: TermList -> [Ngrams]
termListToNgrams = map (\(lbl,_) -> unwords lbl)


-------------------
-- | Documents | --
-------------------

-- | To group a list of Documents by fixed periods
groupDocsByPeriod :: (NFData doc, Ord date, Enum date) => (doc -> date) -> [(date,date)] -> [doc] -> Map (date, date) [doc]
groupDocsByPeriod _ _   [] = panic "[ERR][Viz.Phylo.Example.docsToPeriods] Empty [Documents] can not have any periods"
groupDocsByPeriod f pds es = 
  let periods  = map (inPeriode f es) pds
      periods' = periods `using` parList rdeepseq

  in  trace ("----\nGroup docs by periods\n") $ fromList $ zip pds periods'
  where
    --------------------------------------
    inPeriode :: Ord b => (t -> b) -> [t] -> (b, b) -> [t]
    inPeriode f' h (start,end) =
      fst $ partition (\d -> f' d >= start && f' d <= end) h
    --------------------------------------    


-- | To parse a list of Documents by filtering on a Vector of Ngrams
parseDocs :: Vector Ngrams -> [(Date,Text)] -> [Document]
parseDocs roots c = map (\(d,t)
                -> Document d ( filter (\x -> Vector.elem x roots)
                              $ monoTexts t)) c

-- | To count the number of documents by year
countDocs :: [(Date,a)] -> Map Date Double
countDocs corpus = fromListWith (+) $ map (\(d,_) -> (d,1)) corpus


-----------------
-- | Periods | --
-----------------


-- | To init a list of Periods framed by a starting Date and an ending Date
initPeriods :: (Eq date, Enum date) => Grain -> Step -> (date, date) -> [(date, date)]
initPeriods g s (start,end) = map (\l -> (head' "initPeriods" l, last' "initPeriods" l))
                            $ chunkAlong g s [start .. end]


--------------
-- | Cooc | --
--------------


-- | To transform a tuple of group's information into a coocurency Matrix
toCooc :: [([Int],Double)] -> Map (Int, Int) Double
toCooc l = map (/docs)
         $ foldl (\mem x -> adjust (+1) x mem) cooc
         $ concat
         $ map (\x -> listToFullCombi $ fst x) l
  where
    --------------------------------------
    idx :: [Int]
    idx = nub $ concat $ map fst l
    --------------------------------------
    docs :: Double
    docs = sum $ map snd l
    --------------------------------------
    cooc :: Map (Int, Int) (Double)
    cooc = fromList $ map (\x -> (x,0)) $ listToFullCombi idx
    --------------------------------------    


-- | To reduce a coocurency Matrix to some keys
getSubCooc :: [Int] -> Map (Int, Int) Double -> Map (Int, Int) Double
getSubCooc idx cooc = filterWithKey (\k _ -> (elem (fst k) idx)
                                          && (elem (snd k) idx)) cooc


-- | To get a coocurency Matrix related to a given list of Periods
getCooc :: [PhyloPeriodId] -> Phylo -> Map (Int, Int) Double
getCooc prds p = toCooc $ map (\g -> (getGroupNgrams g,getGroupMeta "support" g)) gs
  where
    --------------------------------------
    -- | Here we need to go back to the level 1 (aka : the Fis level)
    gs :: [PhyloGroup]
    gs = filter (\g -> elem (getGroupPeriod g) prds ) $ getGroupsWithLevel 1 p
    -------------------------------------- 


-- | To transform a list of index into a cooc matrix 
listToCooc :: [Int] -> Map (Int,Int) Double
listToCooc lst = fromList $ map (\combi -> (combi,1)) $ listToFullCombi lst


-- | To build the cooc matrix by years out of the corpus
docsToCooc :: [Document] -> Vector Ngrams -> Map Date (Map (Int,Int) Double)
docsToCooc docs fdt = fromListWith sumCooc 
                    $ map (\(d,l) -> (d, listToCooc l))
                    $ map (\doc -> (date doc, ngramsToIdx (text doc) fdt)) docs


-------------
-- | Fis | --
-------------


-- | To apply a filter with the possibility of keeping some periods non empty (keep : True|False)
filterFis :: Bool -> Int -> (Int -> [PhyloFis] -> [PhyloFis]) -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFis keep thr f m = case keep of
  False -> map (\l -> f thr l) m
  True  -> map (\l -> keepFilled (f) thr l) m


-- | To filter Fis with small Support
filterFisBySupport :: Int -> [PhyloFis] -> [PhyloFis]
filterFisBySupport thr l = filter (\fis -> getSupport fis >= thr) l


-- | To filter Fis with small Clique size
filterFisByClique :: Int -> [PhyloFis] -> [PhyloFis]
filterFisByClique thr l = filter (\fis -> (size $ getClique fis) >= thr) l


-- | To find if l' is nested in l
isNested :: Eq a => [a] -> [a] -> Bool
isNested l l'
  | null l'               = True
  | length l' > length l  = False
  | (union  l l') == l    = True
  | otherwise             = False 


-- | To filter nested Fis
filterFisByNested :: Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis]
filterFisByNested m = 
  let fis  = map (\l -> 
                foldl (\mem f -> if (any (\f' -> isNested (Set.toList $ getClique f') (Set.toList $ getClique f)) mem)
                                 then mem
                                 else 
                                    let fMax = filter (\f' -> not $ isNested (Set.toList $ getClique f) (Set.toList $ getClique f')) mem
                                    in  fMax ++ [f] ) [] l)
           $ elems m 
      fis' = fis `using` parList rdeepseq
  in  fromList $ zip (keys m) fis' 


-- | Choose if we use a set of Fis from a file or if we have to create them
docsToFis :: Map (Date,Date) [Document] -> Phylo -> Map (Date, Date) [PhyloFis]
docsToFis m p = if (null $ getPhyloFis p)
                 then trace("----\nRebuild the Fis from scratch\n") 
                    $ mapWithKey (\k docs -> let fis = Map.toList $ fisWithSizePolyMap (Segment 1 20) 1 (map text docs)
                                                              in map (\f -> PhyloFis (fst f) (snd f) k) fis) m
                 else trace("----\nUse Fis from an existing file\n") 
                    $ unionWith (++) (fromList $ map (\k -> (k,[])) $ keys m) (getPhyloFis p)


-- | Process some filters on top of a set of Fis
refineFis :: Map (Date, Date) [PhyloFis] -> Bool -> Support -> Int -> Map (Date, Date) [PhyloFis]
refineFis fis k s t = traceFis "----\nFiltered Fis by nested :\n"
                      $ filterFisByNested 
                      $ traceFis "----\nFiltered Fis by clique size :\n"                      
                      $ filterFis k t (filterFisByClique)
                      $ traceFis "----\nFiltered Fis by support :\n"
                      $ filterFis k s (filterFisBySupport)
                      $ traceFis "----\nUnfiltered Fis :\n" fis


-----------------
-- | Tracers | --
-----------------


traceFis :: [Char] -> Map (Date, Date) [PhyloFis] -> Map (Date, Date) [PhyloFis] 
traceFis lbl m = trace (lbl <> "count : " <> show (sum $ map length $ elems m) <> " Fis\n"
                            <> "support : " <> show (countSup 1 supps) <> " (>1) "
                                            <> show (countSup 2 supps) <> " (>2) "
                                            <> show (countSup 3 supps) <> " (>3) "
                                            <> show (countSup 4 supps) <> " (>4) "
                                            <> show (countSup 5 supps) <> " (>5) "
                                            <> show (countSup 6 supps) <> " (>6)\n"                                                                                                                                          
                            <> "clique size : " <> show (countSup 1 ngrms) <> " (>1) "
                                                <> show (countSup 2 ngrms) <> " (>2) "
                                                <> show (countSup 3 ngrms) <> " (>3) "
                                                <> show (countSup 4 ngrms) <> " (>4) "
                                                <> show (countSup 5 ngrms) <> " (>5) "
                                                <> show (countSup 6 ngrms) <> " (>6)\n"                                                                                             
                            ) m
  where
    --------------------------------------
    countSup :: Double -> [Double] -> Int
    countSup s l = length $ filter (>s) l 
    --------------------------------------
    supps :: [Double]
    supps = sort $ map (fromIntegral . _phyloFis_support) $ concat $ elems m
    --------------------------------------
    ngrms :: [Double]
    ngrms = sort $ map (\f -> fromIntegral $ size $ _phyloFis_clique f) $ concat $ elems m
    --------------------------------------
