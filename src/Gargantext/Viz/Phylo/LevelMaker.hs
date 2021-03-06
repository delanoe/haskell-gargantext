{-|
Module      : Gargantext.Viz.Phylo.Tools
Description : Phylomemy Tools to build/manage it
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# LANGUAGE TypeSynonymInstances #-}

module Gargantext.Viz.Phylo.LevelMaker
  where

import Control.Parallel.Strategies
import Control.Lens                 hiding (both, Level)
import Data.List                    ((++), sort, concat, nub, zip, last, null)
import Data.Map                     (Map, (!), empty, singleton, size)
import Data.Text (Text)
import Data.Tuple.Extra
import Data.Vector (Vector)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Metrics
import Gargantext.Viz.Phylo.Aggregates
import Gargantext.Viz.Phylo.Cluster
import Gargantext.Viz.Phylo.BranchMaker
import Gargantext.Viz.Phylo.LinkMaker
import Gargantext.Viz.Phylo.Tools
import Gargantext.Text.Context (TermList)

import qualified Data.Vector.Storable as VS
import qualified Data.Set    as Set
import qualified Data.Vector as Vector

import Debug.Trace (trace)
import Numeric.Statistics (percentile)


-------------------------
-- | PhyloLevelMaker | --
-------------------------


-- | A typeClass for polymorphic PhyloLevel functions
class PhyloLevelMaker aggregate
    where
        -- | To add a new Level of PhyloGroups to a Phylo based on a list of Aggregates
        addPhyloLevel :: Level -> Map (Date,Date) [aggregate] -> Phylo -> Phylo
        -- | To create a list of PhyloGroups based on a list of aggregates a
        toPhyloGroups :: Level -> (Date,Date)  -> [aggregate] -> Map (Date,Date) [aggregate] -> Phylo -> [PhyloGroup]


instance PhyloLevelMaker PhyloCluster
  where
    --------------------------------------
    -- | Level -> Map (Date,Date) [Cluster] -> Phylo -> Phylo
    addPhyloLevel lvl m p
      | lvl > 1   = addPhyloLevel' lvl m p
      | otherwise = panic ("[ERR][Viz.Phylo.Example.addPhyloLevel] No process declared for adding Clusters at level < 2")
    --------------------------------------
    -- | Level -> (Date,Date) -> [Cluster] -> Map (Date,Date) [Cluster] -> Phylo -> [PhyloGroup]
    toPhyloGroups lvl (d,d') l m p = 
      let clusters  = map (\(idx,cluster) -> clusterToGroup (d,d') lvl idx "" cluster m p) $ zip [1..] l
          clusters' = clusters `using` parList rdeepseq
      in  clusters'
    --------------------------------------


instance PhyloLevelMaker PhyloFis
  where
    --------------------------------------
    -- | Level -> Map (Date,Date) [Fis] -> Phylo -> Phylo
    addPhyloLevel lvl m p
      | lvl == 1  = addPhyloLevel' lvl m p
      | otherwise = panic ("[ERR][Viz.Phylo.Example.addPhyloLevel] No process declared for adding Fis at level <> 1")
    --------------------------------------
    -- | Level -> (Date,Date) -> [Fis] -> Map (Date,Date) [Fis] -> Phylo -> [PhyloGroup]
    toPhyloGroups lvl (d,d') l _ p =
      let groups  = map (\(idx,fis) -> cliqueToGroup (d,d') lvl idx "" fis (getPhyloCooc p) (getFoundationsRoots p)) $ zip [1..] l
          groups' = groups `using` parList rdeepseq
      in  groups' 
    --------------------------------------


instance PhyloLevelMaker Document
  where
    --------------------------------------
    -- | Level -> Map (Date,Date) [Document] -> Phylo -> Phylo
    addPhyloLevel lvl m p
      | lvl == 0  = addPhyloLevel' lvl m p
      | otherwise = panic ("[ERR][Viz.Phylo.Example.addPhyloLevel] No process declared for adding Documents at level <> 0")
    --------------------------------------
    -- | Level -> (Date,Date) -> [Document] -> Map (Date,Date) [Fis] -> Phylo -> [PhyloGroup]
    toPhyloGroups lvl (d,d') l _m p = map (\ngram -> ngramsToGroup (d,d') lvl (getIdxInRoots ngram p) ngram [ngram] p)
                                          $ (nub . concat)
                                          $ map text l
    --------------------------------------


-- | To traverse a Phylo and add a new PhyloLevel linked to a new list of PhyloGroups
addPhyloLevel' :: PhyloLevelMaker a => Level -> Map (Date, Date) [a] -> Phylo -> Phylo
addPhyloLevel' lvl m p = alterPhyloPeriods
                        (\period -> let pId = _phylo_periodId period
                                    in  over (phylo_periodLevels)
                                        (\phyloLevels ->
                                            let groups = toPhyloGroups lvl pId (m ! pId) m p
                                            in trace (show (length groups) <> " groups for " <> show (pId) ) $ phyloLevels ++ [PhyloLevel (pId, lvl) groups]
                                        ) period) p


----------------------
-- | toPhyloGroup | --
----------------------


-- | To transform a Clique into a PhyloGroup
cliqueToGroup :: PhyloPeriodId -> Level -> Int -> Text -> PhyloFis -> Map Date (Map (Int,Int) Double) -> Vector Ngrams -> PhyloGroup
cliqueToGroup prd lvl idx lbl fis cooc' root = PhyloGroup ((prd, lvl), idx) lbl ngrams 
    (getNgramsMeta cooc ngrams)
    -- empty
    (singleton "support" (fromIntegral $ getSupport fis)) 
    Nothing
    cooc
    [] [] [] childs
      where
        --------------------------------------
        cooc :: Map (Int, Int) Double
        cooc = getMiniCooc (listToFullCombi ngrams) (periodsToYears [prd]) cooc'
        --------------------------------------
        ngrams :: [Int]
        ngrams = sort $ map (\x -> getIdxInRoots' x root)
                      $ Set.toList
                      $ getClique fis
        --------------------------------------
        childs :: [Pointer]
        childs = map (\n -> (((prd, lvl - 1), n),1)) ngrams
        --------------------------------------


-- | To transform a Cluster into a Phylogroup
clusterToGroup :: PhyloPeriodId -> Level -> Int -> Text -> PhyloCluster -> Map (Date,Date) [PhyloCluster]-> Phylo-> PhyloGroup
clusterToGroup prd lvl idx lbl groups _m p =
    PhyloGroup ((prd, lvl), idx) lbl ngrams 
      (getNgramsMeta cooc ngrams) 
      -- empty
      empty
      Nothing
      cooc
      ascLink desLink [] childs
      where
        --------------------------------------
        cooc :: Map (Int, Int) Double
        cooc = getMiniCooc (listToFullCombi ngrams) (periodsToYears [prd]) (getPhyloCooc p)
        --------------------------------------
        childs :: [Pointer]
        childs = map (\g -> (getGroupId g, 1)) groups
        ascLink = concat $ map getGroupPeriodParents groups 
        desLink = concat $ map getGroupPeriodChilds  groups        
        --------------------------------------
        ngrams :: [Int]
        ngrams = (sort . nub . concat) $ map getGroupNgrams groups
        --------------------------------------


-- | To transform a list of Ngrams into a PhyloGroup
ngramsToGroup ::  PhyloPeriodId -> Level -> Int -> Text -> [Ngrams] -> Phylo -> PhyloGroup
ngramsToGroup prd lvl idx lbl ngrams p = PhyloGroup ((prd, lvl), idx) lbl (sort $ map (\x -> getIdxInRoots x p) ngrams) empty empty Nothing
               (getMiniCooc (listToFullCombi $ sort $ map (\x -> getIdxInRoots x p) ngrams) (periodsToYears [prd]) (getPhyloCooc p))
               [] [] [] []


----------------------
-- | toPhyloLevel | --
----------------------


-- | To reconstruct the Phylo from a set of Document to a given Level
toPhylo ::  PhyloQueryBuild -> [Document] -> TermList -> Map (Date,Date) [PhyloFis] -> Phylo
toPhylo q c termList fis = toNthLevel (getNthLevel q) (getInterTemporalMatching q) (getNthCluster q) phylo1
  where
    --------------------------------------
    phylo1 :: Phylo
    phylo1 = toPhylo1 (getContextualUnit q) (getInterTemporalMatching q) phyloDocs phyloBase
    -- phylo1 = toPhylo1 (getContextualUnit q) (getInterTemporalMatching q) phyloDocs phylo
    --------------------------------------
    -- phylo0 :: Phylo
    -- phylo0 = tracePhyloN 0 
    --        $ addPhyloLevel 0 phyloDocs phyloBase
    --------------------------------------
    phyloDocs :: Map (Date, Date) [Document]
    phyloDocs = groupDocsByPeriod date (getPhyloPeriods phyloBase) c
    --------------------------------------
    phyloBase :: Phylo
    phyloBase = tracePhyloBase 
              $ toPhyloBase q (initPhyloParam (Just defaultPhyloVersion) (Just defaultSoftware) (Just q)) c termList fis
    --------------------------------------       


-- | To incrementally add new Levels to a Phylo by making all the linking and aggregation tasks
toNthLevel :: Level -> Proximity -> Cluster -> Phylo -> Phylo
toNthLevel lvlMax prox clus p
  | lvl >= lvlMax = p
  | otherwise     = toNthLevel lvlMax prox clus
                  $ traceBranches (lvl + 1)
                  $ setPhyloBranches (lvl + 1)
                  -- \$ transposePeriodLinks (lvl + 1)
                  $ traceTranspose (lvl + 1) Descendant
                  $ transposeLinks (lvl + 1) Descendant
                  $ traceTranspose (lvl + 1) Ascendant
                  $ transposeLinks (lvl + 1) Ascendant
                  $ tracePhyloN (lvl + 1)
                  $ setLevelLinks (lvl, lvl + 1)
                  $ addPhyloLevel (lvl + 1)
                    (clusters) p
  where
    --------------------------------------
    clusters :: Map (Date,Date) [PhyloCluster]
    clusters = phyloToClusters lvl clus p
    --------------------------------------
    lvl :: Level
    lvl = getLastLevel p
    --------------------------------------


-- | To reconstruct the Level 1 of a Phylo based on a Clustering Method
toPhylo1 :: Cluster -> Proximity -> Map (Date, Date) [Document] -> Phylo -> Phylo
toPhylo1 clus prox d p = case clus of
  Fis (FisParams k s t) -> traceBranches 1 
                       -- \$ reLinkPhyloBranches 1 
                       -- \$ traceBranches 1 
                       $ setPhyloBranches 1
                       $ traceTempoMatching Descendant 1
                       $ interTempoMatching Descendant 1 prox
                       $ traceTempoMatching Ascendant 1
                       $ interTempoMatching Ascendant 1 prox
                       $ tracePhyloN 1
                       -- \$ setLevelLinks (0,1)
                       $ addPhyloLevel 1 (getPhyloFis phyloFis)
                       $ trace (show (size $ getPhyloFis phyloFis) <> " Fis created") $ phyloFis
    where
      --------------------------------------
      phyloFis :: Phylo
      phyloFis = if (null $ getPhyloFis p)
                 then p & phylo_fis .~ refineFis (docsToFis d p) k s t
                 else p & phylo_fis .~ docsToFis d p
      --------------------------------------

  _   -> panic "[ERR][Viz.Phylo.LevelMaker.toPhylo1] fst clustering not recognized"


-- | To create the base of the Phylo (foundations, periods, cooc, etc)
toPhyloBase :: PhyloQueryBuild -> PhyloParam -> [Document] -> TermList -> Map (Date,Date) [PhyloFis] -> Phylo
toPhyloBase q p c termList fis = initPhyloBase periods foundations nbDocs cooc fis p
  where
    --------------------------------------
    cooc :: Map Date (Map (Int,Int) Double)
    cooc = docsToCooc c (foundations ^. phylo_foundationsRoots)
    --------------------------------------
    nbDocs :: Map Date Double
    nbDocs = countDocs $ map (\doc -> (date doc, text doc)) c
    --------------------------------------
    foundations :: PhyloFoundations
    foundations = PhyloFoundations (initFoundationsRoots (termListToNgrams termList)) termList
    --------------------------------------
    periods :: [(Date,Date)]
    periods = initPeriods (getPeriodGrain q) (getPeriodSteps q)
            $ both date (head' "toPhyloBase" c, last' "toPhyloBase" c)
    --------------------------------------


-----------------
-- | Tracers | --
-----------------


tracePhyloN :: Level -> Phylo -> Phylo
tracePhyloN lvl p = trace ("\n---------------\n--| Phylo " <> show (lvl) <> " |--\n---------------\n\n"
                      <> show (length $ getGroupsWithLevel lvl p) <> " groups created \n") p

traceTranspose :: Level -> Filiation -> Phylo -> Phylo
traceTranspose lvl fil p = trace ("----\n Transpose " <> show (fil) <> " links for " <> show (length $ getGroupsWithLevel lvl p) <> " groups\n") p


tracePhyloBase :: Phylo -> Phylo
tracePhyloBase p = trace ( "\n-------------\n--| Phylo |--\n-------------\n\n" 
                        <> show (length $ _phylo_periods p) <> " periods from " 
                                 <> show (getPhyloPeriodId $ (head' "PhyloMaker") $ _phylo_periods p)
                                 <> " to " 
                                 <> show (getPhyloPeriodId $ last $ _phylo_periods p)
                                 <> "\n"
                        <> show ( Vector.length $ getFoundationsRoots p) <> " foundations roots \n") p


traceTempoMatching :: Filiation -> Level -> Phylo -> Phylo
traceTempoMatching fil lvl p = trace ( "----\n" <> show (fil) <> " filtered temporal Matching in Phylo" <> show (lvl) <> " :\n"
                                    <> "count : " <> show (length pts) <> " pointers\n") p
  where
    --------------------------------------
    pts :: [Pointer]
    pts = concat $ map (\g -> getGroupPointers PeriodEdge fil g) $ getGroupsWithLevel lvl p
    --------------------------------------


traceBranches :: Level -> Phylo -> Phylo
traceBranches lvl p = trace ( "----\n" <> "Branches in Phylo" <> show lvl <> " :\n"
                           <> "count : " <> show (length $ filter (\(lvl',_) -> lvl' == lvl ) $ getBranchIds p) <> " branches\n"
                           <> "count : " <> show (length $ getGroupsWithLevel lvl p)    <> " groups\n"
                           <> "groups by branch : " <> show (percentile 25 (VS.fromList brs)) <> " (25%) "
                                                    <> show (percentile 50 (VS.fromList brs)) <> " (50%) "
                                                    <> show (percentile 75 (VS.fromList brs)) <> " (75%) "
                                                    <> show (percentile 90 (VS.fromList brs)) <> " (90%)\n") p
  where
    --------------------------------------
    brs :: [Double]
    brs = sort $ map (\(_,gs) -> fromIntegral $ length gs)
        $ filter (\(id,_) -> (fst id) == lvl)
        $ getGroupsByBranches p
    --------------------------------------
