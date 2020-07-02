{-|
Module      : Gargantext.Viz.Phylo.PhyloMaker
Description : Maker engine for rebuilding a Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


module Gargantext.Viz.Phylo.PhyloMaker where

import Data.List (concat, nub, partition, sort, (++), group, intersect, null, sortOn, groupBy)
import Data.Map (Map, fromListWith, keys, unionWith, fromList, empty, toList, elems, (!), restrictKeys, foldlWithKey)
import Data.Set (size)
import Data.Vector (Vector)

import Gargantext.Prelude
import Gargantext.Viz.AdaptativePhylo
import Gargantext.Viz.Phylo.PhyloTools
import Gargantext.Viz.Phylo.TemporalMatching (adaptativeTemporalMatching, constanteTemporalMatching, getNextPeriods, filterDocs, filterDiago, reduceDiagos, toProximity)
import Gargantext.Viz.Phylo.SynchronicClustering (synchronicClustering)
import Gargantext.Text.Context (TermList)
import Gargantext.Text.Metrics.FrequentItemSet (fisWithSizePolyMap, Size(..))

import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Debug.Trace (trace)
import Control.Lens hiding (Level)

import qualified Data.Vector as Vector
import qualified Data.Set as Set


------------------
-- | To Phylo | --
------------------


toPhylo :: [Document] -> TermList -> Config -> Phylo
toPhylo docs lst conf = trace ("# phylo1 groups " <> show(length $ getGroupsFromLevel 1 phylo1))
                      $ traceToPhylo (phyloLevel conf) $
    if (phyloLevel conf) > 1
      then foldl' (\phylo' _ -> synchronicClustering phylo') phylo1 [2..(phyloLevel conf)]
      else phylo1 
    where
        --------------------------------------
        phylo1 :: Phylo
        phylo1 = toPhylo1 docs phyloBase
        --------------------------------------
        phyloBase :: Phylo 
        phyloBase = toPhyloBase docs lst conf
        --------------------------------------



--------------------
-- | To Phylo 1 | --
--------------------

toGroupsProxi :: Level -> Phylo -> Phylo
toGroupsProxi lvl phylo = 
  let proximity = phyloProximity $ getConfig phylo
      groupsProxi = foldlWithKey (\acc pId pds -> 
                      -- 1) process period by period
                      let egos = map (\g -> (getGroupId g, g ^. phylo_groupNgrams))
                               $ elems 
                               $ view ( phylo_periodLevels 
                                      . traverse . filtered (\phyloLvl -> phyloLvl ^. phylo_levelLevel == lvl) 
                                      . phylo_levelGroups ) pds
                          next    = getNextPeriods ToParents (getTimeFrame $ timeUnit $ getConfig phylo) pId (keys $ phylo ^. phylo_periods)
                          targets = map (\g ->  (getGroupId g, g ^. phylo_groupNgrams)) $ getGroupsFromLevelPeriods lvl next phylo
                          docs    = filterDocs  (phylo ^. phylo_timeDocs) ([pId] ++ next)
                          diagos  = filterDiago (phylo ^. phylo_timeCooc) ([pId] ++ next)
                          -- 2) compute the pairs in parallel
                          pairs  = map (\(id,ngrams) -> 
                                        map (\(id',ngrams') -> 
                                            let nbDocs = (sum . elems) $ filterDocs docs    ([idToPrd id, idToPrd id'])
                                                diago  = reduceDiagos  $ filterDiago diagos ([idToPrd id, idToPrd id'])
                                             in ((id,id'),toProximity nbDocs diago proximity ngrams ngrams' ngrams')
                                        ) $ filter (\(_,ngrams') -> (not . null) $ intersect ngrams ngrams') targets 
                                 ) egos
                          pairs' = pairs `using` parList rdeepseq
                       in acc ++ (concat pairs')
                    ) [] $ phylo ^. phylo_periods
   in phylo & phylo_groupsProxi .~ ((traceGroupsProxi . fromList) groupsProxi) 


appendGroups :: (a -> PhyloPeriodId -> Level -> Int -> Vector Ngrams -> [Cooc] -> PhyloGroup) -> Level -> Map (Date,Date) [a] -> Phylo -> Phylo
appendGroups f lvl m phylo =  trace ("\n" <> "-- | Append " <> show (length $ concat $ elems m) <> " groups to Level " <> show (lvl) <> "\n")
    $ over ( phylo_periods
           .  traverse
           . phylo_periodLevels
           .  traverse)
           (\phyloLvl -> if lvl == (phyloLvl ^. phylo_levelLevel)
                         then
                            let pId = phyloLvl ^. phylo_levelPeriod
                                phyloCUnit = m ! pId
                            in  phyloLvl 
                              & phylo_levelGroups .~ (fromList $ foldl (\groups obj ->
                                    groups ++ [ (((pId,lvl),length groups)
                                              , f obj pId lvl (length groups) (getRoots phylo) 
                                                  (elems $ restrictKeys (phylo ^. phylo_timeCooc) $ periodsToYears [pId]))
                                              ] ) [] phyloCUnit)
                         else 
                            phyloLvl )
           phylo  


cliqueToGroup :: PhyloClique -> PhyloPeriodId -> Level ->  Int -> Vector Ngrams -> [Cooc] -> PhyloGroup
cliqueToGroup fis pId lvl idx fdt coocs =
    let ngrams = ngramsToIdx (Set.toList $ fis ^. phyloClique_nodes) fdt
    in  PhyloGroup pId lvl idx ""
                   (fis ^. phyloClique_support)
                   ngrams
                   (ngramsToCooc ngrams coocs)
                   (1,[0]) --  branchid (lvl,[path in the branching tree])
                   (fromList [("breaks",[0]),("seaLevels",[0])])
                   [] [] [] []


toPhylo1 :: [Document] -> Phylo -> Phylo
toPhylo1 docs phyloBase = case (getSeaElevation phyloBase) of 
    Constante start gap -> constanteTemporalMatching  start gap 
                   $ appendGroups cliqueToGroup 1 phyloClique phyloBase    
    Adaptative steps    -> adaptativeTemporalMatching steps
                   $ toGroupsProxi 1
                   $ appendGroups cliqueToGroup 1 phyloClique phyloBase
    where
        --------------------------------------
        phyloClique :: Map (Date,Date) [PhyloClique]
        phyloClique =  toPhyloClique phyloBase docs'
        --------------------------------------
        docs' :: Map (Date,Date) [Document]
        docs' =  groupDocsByPeriod' date (getPeriodIds phyloBase) docs
        --------------------------------------


---------------------------
-- | Frequent Item Set | --
---------------------------


--  To apply a filter with the possibility of keeping some periods non empty (keep : True|False)
filterClique :: Bool -> Int -> (Int -> [PhyloClique] -> [PhyloClique]) -> Map (Date, Date) [PhyloClique] -> Map (Date, Date) [PhyloClique]
filterClique keep thr f m = case keep of
  False -> map (\l -> f thr l) m
  True  -> map (\l -> keepFilled (f) thr l) m


--  To filter Fis with small Support
filterCliqueBySupport :: Int -> [PhyloClique] -> [PhyloClique]
filterCliqueBySupport thr l = filter (\clq -> (clq ^. phyloClique_support) >= thr) l


--  To filter Fis with small Clique size
filterCliqueBySize :: Int -> [PhyloClique] -> [PhyloClique]
filterCliqueBySize thr l = filter (\clq -> (size $ clq ^. phyloClique_nodes) >= thr) l


--  To filter nested Fis
filterCliqueByNested :: Map (Date, Date) [PhyloClique] -> Map (Date, Date) [PhyloClique]
filterCliqueByNested m = 
  let clq  = map (\l -> 
                foldl (\mem f -> if (any (\f' -> isNested (Set.toList $ f' ^. phyloClique_nodes) (Set.toList $ f ^. phyloClique_nodes)) mem)
                                 then mem
                                 else 
                                    let fMax = filter (\f' -> not $ isNested (Set.toList $ f ^. phyloClique_nodes) (Set.toList $ f' ^. phyloClique_nodes)) mem
                                    in  fMax ++ [f] ) [] l)
           $ elems m 
      clq' = clq `using` parList rdeepseq
  in  fromList $ zip (keys m) clq' 


--  To transform a time map of docs innto a time map of Fis with some filters
toPhyloClique :: Phylo -> Map (Date, Date) [Document] -> Map (Date,Date) [PhyloClique]
toPhyloClique phylo phyloDocs = case (clique $ getConfig phylo) of 
    Fis s s' -> -- traceFis "Filtered Fis"
                filterCliqueByNested 
                {- \$ traceFis "Filtered by clique size" -}
                $ filterClique True s' (filterCliqueBySize)
                {- \$ traceFis "Filtered by support" -}
                $ filterClique True s (filterCliqueBySupport)
                {- \$ traceFis "Unfiltered Fis" -}
                phyloClique
    MaxClique _ -> undefined
    where
        -------------------------------------- 
        phyloClique :: Map (Date,Date) [PhyloClique]
        phyloClique = case (clique $ getConfig phylo) of 
          Fis _ _ ->  let fis  = map (\(prd,docs) -> 
                                  let lst = toList $ fisWithSizePolyMap (Segment 1 20) 1 (map text docs)
                                   in (prd, map (\f -> PhyloClique (fst f) (snd f) prd) lst))
                               $ toList phyloDocs
                          fis' = fis `using` parList rdeepseq
                       in fromList fis'
          MaxClique _ -> undefined
        -------------------------------------- 


--------------------
-- | Coocurency | --
--------------------


--  To transform the docs into a time map of coocurency matrix 
docsToTimeScaleCooc :: [Document] -> Vector Ngrams -> Map Date Cooc
docsToTimeScaleCooc docs fdt = 
    let mCooc  = fromListWith sumCooc
               $ map (\(_d,l) -> (_d, listToMatrix l))
               $ map (\doc -> (date doc, sort $ ngramsToIdx (text doc) fdt)) docs
        mCooc' = fromList
               $ map (\t -> (t,empty))
               $ toTimeScale (map date docs) 1
    in  trace ("\n" <> "-- | Build the coocurency matrix for " <> show (length $ keys mCooc') <> " unit of time" <> "\n")
       $ unionWith sumCooc mCooc mCooc'


-----------------------
-- | to Phylo Base | --
-----------------------

--  To group a list of Documents by fixed periods
groupDocsByPeriod' :: (NFData doc, Ord date, Enum date) => (doc -> date) -> [(date,date)] -> [doc] -> Map (date, date) [doc]
groupDocsByPeriod' f pds docs = 
  let docs'    = groupBy (\d d' -> f d == f d') $ sortOn f docs
      periods  = map (inPeriode f docs') pds
      periods' = periods `using` parList rdeepseq
   in trace ("\n" <> "-- | Group " <> show(length docs) <> " docs by " <> show(length pds) <> " periods" <> "\n") 
    $ fromList $ zip pds periods'
  where
    --------------------------------------
    inPeriode :: Ord b => (t -> b) -> [[t]] -> (b, b) -> [t]
    inPeriode f' h (start,end) =
      concat $ fst $ partition (\d -> f' (head' "inPeriode" d) >= start && f' (head' "inPeriode" d) <= end) h



--  To group a list of Documents by fixed periods
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


docsToTermFreq :: [Document] -> Vector Ngrams -> Map Int Double
docsToTermFreq docs fdt =
  let nbDocs = fromIntegral $ length docs
      freqs = map (/(nbDocs))
             $ fromList
             $ map (\lst -> (head' "docsToTermFreq" lst, fromIntegral $ length lst)) 
             $ group $ sort $ concat $ map (\d -> nub $ ngramsToIdx (text d) fdt) docs
      sumFreqs = sum $ elems freqs
   in map (/sumFreqs) freqs


--  To count the number of docs by unit of time
docsToTimeScaleNb :: [Document] -> Map Date Double
docsToTimeScaleNb docs = 
    let docs' = fromListWith (+) $ map (\d -> (date d,1)) docs
        time  = fromList $ map (\t -> (t,0)) $ toTimeScale (keys docs') 1
    in  trace ("\n" <> "-- | Group " <> show(length docs) <> " docs by " <> show(length time) <> " unit of time" <> "\n") 
      $ unionWith (+) time docs'


initPhyloLevels :: Int -> PhyloPeriodId -> Map PhyloLevelId PhyloLevel
initPhyloLevels lvlMax pId = 
    fromList $ map (\lvl -> ((pId,lvl),PhyloLevel pId lvl empty)) [1..lvlMax]


--  To init the basic elements of a Phylo
toPhyloBase :: [Document] -> TermList -> Config -> Phylo
toPhyloBase docs lst conf = 
    let foundations  = PhyloFoundations (Vector.fromList $ nub $ concat $ map text docs) lst
        params = defaultPhyloParam { _phyloParam_config = conf }
        periods = toPeriods (sort $ nub $ map date docs) (getTimePeriod $ timeUnit conf) (getTimeStep $ timeUnit conf)
    in trace ("\n" <> "-- | Create PhyloBase out of " <> show(length docs) <> " docs \n") 
       $ Phylo foundations
               (docsToTimeScaleCooc docs (foundations ^. foundations_roots))
               (docsToTimeScaleNb docs)
               (docsToTermFreq docs (foundations ^. foundations_roots))
               empty
               params
               (fromList $ map (\prd -> (prd, PhyloPeriod prd (initPhyloLevels 1 prd))) periods)
