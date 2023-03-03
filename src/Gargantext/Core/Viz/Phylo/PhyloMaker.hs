{-|
Module      : Gargantext.Core.Viz.Phylo.PhyloMaker
Description : Maker engine for rebuilding a Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


module Gargantext.Core.Viz.Phylo.PhyloMaker where


import Control.DeepSeq (NFData)
import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.List (concat, nub, partition, sort, (++), group, intersect, null, sortOn, groupBy, tail)
import Data.Map (Map, fromListWith, keys, unionWith, fromList, empty, toList, elems, (!), restrictKeys, insert)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Debug.Trace (trace)
import Prelude (floor)

import Gargantext.Core.Methods.Distances (Distance(Conditional))
import Gargantext.Core.Methods.Graph.MaxClique (getMaxCliques)
import Gargantext.Core.Text.Metrics.FrequentItemSet (fisWithSizePolyMap, fisWithSizePolyMap', Size(..))
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.PhyloExport (toHorizon)
import Gargantext.Core.Viz.Phylo.PhyloTools
import Gargantext.Core.Viz.Phylo.SynchronicClustering (synchronicClustering)
import Gargantext.Core.Viz.Phylo.TemporalMatching (toPhyloQuality, temporalMatching, getNextPeriods, filterDocs, filterDiago, reduceDiagos, toSimilarity)
import Gargantext.Prelude

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Vector as Vector

------------------
-- | To Phylo | --
------------------

{-
-- TODO AD
data Phylo' = PhyloBase { _phylo'_phyloBase :: Phylo}
            | PhyloN    { _phylo'_flatPhylo :: Phylo}


toPhylo' :: Phylo' -> [Document] -> TermList -> PhyloConfig -> Phylo
toPhylo' (PhyloN    phylo) = toPhylo' 
toPhylo' (PhyloBase phylo) = toPhylo 
-}

-- TODO an adaptative synchronic clustering with a slider

toPhylo :: Phylo -> Phylo
toPhylo phylowithoutLink = traceToPhylo (phyloScale $ getConfig phylowithoutLink) $
    if (phyloScale $ getConfig phylowithoutLink) > 1
      then foldl' (\phylo' _ -> synchronicClustering phylo') phyloAncestors [2..(phyloScale $ getConfig phylowithoutLink)]
      else phyloAncestors 
    where
        --------------------------------------
        phyloAncestors :: Phylo
        phyloAncestors = 
            if (findAncestors $ getConfig phylowithoutLink)
              then toHorizon phyloWithLinks
              else phyloWithLinks
        --------------------------------------
        phyloWithLinks :: Phylo
        phyloWithLinks = temporalMatching (getLadder phylowithoutLink) phylowithoutLink
        --------------------------------------


-----------------------------
-- | Create a flat Phylo | --
-----------------------------


{- 
-- create a square ladder
-}
squareLadder :: [Double] -> [Double]
squareLadder ladder = List.map (\x -> x * x) ladder


{- 
-- create an adaptative 'sea elevation' ladder
-}
adaptSeaLadder :: Double -> Set Double -> Set Double -> [Double]
adaptSeaLadder curr similarities ladder = 
  if curr <= 0 || Set.null similarities
    then Set.toList ladder
    else 
      let idx = ((Set.size similarities) `div` (floor curr)) - 1
          thr = Set.elemAt idx similarities
      -- we use a sliding methods 1/10, then 1/9, then ... 1/2
      in adaptSeaLadder (curr -1) (Set.filter (> thr) similarities) (Set.insert thr ladder) 


{- 
-- create a constante 'sea elevation' ladder
-}
constSeaLadder :: Double -> Double -> Set Double -> [Double]
constSeaLadder curr step ladder = 
  if curr > 1
    then Set.toList ladder
    else constSeaLadder (curr + step) step (Set.insert curr ladder)



{- 
-- create an evolving 'sea elevation' ladder based on estimated & local quality maxima
-}    
evolvSeaLadder :: Double -> Double -> Map Int Double -> Set Double -> [((PhyloGroup,PhyloGroup),Double)] -> [Double]
evolvSeaLadder nbFdt lambda freq similarities graph = map snd 
                                                    $ filter fst 
                                                    $ zip maxima (map fst qua')
                                                    -- 3) find the corresponding measures of similarity and create the ladder
  where
    --------
    -- 2) find the local maxima in the quality distribution
    maxima :: [Bool]
    maxima = [snd (List.head qua') > snd (List.head $ List.tail qua')] ++ (findMaxima qua') ++ [snd (List.head $ reverse qua') > snd (List.head $ List.tail $ reverse qua')]
    --------
    -- 1.2)
    qua' :: [(Double,Double)]
    qua' = foldl (\acc (s,q) -> 
                    if length acc == 0
                      then [(s,q)]
                      else if (snd (List.last acc)) == q
                        then acc
                      else acc ++ [(s,q)]
      ) [] $ zip (Set.toList similarities) qua
    --------
    -- 1.1) for each measure of similarity, prune the flat phylo, compute the branches and estimate the quality
    qua :: [Double]
    qua = map (\thr -> 
                let edges    = filter (\edge -> snd edge >= thr) graph
                    nodes    = nub $ concat $ map (\((n,n'),_) -> [n,n']) edges
                    branches = toRelatedComponents nodes edges
                 in toPhyloQuality nbFdt lambda freq branches
              ) $ (Set.toList similarities)


{- 
-- find a similarity ladder regarding the "sea elevation" strategy
-}
findSeaLadder :: Phylo -> Phylo
findSeaLadder phylo = case getSeaElevation phylo of 
    Constante  start gap -> phylo & phylo_seaLadder .~ (constSeaLadder start gap Set.empty)
    Adaptative steps     -> phylo & phylo_seaLadder .~ (squareLadder $ adaptSeaLadder steps similarities Set.empty)
    Evolving   _         -> let ladder = evolvSeaLadder 
                                           (fromIntegral $ Vector.length $ getRoots phylo)
                                           (getLevel phylo)
                                           (getRootsFreq phylo)
                                           similarities simGraph
                             in phylo & phylo_seaLadder .~ (if length ladder > 0
                                                              then ladder 
                                                              -- if we don't find any local maxima with the evolving strategy
                                                              else constSeaLadder 0.1 0.1 Set.empty)
  where
    --------
    -- 2) extract the values of the kinship links    
    similarities :: Set Double
    similarities = Set.fromList $ sort $ map snd simGraph
    --------
    -- 1) we process an initial calculation of the kinship links
    --    this initial calculation is used to estimate the real sea ladder
    simGraph :: [((PhyloGroup,PhyloGroup),Double)]
    simGraph = foldl' (\acc period -> 
          -- 1.1) process period by period
                  let sources = getGroupsFromScalePeriods 1 [period] phylo
                      next    = getNextPeriods ToParents  3  period (keys $ phylo ^. phylo_periods)                           
                      targets = getGroupsFromScalePeriods 1 next phylo
                      docs    = filterDocs  (getDocsByDate phylo) ([period] ++ next)
                      diagos  = filterDiago (getCoocByDate phylo) ([period] ++ next)
          -- 1.2) compute the kinship similarities between pairs of source & target in parallel
                      pairs   = map (\source -> 
                                    let candidates = filter (\target -> (> 2) $ length 
                                                   $ intersect (getGroupNgrams source) (getGroupNgrams target)) targets
                                     in map (\target -> 
                                        let nbDocs = (sum . elems)
                                                   $ filterDocs docs ([idToPrd (getGroupId source), idToPrd (getGroupId target)])
                                            diago  = reduceDiagos  
                                                   $ filterDiago diagos ([idToPrd (getGroupId source), idToPrd (getGroupId target)])
                                         in ((source,target),toSimilarity nbDocs diago (getSimilarity phylo) (getGroupNgrams source) (getGroupNgrams target) (getGroupNgrams target))
                                          ) candidates
                                ) sources
                      pairs'  = pairs `using` parList rdeepseq
                   in acc ++ (concat pairs')
               ) [] $ keys $ phylo ^. phylo_periods

appendGroups :: (a -> Period -> (Text,Text) -> Scale -> Int -> [Cooc] -> PhyloGroup) -> Scale -> Map (Date,Date) [a] -> Phylo -> Phylo
appendGroups f lvl m phylo =  trace ("\n" <> "-- | Append " <> show (length $ concat $ elems m) <> " groups to Level " <> show (lvl) <> "\n")
    $ over ( phylo_periods
           .  traverse
           . phylo_periodScales
           .  traverse)
           (\phyloLvl -> if lvl == (phyloLvl ^. phylo_scaleScale)
                         then
                            let pId  = phyloLvl ^. phylo_scalePeriod
                                pId' = phyloLvl ^. phylo_scalePeriodStr
                                phyloCUnit = m ! pId
                            in  phyloLvl 
                              & phylo_scaleGroups .~ (fromList $ foldl (\groups obj ->
                                    groups ++ [ (((pId,lvl),length groups)
                                              , f obj pId pId' lvl (length groups)
                                                  (elems $ restrictKeys (getCoocByDate phylo) $ periodsToYears [pId]))
                                              ] ) [] phyloCUnit)
                         else 
                            phyloLvl )
           phylo  


clusterToGroup :: Clustering -> Period -> (Text,Text) -> Scale ->  Int -> [Cooc] -> PhyloGroup
clusterToGroup fis pId pId' lvl idx coocs = PhyloGroup pId pId' lvl idx ""
                   (fis ^. clustering_support )
                   (fis ^. clustering_visWeighting)
                   (fis ^. clustering_visFiltering)
                   (fis ^. clustering_roots)
                   (ngramsToCooc (fis ^. clustering_roots) coocs)
                   (1,[0]) -- branchid (lvl,[path in the branching tree])
                   (fromList [("breaks",[0]),("seaLevels",[0])])
                   [] [] [] [] [] [] []


-----------------------
-- | To Phylo Step | --
-----------------------    


indexDates' :: Map (Date,Date) [Document] -> Map (Date,Date) (Text,Text)  
indexDates' m = map (\docs -> 
  let ds = map (\d -> date' d) docs
      f = if (null ds)
            then ""
            else toFstDate ds
      l = if (null ds) 
            then ""
            else toLstDate ds
   in (f,l)) m


-- create a map of roots and group ids
joinRoots :: Phylo -> Phylo
joinRoots phylo = set (phylo_foundations . foundations_rootsInGroups) rootsMap phylo
  where
    --------------------------------------
    rootsMap :: Map Int [PhyloGroupId]
    rootsMap = fromListWith (++)
             $ concat -- flatten
             $ map (\g -> 
                        map (\n -> (n,[getGroupId g])) $ _phylo_groupNgrams g) 
             $ getGroupsFromScale 1 phylo


maybeDefaultParams :: Phylo -> Phylo
maybeDefaultParams phylo =  if (defaultMode (getConfig phylo))
                           then findDefaultLevel phylo
                           else phylo


-- To build the first phylo step from docs and terms
-- QL: backend entre phyloBase et Clustering
-- tophylowithoutLink
toPhyloWithoutLink :: [Document] -> PhyloConfig -> Phylo
toPhyloWithoutLink docs conf = joinRoots
                             $ findSeaLadder 
                             $ maybeDefaultParams
                             $ appendGroups clusterToGroup 1 seriesOfClustering (updatePeriods (indexDates' docs') phyloBase)
    where
        --------------------------------------
        seriesOfClustering :: Map (Date,Date) [Clustering]
        seriesOfClustering =  toSeriesOfClustering phyloBase docs'
        --------------------------------------
        docs' :: Map (Date,Date) [Document]
        -- QL: Time Consuming here
        docs' =  groupDocsByPeriodRec date (getPeriodIds phyloBase) (sortOn date docs) empty
        --------------------------------------
        phyloBase :: Phylo
        phyloBase = initPhylo docs conf
        --------------------------------------

---------------------------
-- | Frequent Item Set | --
---------------------------


--  To apply a filter with the possibility of keeping some periods non empty (keep : True|False)
filterClique :: Bool -> Int -> (Int -> [Clustering] -> [Clustering]) -> Map (Date, Date) [Clustering] -> Map (Date, Date) [Clustering]
filterClique keep thr f m = case keep of
  False -> map (\l -> f thr l) m
  True  -> map (\l -> keepFilled (f) thr l) m


--  To filter Fis with small Support
filterCliqueBySupport :: Int -> [Clustering] -> [Clustering]
filterCliqueBySupport thr l = filter (\clq -> (clq ^. clustering_support ) >= thr) l


--  To filter Fis with small Clique size
filterCliqueBySize :: Int -> [Clustering] -> [Clustering]
filterCliqueBySize thr l = filter (\clq -> (length $ clq ^. clustering_roots) >= thr) l


--  To filter nested Fis
filterCliqueByNested :: Map (Date, Date) [Clustering] -> Map (Date, Date) [Clustering]
filterCliqueByNested m = 
  let clq  = map (\l -> 
                foldl (\mem f -> if (any (\f' -> isNested (f' ^. clustering_roots) (f ^. clustering_roots)) mem)
                                 then mem
                                 else 
                                    let fMax = filter (\f' -> not $ isNested (f ^. clustering_roots) (f' ^. clustering_roots)) mem
                                    in  fMax ++ [f] ) [] l)
           $ elems m 
      clq' = clq `using` parList rdeepseq
  in  fromList $ zip (keys m) clq' 


-- | To transform a time map of docs into a time map of Fis with some filters
toSeriesOfClustering :: Phylo -> Map (Date, Date) [Document] -> Map (Date,Date) [Clustering]
toSeriesOfClustering phylo phyloDocs = case (clique $ getConfig phylo) of 
    Fis s s'    -> -- traceFis "Filtered Fis"
                   filterCliqueByNested 
                 {- \$ traceFis "Filtered by clique size" -}
                 $ filterClique True s' (filterCliqueBySize)
                 {- \$ traceFis "Filtered by support" -}
                 $ filterClique True s (filterCliqueBySupport)
                 {- \$ traceFis "Unfiltered Fis" -}
                 seriesOfClustering
    MaxClique s _ _ -> filterClique True s (filterCliqueBySize)
                       seriesOfClustering
    where
        -------------------------------------- 
        seriesOfClustering :: Map (Date,Date) [Clustering]
        seriesOfClustering = case (clique $ getConfig phylo) of 
          Fis _ _     ->  
                      let fis  = map (\(prd,docs) -> 
                                      case (corpusParser $ getConfig phylo) of
                                        Csv' _  -> let lst = toList 
                                                                  $ fisWithSizePolyMap' (Segment 1 20) 1 (map (\d -> (ngramsToIdx (text d) (getRoots phylo), (weight d, (sourcesToIdx (sources d) (getSources phylo))))) docs)
                                                           in (prd, map (\f -> Clustering (Set.toList $ fst f) ((fst . snd) f) prd ((fst . snd . snd) f) (((snd . snd . snd) f))) lst)
                                        _  -> let lst = toList 
                                                      $ fisWithSizePolyMap (Segment 1 20) 1 (map (\d -> ngramsToIdx (text d) (getRoots phylo)) docs)
                                              in (prd, map (\f -> Clustering (Set.toList $ fst f) (snd f) prd (Just $ fromIntegral $ snd f) []) lst)
                                      )
                               $ toList phyloDocs
                          fis' = fis `using` parList rdeepseq
                       in fromList fis'
          MaxClique _ thr filterType -> 
                      let mcl  = map (\(prd,docs) -> 
                                    let cooc = map round
                                             $ foldl sumCooc empty
                                             $ map listToMatrix 
                                             $ map (\d -> ngramsToIdx (text d) (getRoots phylo)) docs
                                     in (prd, map (\cl -> Clustering cl 0 prd Nothing []) $ getMaxCliques filterType Conditional thr cooc)) 
                               $ toList phyloDocs
                          mcl' = mcl `using` parList rdeepseq                               
                       in fromList mcl' 
        -------------------------------------- 

        -- dev viz graph maxClique getMaxClique


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

-- TODO anoe
groupDocsByPeriodRec :: (NFData doc, Ord date, Enum date) => (doc -> date) -> [(date,date)] -> [doc] -> Map (date, date) [doc] -> Map (date, date) [doc]
groupDocsByPeriodRec f prds docs acc =
    if ((null prds) || (null docs))
      then acc 
      else 
        let prd = head' "groupBy" prds
            docs' = partition (\d -> (f d >= fst prd) && (f d <= snd prd)) docs
         in groupDocsByPeriodRec f (tail prds) (snd docs') (insert prd (fst docs') acc)


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


docsToTermCount :: [Document] -> Vector Ngrams -> Map Int Double
docsToTermCount docs roots = fromList
             $ map (\lst -> (head' "docsToTermCount" lst, fromIntegral $ length lst)) 
             $ group $ sort $ concat $ map (\d -> nub $ ngramsToIdx (text d) roots) docs



docsToLastTermFreq :: Int -> [Document] -> Vector Ngrams -> Map Int Double
docsToLastTermFreq n docs fdt = 
  let last   = take n $ reverse $ sort $ map date docs
      nbDocs = fromIntegral $ length $ filter (\d -> elem (date d) last) docs
      freqs  = map (/(nbDocs))
             $ fromList
             $ map (\lst -> (head' "docsToLastTermFreq" lst, fromIntegral $ length lst)) 
             $ group $ sort $ concat $ map (\d -> nub $ ngramsToIdx (text d) fdt) $ filter (\d -> elem (date d) last) docs
      sumFreqs = sum $ elems freqs
   in map (/sumFreqs) freqs  


--  To count the number of docs by unit of time
docsToTimeScaleNb :: [Document] -> Map Date Double
docsToTimeScaleNb docs = 
    let docs' = fromListWith (+) $ map (\d -> (date d,1)) docs
        time  = fromList $ map (\t -> (t,0)) $ toTimeScale (keys docs') 1
    in  trace ("\n" <> "-- | Group " <> show(length docs) <> " docs by " <> show(length time) <> " unit of time" <> "\n") 
      $ unionWith (+) time docs'


initPhyloScales :: Int -> Period -> Map PhyloScaleId PhyloScale
initPhyloScales lvlMax pId = 
    fromList $ map (\lvl -> ((pId,lvl),PhyloScale pId ("","") lvl empty)) [1..lvlMax]


setDefault :: PhyloConfig -> PhyloConfig
setDefault conf = conf { 
                    phyloScale = 2,
                    similarity = WeightedLogJaccard 0.5 2,
                    findAncestors = True,
                    phyloSynchrony = ByProximityThreshold 0.6 0 SiblingBranches MergeAllGroups,
                    phyloQuality = Quality 0.5 3,
                    timeUnit = Year 3 1 3,
                    clique = MaxClique 5 30 ByNeighbours,
                    exportLabel = [BranchLabel MostEmergentTfIdf 2, GroupLabel MostEmergentInclusive 2],
                    exportSort = ByHierarchy Desc,
                    exportFilter = [ByBranchSize 3]
                  }


--  Init the basic elements of a Phylo
--
initPhylo :: [Document] -> PhyloConfig -> Phylo
initPhylo docs conf = 
    let roots = Vector.fromList $ nub $ concat $ map text docs
        foundations = PhyloFoundations roots empty
        docsSources = PhyloSources (Vector.fromList $ nub $ concat $ map sources docs)
        docsCounts  = PhyloCounts (docsToTimeScaleCooc docs (foundations ^. foundations_roots))
                             (docsToTimeScaleNb docs)
                             (docsToTermCount docs (foundations ^. foundations_roots))
                             (docsToTermFreq docs (foundations ^. foundations_roots))
                             (docsToLastTermFreq (getTimePeriod $ timeUnit conf) docs (foundations ^. foundations_roots))                             
        params = if (defaultMode conf)
                 then defaultPhyloParam { _phyloParam_config = setDefault conf }
                 else defaultPhyloParam { _phyloParam_config = conf }
        periods = toPeriods (sort $ nub $ map date docs) (getTimePeriod $ timeUnit conf) (getTimeStep $ timeUnit conf)
    in trace ("\n" <> "-- | Init a phylo out of " <> show(length docs) <> " docs \n") 
       $ Phylo foundations
               docsSources
               docsCounts
               []
               params
               (fromList $ map (\prd -> (prd, PhyloPeriod prd ("","") (initPhyloScales 1 prd))) periods)
               0
               (_qua_granularity $ phyloQuality $ conf)
