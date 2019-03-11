{-|
Module      : Gargantext.Viz.Phylo.Example
Description : Phylomemy example based on history of Cleopatre.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-- | Cesar et Cleôpatre
-- Exemple de phylomemie
-- French without accents


TODO:
- split the functions : RAW -> Document -> Ngrams

-- reverse history: antechronologique
-- metrics support


-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Viz.Phylo.Example where

import Control.Lens     hiding (makeLenses, both, Level)

import Data.Bool        (Bool, not)
import Data.List        (concat, union, intersect, tails, tail, head, last, null, zip, sort, length, any, (++), (!!), nub, sortOn, reverse, splitAt, take, delete, init)
import Data.Map         (Map, elems, member, adjust, singleton, empty, (!), keys, restrictKeys, mapWithKey, filterWithKey, mapKeys, intersectionWith, unionWith)
import Data.Semigroup   (Semigroup)
import Data.Set         (Set)
import Data.Text        (Text, unwords, toLower, words)
import Data.Tuple       (fst, snd)
import Data.Tuple.Extra
import Data.Vector      (Vector, fromList, elemIndex)

import Gargantext.Prelude                      hiding (head)
import Gargantext.Text.Metrics.FrequentItemSet (fisWithSizePolyMap, Size(..))
import Gargantext.Text.Terms.Mono              (monoTexts)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

import qualified Data.Bool   as Bool
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Maybe  as Maybe
import qualified Data.Set    as Set
import qualified Data.Tuple  as Tuple
import qualified Data.Vector as Vector


------------------------------------------------------------------------
-- | STEP 14 | -- Incrementaly cluster the PhyloGroups n times, link them through the Periods and build level n of the Phylo   


------------------------------------------------------------------------
-- | STEP 13 | -- Cluster the Fis


-- | To do : ajouter de nouveaux clusters / proxi
-- gérer les cooc à level 2 et +, idem pour les quality
-- réfléchir aux formats de sortie 


-- | To apply a Clustering method to a PhyloGraph
graphToClusters :: (Clustering,[Double]) -> PhyloGraph -> [[PhyloGroup]]
graphToClusters (clust,param) (nodes,edges) = case clust of 
  Louvain           -> undefined
  RelatedComponents -> relatedComp 0 (head nodes) (tail nodes,edges) [] []   


-- | To transform a Phylo into Clusters of PhyloGroups at a given level
phyloToClusters :: Level -> (Proximity,[Double]) -> (Clustering,[Double]) -> Phylo -> Map (Date,Date) [[PhyloGroup]]
phyloToClusters lvl (prox,param) (clus,param') p = Map.fromList 
                                                 $ zip (getPhyloPeriods p) 
                                                       (map (\prd -> let graph = groupsToGraph (prox,param) (getGroupsWithFilters lvl prd p) p
                                                                     in if null (fst graph) 
                                                                        then []
                                                                        else graphToClusters (clus,param') graph) 
                                                       (getPhyloPeriods p))


-- | To transform a Cluster into  a Phylogroup 
clusterToGroup :: PhyloPeriodId -> Level -> Int -> Text -> [PhyloGroup] -> PhyloGroup
clusterToGroup prd lvl idx lbl groups = PhyloGroup ((prd, lvl), idx)
                                                   lbl
                                                   ((sort . nub . concat) $ map getGroupNgrams groups)
                                                   empty
                                                   empty
                                                   [] [] []
                                                   (map (\g -> (getGroupId g, 1)) groups)

-- | To transform a list of Clusters into a new Phylolevel
clustersToPhyloLevel :: Level -> Map (Date,Date) [[PhyloGroup]] -> Phylo -> Phylo
clustersToPhyloLevel lvl m p = over (phylo_periods . traverse)
                                    (\period -> 
                                      let periodId = _phylo_periodId period
                                          clusters = zip [1..] (m ! periodId)
                                      in  over (phylo_periodLevels)
                                          (\levels -> 
                                            let groups = map (\cluster -> clusterToGroup periodId lvl (fst cluster) "" (snd cluster)) clusters
                                            in  levels ++ [PhyloLevel (periodId, lvl) groups]
                                          ) period) p


phyloWithGroups2 = clustersToPhyloLevel 2 (phyloToClusters 1 (WeightedLogJaccard,[0]) (RelatedComponents, []) phyloWithBranches_1) phyloWithBranches_1

------------------------------------------------------------------------
-- | STEP 12 | -- Find the Branches


-- | To apply the related components method to a PhyloGraph
-- curr = the current PhyloGroup 
-- (nodes,edges) = the initial PhyloGraph minus the current PhyloGroup
-- next = the next PhyloGroups to be added in the cluster  
-- memo = the memory of the allready created clusters
relatedComp :: Int -> PhyloGroup -> PhyloGraph -> [PhyloGroup] -> [[PhyloGroup]] -> [[PhyloGroup]]
relatedComp idx curr (nodes,edges) next memo
  | null nodes' && null next' = memo'
  | (not . null) next'        = relatedComp idx (head next') (nodes',edges) (tail next') memo'
  | otherwise                 = relatedComp (idx + 1) (head nodes') (tail nodes',edges) [] memo'
  where
    --------------------------------------
    memo' :: [[PhyloGroup]]
    memo' 
      | null memo                  = [[curr]]
      | idx == ((length memo) - 1) = (init memo) ++ [(last memo) ++ [curr]]
      | otherwise                  = memo ++ [[curr]]
    --------------------------------------
    next' :: [PhyloGroup]
    next' = filter (\x -> not $ elem x $ concat memo) $ nub $ next ++ (getNeighbours False curr edges)
    --------------------------------------
    nodes' :: [PhyloGroup]
    nodes' = filter (\x -> not $ elem x next') nodes
    --------------------------------------


-- | To transform a PhyloGraph into a list of PhyloBranches by using the relatedComp clustering
graphToBranches :: Level -> PhyloGraph -> Phylo -> [PhyloBranch]
graphToBranches lvl (nodes,edges) p = map (\(idx,c) -> PhyloBranch (lvl,idx) "" (map getGroupId c)) $ zip [0..] clusters
  where
    -------------------------------------- 
    clusters :: [[PhyloGroup]]
    clusters = relatedComp 0 (head nodes) (tail nodes,edges) [] []
    --------------------------------------


-- | To transform a list of PhyloGroups into a PhyloGraph by using a given Proximity mesure
groupsToGraph :: (Proximity,[Double]) -> [PhyloGroup] -> Phylo -> PhyloGraph
groupsToGraph (prox,param) groups p = (groups,edges)
  where 
    edges :: PhyloEdges
    edges = case prox of  
      FromPairs          -> (nub . concat) $ map (\g -> (map (\g' -> ((g',g),1)) $ getGroupParents g p)
                                                        ++
                                                        (map (\g' -> ((g,g'),1)) $ getGroupChilds g p)) groups 
      WeightedLogJaccard -> map (\(x,y) -> ((x,y), weightedLogJaccard 
                                                   (param !! 0) (getGroupCooc x) 
                                                   (unifySharedKeys (getGroupCooc x) (getGroupCooc y)))) $ listToDirectedCombi groups
      _                  -> undefined 


-- | To set all the PhyloBranches for a given Level in a Phylo
setPhyloBranches :: Level -> Phylo -> Phylo 
setPhyloBranches lvl p = alterPhyloBranches 
                          (\branches -> branches 
                                        ++
                                        (graphToBranches lvl (groupsToGraph (FromPairs,[]) (getGroupsWithLevel lvl p)p)p))p


phyloWithBranches_1 = setPhyloBranches 1 phyloWithPair_1_Childs


------------------------------------------------------------------------
-- | STEP 11 | -- Link the PhyloGroups of level 1 through the Periods  


-- | To process the weightedLogJaccard between two PhyloGroups fields
weightedLogJaccard :: Double -> Map (Int, Int) Double -> Map (Int, Int) Double -> Double
weightedLogJaccard s f1 f2
  | null wUnion      = 0
  | wUnion == wInter = 1 
  | s == 0           = (fromIntegral $ length wInter)/(fromIntegral $ length wUnion)
  | s > 0            = (sumInvLog wInter)/(sumInvLog wUnion)
  | otherwise        = (sumLog wInter)/(sumLog wUnion)
  where 
    --------------------------------------
    wInter :: [Double]
    wInter = elems $ intersectionWith (+) f1 f2
    --------------------------------------
    wUnion :: [Double]
    wUnion = elems $ unionWith (+) f1 f2  
    --------------------------------------
    sumInvLog :: [Double] -> Double
    sumInvLog l = foldl (\mem x -> mem + (1 / log (s + x))) 0 l
    --------------------------------------
    sumLog :: [Double] -> Double
    sumLog l = foldl (\mem x -> mem + log (s + x)) 0 l  
    --------------------------------------  


-- | To apply the corresponding proximity function based on a given Proximity
getProximity :: (Proximity,[Double]) -> PhyloGroup -> PhyloGroup -> (PhyloGroupId, Double)
getProximity (prox,param) g1 g2 = case prox of 
  WeightedLogJaccard -> ((getGroupId g2),weightedLogJaccard (param !! 0) (getGroupCooc g1) (unifySharedKeys (getGroupCooc g2) (getGroupCooc g1)))
  _                  -> panic ("[ERR][Viz.Phylo.Example.getProximity] Proximity function not defined")


-- | To get the next or previous PhyloPeriod based on a given PhyloPeriodId
getNextPeriods :: PairTo -> PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId] 
getNextPeriods to id l = case to of 
    Childs  -> unNested id ((tail . snd) next) 
    Parents -> unNested id ((reverse . fst) next)
    _       -> panic ("[ERR][Viz.Phylo.Example.getNextPeriods] PairTo type not defined")
    where 
      --------------------------------------
      next :: ([PhyloPeriodId], [PhyloPeriodId])
      next = splitAt idx l
      --------------------------------------
      idx :: Int
      idx = case (List.elemIndex id l) of 
        Nothing -> panic ("[ERR][Viz.Phylo.Example.getNextPeriods] PhyloPeriodId not defined")
        Just i  -> i
      --------------------------------------
      -- | To have an non-overlapping next period
      unNested :: PhyloPeriodId -> [PhyloPeriodId] -> [PhyloPeriodId]
      unNested x l
        | null l                  = []
        | nested (fst $ head l) x = unNested x (tail l)
        | nested (snd $ head l) x = unNested x (tail l)
        | otherwise               = l
      --------------------------------------
      nested :: Date -> PhyloPeriodId -> Bool
      nested d prd = d >= fst prd && d <= snd prd
      --------------------------------------


-- | To find the best set (max = 2) of Childs/Parents candidates based on a given Proximity mesure until a maximum depth (max = Period + 5 units )  
findBestCandidates :: PairTo -> Int -> Int -> Double -> (Proximity,[Double]) -> PhyloGroup -> Phylo -> [(PhyloGroupId, Double)]
findBestCandidates to depth max thr (prox,param) group p
  | depth > max || null next = [] 
  | (not . null) best = take 2 best
  | otherwise = findBestCandidates to (depth + 1) max thr (prox,param) group p   
  where
    --------------------------------------
    next :: [PhyloPeriodId]
    next = getNextPeriods to (getGroupPeriod group) (getPhyloPeriods p)
    --------------------------------------
    candidates :: [PhyloGroup] 
    candidates = getGroupsWithFilters (getGroupLevel group) (head next) p
    --------------------------------------
    scores :: [(PhyloGroupId, Double)]
    scores = map (\group' -> getProximity (prox,param) group group') candidates
    --------------------------------------
    best :: [(PhyloGroupId, Double)]
    best = reverse
         $ sortOn snd 
         $ filter (\(id,score) -> score >= thr) scores
    --------------------------------------


-- | To add a new list of Pointers into an existing Childs/Parents list of Pointers 
makePair :: PairTo -> PhyloGroup -> [(PhyloGroupId, Double)] -> PhyloGroup
makePair to group ids = case to of 
    Childs  -> over (phylo_groupPeriodChilds) addPointers group
    Parents -> over (phylo_groupPeriodParents) addPointers group
    _       -> panic ("[ERR][Viz.Phylo.Example.makePair] PairTo type not defined")
    where
      -------------------------------------- 
      addPointers :: [Pointer] -> [Pointer]
      addPointers l = nub $ (l ++ ids)
      --------------------------------------


-- | To pair all the Phylogroups of given PhyloLevel to their best Parents or Childs
pairGroupsToGroups :: PairTo -> Level -> Double -> (Proximity,[Double]) -> Phylo -> Phylo
pairGroupsToGroups to lvl thr (prox,param) p = alterPhyloGroups
                                    (\groups -> 
                                      map (\group ->
                                            if (getGroupLevel group) == lvl
                                            then 
                                              let
                                                --------------------------------------
                                                candidates :: [(PhyloGroupId, Double)] 
                                                candidates = findBestCandidates to 1 5 thr (prox,param) group p
                                                --------------------------------------
                                              in 
                                                makePair to group candidates
                                            else 
                                              group ) groups) p


phyloWithPair_1_Childs :: Phylo
phyloWithPair_1_Childs = pairGroupsToGroups Childs 1 0.01 (WeightedLogJaccard,[0]) phyloWithPair_1_Parents


phyloWithPair_1_Parents :: Phylo
phyloWithPair_1_Parents = pairGroupsToGroups Parents 1 0.01 (WeightedLogJaccard,[0]) phyloLinked_0_1


------------------------------------------------------------------------
-- | STEP 10 | -- Build the coocurency Matrix of the Phylo 


-- | To transform the Fis into a coocurency Matrix in a Phylo 
fisToCooc :: Map (Date, Date) [Fis] -> Phylo -> Map (Int, Int) Double
fisToCooc m p = map   (/docs)
              $ foldl (\mem x -> adjust (+1) (getKeyPair x mem) mem) cooc
              $ concat
              $ map (\x -> listToUnDirectedCombiWith (\x -> ngramsToIdx x p) $ (Set.toList . fst) x) 
              $ (concat . elems) m
  where
    --------------------------------------
    fisNgrams :: [Ngrams]
    fisNgrams = foldl (\mem x -> union mem $ (Set.toList . fst) x) [] $ (concat . elems) m
    --------------------------------------
    docs :: Double
    docs = fromIntegral $ foldl (\mem x -> mem + (snd x)) 0 $ (concat . elems) m
    --------------------------------------
    cooc :: Map (Int, Int) (Double)
    cooc = Map.fromList $ map (\x -> (x,0)) (listToUnDirectedCombiWith (\x -> ngramsToIdx x p) fisNgrams)
    --------------------------------------


phyloCooc :: Map (Int, Int) Double
phyloCooc = fisToCooc phyloFisFiltered phyloLinked_0_1


------------------------------------------------------------------------
-- | STEP 9 | -- Build level 1 of the Phylo 


-- | To Cliques into Groups
cliqueToGroup :: PhyloPeriodId -> Level -> Int -> Ngrams -> (Clique,Support) -> Map (Date, Date) [Fis] -> Phylo -> PhyloGroup
cliqueToGroup period lvl idx label fis m p = 
  PhyloGroup ((period, lvl), idx) label ngrams (singleton "support" (fromIntegral $ snd fis)) cooc [] [] [] []
  where
    --------------------------------------
    ngrams :: [Int]
    ngrams = sort $ map (\x -> ngramsToIdx x p)
                  $ Set.toList
                  $ fst fis
    --------------------------------------
    cooc :: Map (Int, Int) Double 
    cooc =  filterWithKey (\k _ -> elem (fst k) ngrams && elem (snd k) ngrams) 
                                 $ fisToCooc (restrictKeys m $ Set.fromList [period]) p
    -------------------------------------- 


-- | To transform Fis into PhyloLevels
fisToPhyloLevel :: Map (Date, Date) [Fis] -> Phylo -> Phylo
fisToPhyloLevel m p = over (phylo_periods . traverse)
                           (\period ->
                              let periodId = _phylo_periodId period
                                  fisList  = zip [1..] (m ! periodId)
                              in  over (phylo_periodLevels)
                                       (\phyloLevels ->
                                          let groups = map (\fis -> cliqueToGroup periodId 1 (fst fis) "" (snd fis) m p) fisList
                                          in  phyloLevels ++ [PhyloLevel (periodId, 1) groups] 
                                       ) period ) p


phyloLinked_0_1 :: Phylo
phyloLinked_0_1 = alterLevelLinks (0,1)  phyloLinked_1_0


phyloLinked_1_0 :: Phylo
phyloLinked_1_0 = alterLevelLinks (1,0)  phyloWithGroups1


phyloWithGroups1 :: Phylo
phyloWithGroups1 = updatePhyloByLevel 1 phyloLinked_m1_0


------------------------------------------------------------------------
-- | STEP 8 | -- Create Frequent Items Sets by Period and filter them


-- | To Filter Fis by support 
filterFisBySupport :: Bool -> Int -> Map (Date, Date) [Fis] -> Map (Date, Date) [Fis]
filterFisBySupport empty min m = case empty of
  True  -> Map.map (\l -> filterMinorFis min l) m
  False -> Map.map (\l -> keepFilled (filterMinorFis) min l) m


-- | To filter Fis with small Support, to preserve nonempty periods please use : filterFisBySupport False
filterMinorFis :: Int -> [Fis] -> [Fis]
filterMinorFis min l = filter (\fis -> snd fis > min) l


-- | To filter nested Fis 
filterFisByNested :: Map (Date, Date) [Fis] -> Map (Date, Date) [Fis]
filterFisByNested = map (\l -> let cliqueMax = filterNestedSets (head $ map fst l) (map fst l) []
                               in  filter (\fis -> elem (fst fis) cliqueMax) l)


-- | To transform a list of Documents into a Frequent Items Set 
docsToFis :: Map (Date, Date) [Document] -> Map (Date, Date) [Fis]
docsToFis docs = map (\d -> Map.toList 
                          $ fisWithSizePolyMap (Segment 1 20) 1 (map (words . text) d)) docs


phyloFisFiltered :: Map (Date, Date) [Fis]
phyloFisFiltered = filterFisBySupport True 1 (filterFisByNested phyloFis)


phyloFis :: Map (Date, Date) [Fis]
phyloFis = docsToFis phyloPeriods 


------------------------------------------------------------------------
-- | STEP 7 | -- Link level -1 to level 0


phyloLinked_m1_0 :: Phylo
phyloLinked_m1_0 = alterLevelLinks ((-1),0)  phyloLinked_0_m1


------------------------------------------------------------------------
-- | STEP 6 | -- Link level 0 to level -1


-- | To set the LevelLinks between a given PhyloGroup and a list of childs/parents PhyloGroups
linkGroupToGroups :: (Level,Level) -> PhyloGroup -> [PhyloGroup] -> PhyloGroup
linkGroupToGroups (lvl,lvl') current targets 
  | lvl < lvl' = setLevelParents current 
  | lvl > lvl' = setLevelChilds current
  | otherwise = current  
  where
    -------------------------------------- 
    setLevelChilds :: PhyloGroup -> PhyloGroup 
    setLevelChilds =  over (phylo_groupLevelChilds) addPointers 
    --------------------------------------
    setLevelParents :: PhyloGroup -> PhyloGroup 
    setLevelParents =  over (phylo_groupLevelParents) addPointers
    --------------------------------------
    addPointers :: [Pointer] -> [Pointer]
    addPointers lp = lp ++ Maybe.mapMaybe (\target -> 
                                            if shouldLink (lvl,lvl') 
                                                          (_phylo_groupNgrams current)
                                                          (_phylo_groupNgrams target )
                                            then Just ((getGroupId target),1)
                                            else Nothing) targets 
    --------------------------------------


-- | To set the LevelLinks between two lists of PhyloGroups 
linkGroupsByLevel :: (Level,Level) -> Phylo -> [PhyloGroup] -> [PhyloGroup]
linkGroupsByLevel (lvl,lvl') p groups  = map (\group ->
                                              if getGroupLevel group == lvl 
                                              then linkGroupToGroups (lvl,lvl') group (getGroupsWithFilters lvl' (getGroupPeriod group) p)
                                              else group) groups


-- | To set the LevelLink of all the PhyloGroups of a Phylo
alterLevelLinks :: (Level,Level) -> Phylo -> Phylo
alterLevelLinks (lvl,lvl') p = alterPhyloGroups (linkGroupsByLevel (lvl,lvl') p) p


phyloLinked_0_m1 :: Phylo
phyloLinked_0_m1 = alterLevelLinks (0,(-1)) phyloWithGroups0


------------------------------------------------------------------------
-- | STEP 5 | -- Build level 0 as a copy of level -1


-- | To clone the last PhyloLevel of each PhyloPeriod and update it with a new LevelValue 
clonePhyloLevel :: Level -> Phylo -> Phylo
clonePhyloLevel lvl p = alterPhyloLevels (\l -> addPhyloLevel (setPhyloLevelId lvl $ head l) l) p 


phyloWithGroups0 :: Phylo
phyloWithGroups0 = updatePhyloByLevel 0 phyloWithGroupsm1


------------------------------------------------------------------------
-- | STEP 4 | -- Build level -1 


-- | To transform a list of Documents into a PhyloLevel
docsToPhyloLevel :: Level -> (Date, Date) -> [Document] -> Phylo -> PhyloLevel
docsToPhyloLevel lvl (d, d') docs p = initPhyloLevel 
                                        ((d, d'), lvl)
                                        (map (\(f,s) -> initGroup [s] s f lvl d d' p) 
                                             $ zip [1..] 
                                             $ (nub . concat) 
                                             $ map (words . text) docs) 


-- | To transform a Map of Periods and Documents into a list of PhyloPeriods
docsToPhyloPeriods :: Level -> Map (Date,Date) [Document] -> Phylo -> [PhyloPeriod]
docsToPhyloPeriods lvl docs p = map (\(id,l) -> initPhyloPeriod id l) 
                                    $ Map.toList 
                                    $ mapWithKey (\k v -> [docsToPhyloLevel lvl k v p]) docs 


-- | To update a Phylo for a given Levels
updatePhyloByLevel :: Level -> Phylo -> Phylo 
updatePhyloByLevel lvl p
  | lvl < 0   = appendPhyloPeriods (docsToPhyloPeriods lvl phyloPeriods p) p
  | lvl == 0  = clonePhyloLevel lvl p
  | lvl == 1  = fisToPhyloLevel phyloFisFiltered p
  | lvl > 1   = undefined
  | otherwise = panic ("[ERR][Viz.Phylo.Example.updatePhyloByLevel] Level not defined")


phyloWithGroupsm1 :: Phylo
phyloWithGroupsm1 = updatePhyloByLevel (-1) phylo


------------------------------------------------------------------------
-- | STEP 3 | -- Parse the Documents and group them by Periods


-- | To init a set of periods out of a given Grain and Step 
docsToPeriods :: (Ord date, Enum date) => (doc -> date)
     -> Grain -> Step -> [doc] -> Map (date, date) [doc]
docsToPeriods _ _ _ [] = panic "[ERR][Viz.Phylo.Example.docsToPeriods] Empty [Documents] can not have any periods"
docsToPeriods f g s es = Map.fromList $ zip hs $ map (inPeriode f es) hs
  where
    --------------------------------------
    hs = steps g s $ both f (head es, last es)
    --------------------------------------
    inPeriode :: Ord b => (t -> b) -> [t] -> (b, b) -> [t]
    inPeriode f' h (start,end) =
      fst $ List.partition (\d -> f' d >= start && f' d <= end) h
    --------------------------------------
    steps :: (Eq date, Enum date) => Grain -> Step -> (date, date) -> [(date, date)]
    steps s' o' (start,end) = map (\l -> (head l, last l))
                          $ chunkAlong s' o' [start .. end]
    --------------------------------------


-- | To parse a list of Documents by filtering on a Vector of Ngrams 
parseDocs :: PhyloNgrams -> [Document] -> [Document]
parseDocs l docs = map (\(Document d t) 
                        -> Document d ( unwords 
                                      $ filter (\x -> Vector.elem x l)
                                      $ monoTexts t)) docs


-- | To group a list of Documents by fixed periods
groupDocsByPeriod :: Grain -> Step -> [Document] -> Phylo -> Map (Date, Date) [Document]
groupDocsByPeriod g s docs p = docsToPeriods date g s $ parseDocs (getPhyloNgrams p) docs 


phyloPeriods :: Map (Date, Date) [Document]
phyloPeriods = groupDocsByPeriod 5 3 phyloDocs phylo


------------------------------------------------------------------------
-- | STEP 2 | -- Init an initial list of Ngrams and a Phylo  


phylo :: Phylo
phylo = initPhylo phyloDocs (initNgrams actants) 


------------------------------------------------------------------------
-- | STEP 1 | -- Get a list of Document 


-- | To transform a corpus of texts into a structured list of Documents
corpusToDocs :: [(Date, Text)] -> [Document]
corpusToDocs l = map (\(d,t) -> Document d t) l   


phyloDocs :: [Document]
phyloDocs = corpusToDocs corpus


------------------------------------------------------------------------
-- | STEP 0 | -- Let's start with an example


actants :: [Ngrams]
actants = [ "Cleopatre"   , "Ptolemee", "Ptolemee-XIII", "Ptolemee-XIV"
          , "Marc-Antoine", "Cesar"   , "Antoine"      , "Octave"  , "Rome"
          , "Alexandrie"  , "Auguste" , "Pompee"       , "Cassius" , "Brutus"]


corpus :: [(Date, Text)]
corpus = List.sortOn fst [ (-51,"Cleopatre règne sur l’egypte entre 51 et 30 av. J.-C. avec ses frères-epoux Ptolemee-XIII et Ptolemee-XIV, puis aux côtes du general romain Marc-Antoine. Elle est celèbre pour avoir ete la compagne de Jules Cesar puis d'Antoine, avec lesquels elle a eu plusieurs enfants. Partie prenante dans la guerre civile opposant Antoine à Octave, elle est vaincue à la bataille d'Actium en 31 av. J.-C. Sa defaite va permettre aux Romains de mener à bien la conquête de l’egypte, evenement qui marquera la fin de l'epoque hellenistique."), (-40,"Il existe relativement peu d'informations sur son sejour à Rome, au lendemain de l'assassinat de Cesar, ou sur la periode passee à Alexandrie durant l'absence d'Antoine, entre -40 et -37."), (-48,"L'historiographie antique lui est globalement defavorable car inspiree par son vainqueur, l'empereur Auguste, et par son entourage, dont l'interêt est de la noircir, afin d'en faire l'adversaire malfaisant de Rome et le mauvais genie d'Antoine. On observe par ailleurs que Cesar ne fait aucune mention de sa liaison avec elle dans les Commentaires sur la Guerre civile"), (-69,"Cleopatre est nee au cours de l'hiver -69/-686 probablement à Alexandrie."), (-48,"Pompee a en effet ete le protecteur de Ptolemee XII, le père de Cleopatre et de Ptolemee-XIII dont il se considère comme le tuteur."), (-48,"Ptolemee-XIII et Cleopatre auraient d'ailleurs aide Pompee par l'envoi d'une flotte de soixante navires."), (-48,"Mais le jeune roi Ptolemee-XIII et ses conseillers jugent sa cause perdue et pensent s'attirer les bonnes graces du vainqueur en le faisant assassiner à peine a-t-il pose le pied sur le sol egyptien, près de Peluse, le 30 juillet 48 av. J.-C., sous les yeux de son entourage."), (-48,"Cesar fait enterrer la tête de Pompee dans le bosquet de Nemesis en bordure du mur est de l'enceinte d'Alexandrie. Pour autant la mort de Pompee est une aubaine pour Cesar qui tente par ailleurs de profiter des querelles dynastiques pour annexer l’egypte."), (-48,"Il est difficile de se prononcer clairement sur les raisons qui ont pousse Cesar à s'attarder à Alexandrie. Il y a des raisons politiques, mais aussi des raisons plus sentimentales (Cleopatre ?). Il tente d'abord d'obtenir le remboursement de dettes que Ptolemee XII"), (-46,"Les deux souverains sont convoques par Cesar au palais royal d'Alexandrie. Ptolemee-XIII s'y rend après diverses tergiversations ainsi que Cleopatre."), (-47,"A Rome, Cleopatre epouse alors un autre de ses frères cadets, à Alexandrie, Ptolemee-XIV, sur l'injonction de Jules Cesar"), (-46,"Cesar a-t-il comme objectif de montrer ce qu'il en coûte de se revolter contre Rome en faisant figurer dans son triomphe la sœur de Cleopatre et de Ptolemee-XIV, Arsinoe, qui s'est fait reconnaître reine par les troupes de Ptolemee-XIII ?"), (-44,"Au debut de l'annee -44, Cesar est assassine par Brutus. Profitant de la situation confuse qui s'ensuit, Cleopatre quitte alors Rome à la mi-avril, faisant escale en Grèce. Elle parvient à Alexandrie en juillet -44."), (-44,"La guerre que se livrent les assassins de Cesar, Cassius et Brutus et ses heritiers, Octave et Marc-Antoine, oblige Cleopatre à des contorsions diplomatiques."), (-41,"Nous ignorons depuis quand Cleopatre, agee de 29 ans en -41, et Marc-Antoine, qui a une quarantaine d'annees, se connaissent. Marc-Antoine est l'un des officiers qui ont participe au retablissement de Ptolemee XII.  Il est plus vraisemblable qu'ils se soient frequentes lors du sejour à Rome de Cleopatre."), (-42,"Brutus tient la Grèce tandis que Cassius s'installe en Syrie. Le gouverneur de Cleopatre à Chypre, Serapion, vient en aide à Cassius."), (-42,"Cassius aurait envisage de s'emparer d'Alexandrie quand le 'debarquement' en Grèce d'Antoine et d'Octave l'oblige à renoncer à ses projets")]