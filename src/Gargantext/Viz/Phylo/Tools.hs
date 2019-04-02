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
{-# LANGUAGE ViewPatterns #-}

module Gargantext.Viz.Phylo.Tools
  where

import Control.Lens         hiding (both, Level)
import Data.List            (filter, intersect, (++), sort, null, head, tail, last, tails, delete, nub, concat, union, sortOn)
import Data.Maybe           (mapMaybe,fromMaybe)
import Data.Map             (Map, mapKeys, member, elems, adjust, (!))
import Data.Set             (Set)
import Data.Text            (Text, toLower)
import Data.Tuple.Extra
import Data.Vector          (Vector,elemIndex)
import Gargantext.Prelude   hiding (head)
import Gargantext.Viz.Phylo

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vector


--------------
-- | Misc | --
--------------


-- | Define a default value
def :: a -> Maybe a -> a
def = fromMaybe


-- | Does a List of Sets contains at least one Set of an other List
doesAnySetContains :: Eq a =>  Set a -> [Set a] -> [Set a] -> Bool
doesAnySetContains h l l' = any (\c -> doesContains (Set.toList c) (Set.toList h)) (l' ++ l)


-- | Does a list of A contains an other list of A
doesContains :: Eq a => [a] -> [a] -> Bool
doesContains l l'
  | null l'               = True
  | length l' > length l  = False
  | elem (head l') l      = doesContains l (tail l')
  | otherwise             = False


-- | Does a list of ordered A contains an other list of ordered A
doesContainsOrd :: Eq a => Ord a => [a] -> [a] -> Bool
doesContainsOrd l l'
  | null l'          = False
  | last l < head l' = False
  | head l' `elem` l = True
  | otherwise        = doesContainsOrd l (tail l')


-- | To filter nested Sets of a
filterNestedSets :: Eq a => Set a -> [Set a] -> [Set a] -> [Set a]
filterNestedSets h l l'
  | null l                 = if doesAnySetContains h l l'
                             then l'
                             else h : l'
  | doesAnySetContains h l l' = filterNestedSets (head l) (tail l) l'
  | otherwise              = filterNestedSets (head l) (tail l) (h : l')  



-- | To get the good pair of keys (x,y) or (y,x) in a given Map (a,b) c
getKeyPair :: (Int,Int) -> Map (Int,Int) a -> (Int,Int)
getKeyPair (x,y) m = case findPair (x,y) m of
                      Nothing -> panic "[ERR][Viz.Phylo.Example.getKeyPair] Nothing"
                      Just i  -> i
                     where
                      --------------------------------------
                      findPair :: (Int,Int) -> Map (Int,Int) a -> Maybe (Int,Int)
                      findPair (x,y) m
                        | member (x,y) m = Just (x,y)
                        | member (y,x) m = Just (y,x)
                        | otherwise      = Nothing
                      --------------------------------------  


-- | To filter Fis with small Support but by keeping non empty Periods
keepFilled :: (Int -> [a] -> [a]) -> Int -> [a] -> [a] 
keepFilled f thr l = if (null $ f thr l) && (not $ null l)
                     then keepFilled f (thr - 1) l
                     else f thr l  


-- | To get all combinations of a list
listToDirectedCombi :: Eq a => [a] -> [(a,a)]
listToDirectedCombi l = [(x,y) | x <- l, y <- l, x /= y]


-- | To get all combinations of a list and apply a function to the resulting list of pairs
listToDirectedCombiWith :: Eq a => forall b. (a -> b) -> [a] -> [(b,b)]
listToDirectedCombiWith f l = [(f x,f y) | x <- l, y <- l, x /= y]


-- | To get all combinations of a list with no repetition
listToUnDirectedCombi :: [a] -> [(a,a)]
listToUnDirectedCombi l = [ (x,y) | (x:rest) <- tails l,  y <- rest ]


-- | To get all combinations of a list with no repetition and apply a function to the resulting list of pairs
listToUnDirectedCombiWith :: forall a b. (a -> b) -> [a] -> [(b,b)]
listToUnDirectedCombiWith f l = [ (f x, f y) | (x:rest) <- tails l,  y <- rest ] 


-- | To unify the keys (x,y) that Map 1 share with Map 2 such as: (x,y) <=> (y,x)
unifySharedKeys :: Eq a => Ord a => Map (a,a) b -> Map (a,a) b -> Map (a,a) b
unifySharedKeys m1 m2 = mapKeys (\(x,y) -> if member (y,x) m2
                                           then (y,x)
                                           else (x,y) ) m1                      


---------------
-- | Phylo | --
---------------


-- | To init the foundation of the Phylo as a Vector of Ngrams 
initFoundations :: [Ngrams] -> Vector Ngrams
initFoundations l = Vector.fromList $ map toLower l

-- | To init the base of a Phylo from a List of Periods and Foundations
initPhyloBase :: [(Date, Date)] -> Vector Ngrams -> PhyloParam -> Phylo
initPhyloBase pds fds prm = Phylo ((fst . head) pds, (snd . last) pds) fds (map (\pd -> initPhyloPeriod pd []) pds) prm

-- | To init the param of a Phylo
initPhyloParam :: Maybe Text -> Maybe Software -> Maybe PhyloQuery -> PhyloParam
initPhyloParam (def defaultPhyloVersion -> v) (def defaultSoftware -> s) (def defaultQuery -> q) = PhyloParam v s q


-- | To get the foundations of a Phylo
getFoundations :: Phylo -> Vector Ngrams
getFoundations = _phylo_foundations


-- | To get the Index of a Ngrams in the Foundations of a Phylo
getIdxInFoundations :: Ngrams -> Phylo -> Int
getIdxInFoundations n p = case (elemIndex n (getFoundations p)) of
    Nothing  -> panic "[ERR][Viz.Phylo.Tools.getFoundationIdx] Ngrams not in Foundations"
    Just idx -> idx


-- | To get the last computed Level in a Phylo
getLastLevel :: Phylo -> Level 
getLastLevel p = (last . sort) 
               $ map (snd . getPhyloLevelId) 
               $ view ( phylo_periods
                      .  traverse
                      . phylo_periodLevels ) p


--------------------
-- | PhyloGroup | --
--------------------


-- | To alter a PhyloGroup matching a given Level
alterGroupWithLevel :: (PhyloGroup -> PhyloGroup) -> Level -> Phylo -> Phylo
alterGroupWithLevel f lvl p = over ( phylo_periods
                                   .  traverse
                                   . phylo_periodLevels
                                   .  traverse
                                   . phylo_levelGroups
                                   .  traverse
                                   ) (\g -> if getGroupLevel g == lvl
                                            then f g
                                            else g ) p  


-- | To alter each list of PhyloGroups following a given function
alterPhyloGroups :: ([PhyloGroup] -> [PhyloGroup]) -> Phylo -> Phylo
alterPhyloGroups f p = over ( phylo_periods
                            .  traverse
                            . phylo_periodLevels
                            .  traverse
                            . phylo_levelGroups
                            ) f p 


-- | To filter the PhyloGroup of a Phylo according to a function and a value
filterGroups :: Eq a => (PhyloGroup -> a) -> a -> [PhyloGroup] -> [PhyloGroup]
filterGroups f x l = filter (\g -> (f g) == x) l


-- | To maybe get the PhyloBranchId of a PhyloGroup
getGroupBranchId :: PhyloGroup -> Maybe PhyloBranchId
getGroupBranchId = _phylo_groupBranchId 


-- | To get the PhyloGroups Childs of a PhyloGroup
getGroupChilds :: PhyloGroup -> Phylo -> [PhyloGroup]
getGroupChilds g p = getGroupsFromIds (getGroupPeriodChildsId g) p


-- | To get the id of a PhyloGroup
getGroupId :: PhyloGroup -> PhyloGroupId
getGroupId = _phylo_groupId


-- | To get the Cooc Matrix of a PhyloGroup
getGroupCooc :: PhyloGroup -> Map (Int,Int) Double
getGroupCooc = _phylo_groupCooc


-- | To get the level out of the id of a PhyloGroup
getGroupLevel :: PhyloGroup -> Int
getGroupLevel = snd . fst . getGroupId


-- | To get the level child pointers of a PhyloGroup
getGroupLevelChilds :: PhyloGroup -> [Pointer]
getGroupLevelChilds = _phylo_groupLevelChilds


-- | To get the PhyloGroups Level Childs Ids of a PhyloGroup
getGroupLevelChildsId :: PhyloGroup -> [PhyloGroupId]
getGroupLevelChildsId g = map fst $ getGroupLevelChilds g


-- | To get the level parent pointers of a PhyloGroup
getGroupLevelParents :: PhyloGroup -> [Pointer]
getGroupLevelParents = _phylo_groupLevelParents


-- | To get the PhyloGroups Level Parents Ids of a PhyloGroup
getGroupLevelParentsId :: PhyloGroup -> [PhyloGroupId]
getGroupLevelParentsId g = map fst $ getGroupLevelParents g


-- | To get the Ngrams of a PhyloGroup
getGroupNgrams :: PhyloGroup -> [Int]
getGroupNgrams =  _phylo_groupNgrams


-- | To get the list of pairs (Childs & Parents) of a PhyloGroup
getGroupPairs :: PhyloGroup -> Phylo -> [PhyloGroup]
getGroupPairs g p = (getGroupChilds g p) ++ (getGroupParents g p)


-- | To get the PhyloGroups Parents of a PhyloGroup
getGroupParents :: PhyloGroup -> Phylo -> [PhyloGroup]
getGroupParents g p = getGroupsFromIds (getGroupPeriodParentsId g) p


-- | To get the period out of the id of a PhyloGroup
getGroupPeriod :: PhyloGroup -> (Date,Date)
getGroupPeriod = fst . fst . getGroupId


-- | To get the period child pointers of a PhyloGroup
getGroupPeriodChilds :: PhyloGroup -> [Pointer]
getGroupPeriodChilds = _phylo_groupPeriodChilds


-- | To get the PhyloGroups Period Parents Ids of a PhyloGroup
getGroupPeriodChildsId :: PhyloGroup -> [PhyloGroupId]
getGroupPeriodChildsId g = map fst $ getGroupPeriodChilds g


-- | To get the period parent pointers of a PhyloGroup
getGroupPeriodParents :: PhyloGroup -> [Pointer]
getGroupPeriodParents = _phylo_groupPeriodParents


-- | To get the PhyloGroups Period Parents Ids of a PhyloGroup
getGroupPeriodParentsId :: PhyloGroup -> [PhyloGroupId]
getGroupPeriodParentsId g = map fst $ getGroupPeriodParents g


-- | To get all the PhyloGroup of a Phylo
getGroups :: Phylo -> [PhyloGroup]
getGroups = view ( phylo_periods
                 .  traverse
                 . phylo_periodLevels
                 .  traverse 
                 . phylo_levelGroups
                 )


-- | To get all PhyloGroups matching a list of PhyloGroupIds in a Phylo
getGroupsFromIds :: [PhyloGroupId] -> Phylo -> [PhyloGroup]
getGroupsFromIds ids p = filter (\g -> elem (getGroupId g) ids) $ getGroups p


-- | To get the corresponding list of PhyloGroups from a list of PhyloNodes
getGroupsFromNodes :: [PhyloNode] -> Phylo -> [PhyloGroup]
getGroupsFromNodes ns p = getGroupsFromIds (map getNodeId ns) p 


-- | To get all the PhyloGroup of a Phylo with a given level and period
getGroupsWithFilters :: Int -> (Date,Date) -> Phylo -> [PhyloGroup]
getGroupsWithFilters lvl prd p = (getGroupsWithLevel  lvl p)
                                 `intersect`
                                 (getGroupsWithPeriod prd p)


-- | To get all the PhyloGroup of a Phylo with a given Level
getGroupsWithLevel :: Int -> Phylo -> [PhyloGroup]
getGroupsWithLevel lvl p = filterGroups getGroupLevel lvl (getGroups p)


-- | To get all the PhyloGroup of a Phylo with a given Period
getGroupsWithPeriod :: (Date,Date) -> Phylo -> [PhyloGroup]
getGroupsWithPeriod prd p = filterGroups getGroupPeriod prd (getGroups p)


-- | To create a PhyloGroup in a Phylo out of a list of Ngrams and a set of parameters 
initGroup :: [Ngrams] -> Text -> Int -> Int -> Int -> Int -> Phylo -> PhyloGroup
initGroup ngrams lbl idx lvl from to p = PhyloGroup 
  (((from, to), lvl), idx)
  lbl
  (sort $ map (\x -> getIdxInFoundations x p) ngrams)
  (Map.empty)
  (Map.empty)
  Nothing
  [] [] [] []


---------------------
-- | PhyloPeriod | --
---------------------


-- | To alter each PhyloPeriod of a Phylo following a given function
alterPhyloPeriods :: (PhyloPeriod -> PhyloPeriod) -> Phylo -> Phylo
alterPhyloPeriods f p = over ( phylo_periods
                             .  traverse) f p


-- | To append a list of PhyloPeriod to a Phylo
appendToPhyloPeriods :: [PhyloPeriod] -> Phylo -> Phylo
appendToPhyloPeriods l p = over (phylo_periods) (++ l) p


-- | To get all the PhyloPeriodIds of a Phylo
getPhyloPeriods :: Phylo -> [PhyloPeriodId]
getPhyloPeriods p = map _phylo_periodId 
                  $ view (phylo_periods) p


-- | To get the id of a given PhyloPeriod
getPhyloPeriodId :: PhyloPeriod -> PhyloPeriodId
getPhyloPeriodId prd = _phylo_periodId prd 


-- | To create a PhyloPeriod
initPhyloPeriod :: PhyloPeriodId -> [PhyloLevel] -> PhyloPeriod
initPhyloPeriod id l = PhyloPeriod id l


--------------------
-- | PhyloLevel | --
--------------------


-- | To alter a list of PhyloLevels following a given function
alterPhyloLevels :: ([PhyloLevel] -> [PhyloLevel]) -> Phylo -> Phylo
alterPhyloLevels f p = over ( phylo_periods
                            .  traverse
                            . phylo_periodLevels) f p


-- | To get the PhylolevelId of a given PhyloLevel
getPhyloLevelId :: PhyloLevel -> PhyloLevelId
getPhyloLevelId = _phylo_levelId


-- | To get all the Phylolevels of a given PhyloPeriod
getPhyloLevels :: PhyloPeriod -> [PhyloLevel]
getPhyloLevels = view (phylo_periodLevels)


-- | To create a PhyloLevel
initPhyloLevel :: PhyloLevelId -> [PhyloGroup] -> PhyloLevel
initPhyloLevel id groups = PhyloLevel id groups


-- | To set the LevelId of a PhyloLevel and of all its PhyloGroups
setPhyloLevelId :: Int -> PhyloLevel -> PhyloLevel
setPhyloLevelId lvl' (PhyloLevel (id, lvl) groups)
    = PhyloLevel (id, lvl') groups'
        where 
            groups' = over (traverse . phylo_groupId) (\((period, lvl), idx) -> ((period, lvl'), idx)) groups 


----------------------------
-- | PhyloNodes & Edges | --
----------------------------


-- | To filter some GroupEdges with a given threshold
filterGroupEdges :: Double -> GroupEdges -> GroupEdges
filterGroupEdges thr edges = filter (\((s,t),w) -> w > thr) edges 


-- | To get the neighbours (directed/undirected) of a PhyloGroup from a list of GroupEdges 
getNeighbours :: Bool -> PhyloGroup -> GroupEdges -> [PhyloGroup]
getNeighbours directed g e = case directed of 
  True  -> map (\((s,t),w) -> t) 
             $ filter (\((s,t),w) -> s == g) e 
  False -> map (\((s,t),w) -> head $ delete g $ nub [s,t,g]) 
             $ filter (\((s,t),w) -> s == g || t == g) e


-- | To get the PhyloBranchId of PhyloNode if it exists
getNodeBranchId :: PhyloNode -> PhyloBranchId
getNodeBranchId n = case n ^. phylo_nodeBranchId of
                     Nothing -> panic "[ERR][Viz.Phylo.Tools.getNodeBranchId] branchId not found"
                     Just i  -> i 


-- | To get the PhyloGroupId of a PhyloNode
getNodeId :: PhyloNode -> PhyloGroupId
getNodeId n = n ^. phylo_nodeId


-- | To get the Level of a PhyloNode
getNodeLevel :: PhyloNode -> Level
getNodeLevel n = (snd . fst) $ getNodeId n


-- | To get the Parent Node of a PhyloNode in a PhyloView
getNodeParent :: PhyloNode -> PhyloView -> PhyloNode
getNodeParent n v = head 
                  $ filter (\n' -> getNodeId n' == getNodeParentId n)
                  $ v ^. phylo_viewNodes


-- | To get the Parent Node id of a PhyloNode if it exists
getNodeParentId :: PhyloNode -> PhyloGroupId
getNodeParentId n = case n ^. phylo_nodeParent of
                    Nothing -> panic "[ERR][Viz.Phylo.Tools.getNodeParentId] node parent not found"
                    Just id -> id


-- | To get a list of PhyloNodes grouped by PhyloBranch in a PhyloView
getNodesByBranches :: PhyloView -> [(PhyloBranchId,[PhyloNode])]
getNodesByBranches v = zip bIds $ map (\id -> filter (\n -> (getNodeBranchId n) == id) 
                                            $ getNodesInBranches v ) bIds
  where
    -------------------------------------- 
    bIds :: [PhyloBranchId] 
    bIds = getViewBranchIds v 
    --------------------------------------


-- | To get a list of PhyloNodes owned by any PhyloBranches in a PhyloView
getNodesInBranches :: PhyloView -> [PhyloNode]
getNodesInBranches v = filter (\n -> isJust $ n ^. phylo_nodeBranchId)
                     $ v ^. phylo_viewNodes


-- | To get the PhyloGroupId of the Source of a PhyloEdge 
getSourceId :: PhyloEdge -> PhyloGroupId
getSourceId e = e ^. phylo_edgeSource 


-- | To get the PhyloGroupId of the Target of a PhyloEdge
getTargetId :: PhyloEdge -> PhyloGroupId
getTargetId e = e ^. phylo_edgeTarget                     


---------------------
-- | PhyloBranch | --
---------------------


-- | To get the PhyloBranchId of a PhyloBranch
getBranchId :: PhyloBranch -> PhyloBranchId
getBranchId b = b ^. phylo_branchId


-- | To get a list of PhyloBranchIds given a Level in a Phylo
getBranchIdsWith :: Level -> Phylo -> [PhyloBranchId]
getBranchIdsWith lvl p = sortOn snd
                       $ mapMaybe getGroupBranchId
                       $ getGroupsWithLevel lvl p


-- | To get the Meta value of a PhyloBranch 
getBranchMeta :: Text -> PhyloBranch -> Double 
getBranchMeta k b = (b ^. phylo_branchMeta) ! k


-- | To get all the PhyloBranchIds of a PhyloView
getViewBranchIds :: PhyloView -> [PhyloBranchId]
getViewBranchIds v = map getBranchId $ v ^. phylo_viewBranches


--------------------------------
-- | PhyloQuery & QueryView | --
--------------------------------


-- | To get the first clustering method to apply to get the level 1 of a Phylo
getFstCluster :: PhyloQuery -> Cluster
getFstCluster q = q ^. q_cluster


-- | To get the cluster methods to apply to the Nths levels of a Phylo
getNthCluster :: PhyloQuery -> Cluster
getNthCluster q = q ^. q_nthCluster


-- | To get the Sup Level of a reconstruction of a Phylo from a PhyloQuery
getNthLevel :: PhyloQuery -> Level
getNthLevel q = q ^. q_nthLevel


-- | To get the Grain of the PhyloPeriods from a PhyloQuery
getPeriodGrain :: PhyloQuery -> Int
getPeriodGrain q = q ^. q_periodGrain 


-- | To get the intertemporal matching strategy to apply to a Phylo from a PhyloQuery
getInterTemporalMatching :: PhyloQuery -> Proximity
getInterTemporalMatching q = q ^. q_interTemporalMatching


-- | To get the Steps of the PhyloPeriods from a PhyloQuery
getPeriodSteps :: PhyloQuery -> Int 
getPeriodSteps q = q ^. q_periodSteps


--------------------------------------------------
-- | PhyloQuery & PhyloQueryView Constructors | --
--------------------------------------------------


-- | To get the Proximity associated to a given Clustering method
getProximity :: Cluster -> Proximity
getProximity cluster = case cluster of 
  Louvain (LouvainParams proxi)      -> proxi
  RelatedComponents (RCParams proxi) -> proxi
  _   -> panic "[ERR][Viz.Phylo.Tools.getProximity] this cluster has no associated Proximity"


-- | To initialize all the Cluster / Proximity with their default parameters
initFis :: Maybe Bool -> Maybe Bool -> Maybe Support -> FisParams
initFis (def True -> flt) (def True -> kmf) (def 1 -> min) = FisParams flt kmf min

initHamming :: Maybe Double -> HammingParams
initHamming (def 0.01 -> sens) = HammingParams sens

initLonelyBranch :: Maybe Int -> Maybe Int -> Maybe Int -> LBParams
initLonelyBranch (def 2 -> periodsInf) (def 2 -> periodsSup) (def 1 -> minNodes) = LBParams periodsInf periodsSup minNodes

initLouvain :: Maybe Proximity -> LouvainParams
initLouvain (def defaultWeightedLogJaccard -> proxi) = LouvainParams proxi

initRelatedComponents :: Maybe Proximity -> RCParams
initRelatedComponents (def Filiation -> proxi) = RCParams proxi

initWeightedLogJaccard :: Maybe Double -> Maybe Double -> WLJParams
initWeightedLogJaccard (def 0 -> thr) (def 0.01 -> sens) = WLJParams thr sens


-- | To initialize a PhyloQuery from given and default parameters
initPhyloQuery :: Text -> Text -> Maybe Int -> Maybe Int -> Maybe Cluster -> Maybe Proximity -> Maybe Level -> Maybe Cluster -> PhyloQuery
initPhyloQuery name desc (def 5 -> grain) (def 3 -> steps) (def defaultFis -> cluster)
  (def defaultWeightedLogJaccard -> matching) (def 2 -> nthLevel) (def defaultRelatedComponents -> nthCluster) =
    PhyloQuery name desc grain steps cluster matching nthLevel nthCluster


-- | To initialize a PhyloQueryView default parameters
initPhyloQueryView :: Maybe Level -> Maybe Filiation -> Maybe Bool -> Maybe Level -> Maybe [Metric] -> Maybe [Filter] -> Maybe [Tagger] -> Maybe (Sort, Order) -> Maybe DisplayMode -> Maybe Bool -> PhyloQueryView
initPhyloQueryView (def 2 -> lvl) (def Descendant -> f) (def False -> c) (def 1 -> d) (def [] -> ms) (def [] -> fs) (def [] -> ts) s (def Flat -> dm) (def True -> v) =
  PhyloQueryView lvl f c d ms fs ts s dm v


-- | To define some obvious boolean getters
shouldFilterFis :: FisParams -> Bool
shouldFilterFis = _fis_filtered

shouldKeepMinorFis :: FisParams -> Bool
shouldKeepMinorFis = _fis_keepMinorFis

----------------------------
-- | Default ressources | --
----------------------------

-- Clusters

defaultFis :: Cluster
defaultFis = Fis (initFis Nothing Nothing Nothing)

defaultLouvain :: Cluster
defaultLouvain = Louvain (initLouvain Nothing)

defaultRelatedComponents :: Cluster
defaultRelatedComponents = RelatedComponents (initRelatedComponents Nothing)

-- Filters

defaultLonelyBranch :: Filter
defaultLonelyBranch = LonelyBranch (initLonelyBranch Nothing Nothing Nothing)

-- Params 

defaultPhyloParam :: PhyloParam
defaultPhyloParam = initPhyloParam Nothing Nothing Nothing

-- Proximities

defaultHamming :: Proximity
defaultHamming = Hamming (initHamming Nothing)

defaultWeightedLogJaccard :: Proximity
defaultWeightedLogJaccard = WeightedLogJaccard (initWeightedLogJaccard Nothing Nothing)

-- Queries

defaultQuery :: PhyloQuery
defaultQuery = initPhyloQuery "Cesar et Cleôpatre" "An example of Phylomemy (french without accent)" 
                              Nothing Nothing Nothing Nothing Nothing Nothing

defaultQueryView :: PhyloQueryView
defaultQueryView = initPhyloQueryView Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing                              

-- Software

defaultSoftware :: Software
defaultSoftware = Software "Gargantext" "v4"

-- Version 

defaultPhyloVersion :: Text
defaultPhyloVersion = "v1"




