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

module Gargantext.Viz.Phylo.Tools
  where

import Control.Lens         hiding (both, Level, Empty)
import Data.List            (filter, intersect, (++), sort, null, tail, last, tails, delete, nub, sortOn, nubBy, concat)
import Data.Maybe           (mapMaybe,fromMaybe)
import Data.Map             (Map, mapKeys, member, (!), restrictKeys, elems, empty, filterWithKey, unionWith)
import Data.Set             (Set)
import Data.Text            (Text,toLower,unwords)
import Data.Tuple.Extra
import Data.Vector          (Vector,elemIndex)
import Gargantext.Prelude
import Gargantext.Viz.Phylo
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
  | elem (head' "doesContains" l') l      = doesContains l (tail l')
  | otherwise             = False


-- | Does a list of ordered A contains an other list of ordered A
doesContainsOrd :: Eq a => Ord a => [a] -> [a] -> Bool
doesContainsOrd l l'
  | null l'          = False
  | last l < (head' "doesContainsOrd" l') = False
  | (head' "doesContainsOrd" l') `elem` l = True
  | otherwise        = doesContainsOrd l (tail l')


-- | To filter nested Sets of a
filterNestedSets :: Eq a => Set a -> [Set a] -> [Set a] -> [Set a]
filterNestedSets h l l'
  | null l                 = if doesAnySetContains h l l'
                             then l'
                             else h : l'
  | doesAnySetContains h l l' = filterNestedSets (head' "filterNestedSets1" l) (tail l) l'
  | otherwise              = filterNestedSets (head' "filterNestedSets2" l) (tail l) (h : l')



-- | To get the good pair of keys (x,y) or (y,x) in a given Map (a,b) c
getKeyPair :: (Int,Int) -> Map (Int,Int) a -> (Int,Int)
getKeyPair (x,y) m = case findPair (x,y) m of
                      Nothing -> panic "[ERR][Viz.Phylo.Example.getKeyPair] Nothing"
                      Just i  -> i
                     where
                      --------------------------------------
                      findPair :: (Int,Int) -> Map (Int,Int) a -> Maybe (Int,Int)
                      findPair (x',y') m'
                        | member (x',y') m' = Just (x',y')
                        | member (y',x') m' = Just (y',x')
                        | otherwise      = Nothing
                      --------------------------------------


-- | To filter Fis with small Support but by keeping non empty Periods
keepFilled :: (Int -> [a] -> [a]) -> Int -> [a] -> [a]
keepFilled f thr l = if (null $ f thr l) && (not $ null l)
                     then keepFilled f (thr - 1) l
                     else f thr l


-- | To get all combinations of a list
listToFullCombi :: Eq a => [a] -> [(a,a)]
listToFullCombi l = [(x,y) | x <- l, y <- l]


-- | To get all combinations of a list
listToDirectedCombi :: Eq a => [a] -> [(a,a)]
listToDirectedCombi l = [(x,y) | x <- l, y <- l, x /= y]


listToEqualCombi :: Eq a => [a] -> [(a,a)]
listToEqualCombi l = [(x,y) | x <- l, y <- l, x == y]

listToPairs :: Eq a => [a] -> [(a,a)]
listToPairs l = (listToEqualCombi l) ++ (listToUnDirectedCombi l)


-- | To get all combinations of a list and apply a function to the resulting list of pairs
listToDirectedCombiWith :: Eq a => forall b. (a -> b) -> [a] -> [(b,b)]
listToDirectedCombiWith f l = [(f x,f y) | x <- l, y <- l, x /= y]


-- | To get the sequential combinations of an order list
listToSequentialCombi :: Eq a => [a] -> [(a,a)]
listToSequentialCombi l = nubBy (\x y -> fst x == fst y) $ listToUnDirectedCombi l


-- | To get all combinations of a list with no repetition
listToUnDirectedCombi :: [a] -> [(a,a)]
listToUnDirectedCombi l = [ (x,y) | (x:rest) <- tails l,  y <- rest ]


-- | To get all combinations of a list with no repetition and apply a function to the resulting list of pairs
listToUnDirectedCombiWith :: forall a b. (a -> b) -> [a] -> [(b,b)]
listToUnDirectedCombiWith f l = [ (f x, f y) | (x:rest) <- tails l,  y <- rest ]


-- | To transform a list of Ngrams Indexes into a Label
ngramsToLabel :: Vector Ngrams -> [Int] -> Text
ngramsToLabel ngrams l = unwords $ tail' "ngramsToLabel" $ concat $ map (\n -> ["|",n]) $ ngramsToText ngrams l


-- | To transform a list of Ngrams Indexes into a list of Text
ngramsToText :: Vector Ngrams -> [Int] -> [Text]
ngramsToText ngrams l = map (\idx -> ngrams Vector.! idx) l


-- | To transform a list of ngrams into a list of indexes
ngramsToIdx :: [Ngrams] -> Vector Ngrams -> [Int]
ngramsToIdx ns v = sort $ map (\n -> getIdxInVector n v) ns


-- | To unify the keys (x,y) that Map 1 share with Map 2 such as: (x,y) <=> (y,x)
unifySharedKeys :: Eq a => Ord a => Map (a,a) b -> Map (a,a) b -> Map (a,a) b
unifySharedKeys m1 m2 = mapKeys (\(x,y) -> if member (y,x) m2
                                           then (y,x)
                                           else (x,y) ) m1


---------------
-- | Phylo | --
---------------

-- | An analyzer ingests a Ngrams and generates a modified version of it
phyloAnalyzer :: Ngrams -> Ngrams
phyloAnalyzer n = toLower n

-- | To init the foundation roots of the Phylo as a Vector of Ngrams
initFoundationsRoots :: [Ngrams] -> Vector Ngrams
initFoundationsRoots l = Vector.fromList $ map phyloAnalyzer l

-- | To init the base of a Phylo from a List of Periods and Foundations
initPhyloBase :: [(Date, Date)] -> PhyloFoundations -> Map Date Double  -> Map Date (Map (Int,Int) Double) -> Map (Date,Date) [PhyloFis] -> PhyloParam -> Phylo
initPhyloBase pds fds nbDocs cooc fis prm = Phylo ((fst . (head' "initPhyloBase")) pds, (snd . last) pds) fds (map (\pd -> initPhyloPeriod pd []) pds) nbDocs cooc fis prm

-- | To init the param of a Phylo
initPhyloParam :: Maybe Text -> Maybe Software -> Maybe PhyloQueryBuild -> PhyloParam
initPhyloParam (def defaultPhyloVersion -> v) (def defaultSoftware -> s) (def defaultQueryBuild -> q) = PhyloParam v s q

-- | To get the last computed Level in a Phylo
getLastLevel :: Phylo -> Level
getLastLevel p = (last . sort)
               $ map (snd . getPhyloLevelId)
               $ view ( phylo_periods
                      .  traverse
                      . phylo_periodLevels ) p

-- | To get all the coocurency matrix of a phylo
getPhyloCooc :: Phylo -> Map Date (Map (Int,Int) Double)
getPhyloCooc p = p ^. phylo_cooc


-- | To get the PhyloParam of a Phylo
getPhyloParams :: Phylo -> PhyloParam
getPhyloParams = _phylo_param

-- | To get the title of a Phylo
getPhyloTitle :: Phylo -> Text
getPhyloTitle p = _q_phyloTitle $ _phyloParam_query $ getPhyloParams p

-- | To get the desc of a Phylo
getPhyloDescription :: Phylo -> Text
getPhyloDescription p = _q_phyloTitle $ _phyloParam_query $ getPhyloParams p

getPhyloMatchingFrame :: Phylo -> Int
getPhyloMatchingFrame p = _q_interTemporalMatchingFrame $ _phyloParam_query $ getPhyloParams p

getPhyloMatchingFrameTh :: Phylo -> Double
getPhyloMatchingFrameTh p = _q_interTemporalMatchingFrameTh $ _phyloParam_query $ getPhyloParams p

getPhyloProximity :: Phylo -> Proximity
getPhyloProximity p = _q_interTemporalMatching $ _phyloParam_query $ getPhyloParams p

getPhyloReBranchThr :: Phylo -> Double
getPhyloReBranchThr p = _q_reBranchThr $ _phyloParam_query $ getPhyloParams p

getPhyloReBranchNth :: Phylo -> Int
getPhyloReBranchNth p = _q_reBranchNth $ _phyloParam_query $ getPhyloParams p

getPhyloFis :: Phylo -> Map (Date,Date) [PhyloFis]
getPhyloFis = _phylo_fis


--------------------
-- | PhyloRoots | --
--------------------

-- | To get the foundations of a Phylo
getFoundations :: Phylo -> PhyloFoundations
getFoundations = _phylo_foundations

-- | To get the foundations roots of a Phylo
getFoundationsRoots :: Phylo -> Vector Ngrams
getFoundationsRoots p = (getFoundations p) ^. phylo_foundationsRoots

-- | To get the Index of a Ngrams in the foundationsRoots of a Phylo
getIdxInRoots :: Ngrams -> Phylo -> Int
getIdxInRoots n p = case (elemIndex n (getFoundationsRoots p)) of
    Nothing  -> panic $ "[ERR][Viz.Phylo.Tools.getIdxInRoots] Ngrams not in foundationsRoots: " <> cs n
    Just idx -> idx

getIdxInRoots' :: Ngrams -> Vector Ngrams -> Int
getIdxInRoots' n root = case (elemIndex n root) of
    Nothing  -> panic $ "[ERR][Viz.Phylo.Tools.getIdxInRoots] Ngrams not in foundationsRoots: " <> cs n
    Just idx -> idx    

getIdxInVector :: Ngrams -> Vector Ngrams -> Int
getIdxInVector n ns = case (elemIndex n ns) of
  Nothing  -> panic $ "[ERR][Viz.Phylo.Tools.getIdxInVector] Ngrams not in foundationsRoots: " <> cs n
  Just idx -> idx

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


-- | To get the PhyloGroups Level Parents Ids of a PhyloGroup
getGroupLevelParentId :: PhyloGroup -> PhyloGroupId
getGroupLevelParentId g = (head' "getGroupLevelParentId") $ getGroupLevelParentsId g

-- | To get the Meta value of a PhyloGroup
getGroupMeta :: Text -> PhyloGroup -> Double
getGroupMeta k g = (g ^. phylo_groupMeta) ! k


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


-- | To get the pointers of a given Phylogroup
getGroupPointers :: EdgeType -> Filiation -> PhyloGroup -> [Pointer]
getGroupPointers t f g = case t of
                          PeriodEdge -> case f of 
                                          Ascendant  -> getGroupPeriodParents g
                                          Descendant -> getGroupPeriodChilds g
                                          _          -> panic "[ERR][Viz.Phylo.Tools.getGroupPointers] wrong filiation"
                          LevelEdge  -> case f of 
                                          Ascendant  -> getGroupLevelParents g
                                          Descendant -> getGroupLevelChilds g
                                          _          -> panic "[ERR][Viz.Phylo.Tools.getGroupPointers] wrong filiation"


-- | To get the roots labels of a list of group ngrams
getGroupText :: PhyloGroup -> Phylo -> [Text] 
getGroupText g p = ngramsToText (getFoundationsRoots p) (getGroupNgrams g) 


-- | To get all the PhyloGroup of a Phylo
getGroups :: Phylo -> [PhyloGroup]
getGroups = view ( phylo_periods
                 .  traverse
                 . phylo_periodLevels
                 .  traverse
                 . phylo_levelGroups
                 )


-- | To get all PhyloGroups matching a list of PhyloGoupIds in a Phylo
-- getGroupsFromIds :: [PhyloGroupId] -> Phylo -> [PhyloGroup]
-- getGroupsFromIds ids p = filter (\g -> elem (getGroupId g) ids) $ getGroups p

getGroupFromId :: PhyloGroupId -> Phylo -> PhyloGroup
getGroupFromId id p = 
  let groups = Map.fromList $ map (\g -> (getGroupId g, g)) $ getGroups p
  in  groups ! id 

getGroupsFromIds :: [PhyloGroupId] -> Phylo -> [PhyloGroup]
getGroupsFromIds ids p =
  let groups = Map.fromList $ map (\g -> (getGroupId g, g)) $ getGroups p
  in  elems $ restrictKeys groups (Set.fromList ids)


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
initGroup ngrams lbl idx lvl from' to' p = PhyloGroup
  (((from', to'), lvl), idx)
  lbl
  idxs
  (Map.empty)
  (Map.empty)
  Nothing
  (getMiniCooc (listToFullCombi idxs) (periodsToYears [(from', to')]) (getPhyloCooc p))
  [] [] [] []
  where 
    idxs = sort $ map (\x -> getIdxInRoots x p) ngrams


-- | To sum two coocurency Matrix
sumCooc :: Map (Int, Int) Double ->  Map (Int, Int) Double ->  Map (Int, Int) Double
sumCooc m m' = unionWith (+) m m'

-- | To build the mini cooc matrix of each group
getMiniCooc :: [(Int,Int)] -> Set Date -> Map Date (Map (Int,Int) Double) -> Map (Int,Int) Double
getMiniCooc pairs years cooc = filterWithKey (\(n,n') _ -> elem (n,n') pairs) cooc'
  where 
    --------------------------------------
    cooc' :: Map (Int,Int) Double
    cooc' = foldl (\m m' -> sumCooc m m') empty 
          $ elems 
          $ restrictKeys cooc years
    --------------------------------------


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


-- | To transform a list of periods into a set of Dates
periodsToYears :: [(Date,Date)] -> Set Date
periodsToYears periods = (Set.fromList . sort . concat)
                       $ map (\(d,d') -> [d..d']) periods


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
setPhyloLevelId lvl' (PhyloLevel (id, _lvl) groups)
    = PhyloLevel (id, lvl') groups'
        where
            groups' = over (traverse . phylo_groupId)
                           (\((period, _lvl), idx) -> ((period, lvl'), idx))
                           groups


------------------
-- | PhyloFis | --
------------------


-- | To get the clique of a PhyloFis
getClique :: PhyloFis -> Clique
getClique = _phyloFis_clique

-- | To get the support of a PhyloFis
getSupport :: PhyloFis -> Support
getSupport = _phyloFis_support

-- | To get the period of a PhyloFis
getFisPeriod :: PhyloFis -> (Date,Date)
getFisPeriod = _phyloFis_period


----------------------------
-- | PhyloNodes & Edges | --
----------------------------


-- | To alter a PhyloNode
alterPhyloNode :: (PhyloNode -> PhyloNode) -> PhyloView -> PhyloView
alterPhyloNode f v = over ( pv_nodes
                          .  traverse
                          ) (\pn ->  f pn ) v


-- | To filter some GroupEdges with a given threshold
filterGroupEdges :: Double -> [GroupEdge] -> [GroupEdge]
filterGroupEdges thr edges = filter (\((_s,_t),w) -> w > thr) edges


-- | To get the neighbours (directed/undirected) of a PhyloGroup from a list of GroupEdges
getNeighbours :: Bool -> PhyloGroup -> [GroupEdge] -> [PhyloGroup]
getNeighbours directed g e = case directed of
  True  -> map (\((_s,t),_w) -> t)
             $ filter (\((s,_t),_w) -> s == g) e
  False -> map (\((s,t),_w) -> (head' "getNeighbours") $ delete g $ nub [s,t,g])
             $ filter (\((s,t),_w) -> s == g || t == g) e


-- | To get the PhyloBranchId of PhyloNode if it exists
getNodeBranchId :: PhyloNode -> PhyloBranchId
getNodeBranchId n = case n ^. pn_bid of
                     Nothing -> panic "[ERR][Viz.Phylo.Tools.getNodeBranchId] branchId not found"
                     Just i  -> i


-- | To get the PhyloGroupId of a PhyloNode
getNodeId :: PhyloNode -> PhyloGroupId
getNodeId n = n ^. pn_id


getNodePeriod :: PhyloNode -> (Date,Date)
getNodePeriod n = fst $ fst $ getNodeId n


-- | To get the Level of a PhyloNode
getNodeLevel :: PhyloNode -> Level
getNodeLevel n = (snd . fst) $ getNodeId n


-- | To get the Parent Node of a PhyloNode in a PhyloView
getNodeParent :: PhyloNode -> PhyloView -> [PhyloNode]
getNodeParent n v = filter (\n' -> elem (getNodeId n') (getNodeParentsId n))
                  $ v ^. pv_nodes


-- | To get the Parent Node id of a PhyloNode if it exists
getNodeParentsId :: PhyloNode -> [PhyloGroupId]
getNodeParentsId n = case n ^. pn_parents of
                    Nothing  -> panic "[ERR][Viz.Phylo.Tools.getNodeParentsId] node parent not found"
                    Just ids -> ids


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
getNodesInBranches v = filter (\n -> isJust $ n ^. pn_bid)
                     $ v ^. pv_nodes


-- | To get the PhyloGroupId of the Source of a PhyloEdge
getSourceId :: PhyloEdge -> PhyloGroupId
getSourceId e = e ^. pe_source


-- | To get the PhyloGroupId of the Target of a PhyloEdge
getTargetId :: PhyloEdge -> PhyloGroupId
getTargetId e = e ^. pe_target


---------------------
-- | PhyloBranch | --
---------------------


-- | To get the PhyloBranchId of a PhyloBranch
getBranchId :: PhyloBranch -> PhyloBranchId
getBranchId b = b ^. pb_id

-- | To get a list of PhyloBranchIds
getBranchIds :: Phylo -> [PhyloBranchId]
getBranchIds p = sortOn snd
               $ nub 
               $ mapMaybe getGroupBranchId
               $ getGroups p


-- | To get a list of PhyloBranchIds given a Level in a Phylo
getBranchIdsWith :: Level -> Phylo -> [PhyloBranchId]
getBranchIdsWith lvl p = sortOn snd
                       $ mapMaybe getGroupBranchId
                       $ getGroupsWithLevel lvl p


-- | To get the Meta value of a PhyloBranch
getBranchMeta :: Text -> PhyloBranch -> [Double]
getBranchMeta k b = (b ^. pb_metrics) ! k


-- | To get all the PhyloBranchIds of a PhyloView
getViewBranchIds :: PhyloView -> [PhyloBranchId]
getViewBranchIds v = map getBranchId $ v ^. pv_branches


-- | To get a list of PhyloGroup sharing the same PhyloBranchId
getGroupsByBranches :: Phylo -> [(PhyloBranchId,[PhyloGroup])]
getGroupsByBranches p = zip (getBranchIds p) 
                      $ map (\id -> filter (\g -> (fromJust $ getGroupBranchId g) == id)
                                    $ getGroupsInBranches p) 
                      $ getBranchIds p 


-- | To get the sublist of all the PhyloGroups linked to a branch
getGroupsInBranches :: Phylo -> [PhyloGroup]
getGroupsInBranches p = filter (\g -> isJust $ g ^. phylo_groupBranchId)
                      $ getGroups p


--------------------------------
-- | PhyloQuery & QueryView | --
--------------------------------


-- | To filter PhyloView's Branches by level
filterBranchesByLevel :: Level -> PhyloView -> [PhyloBranch]
filterBranchesByLevel lvl pv = filter (\pb -> lvl == (fst $ pb ^. pb_id)) 
                          $ pv ^. pv_branches


-- | To filter PhyloView's Edges by level
filterEdgesByLevel :: Level -> [PhyloEdge] -> [PhyloEdge]
filterEdgesByLevel lvl pes = filter (\pe -> (lvl == ((snd . fst) $ pe ^. pe_source))
                                         && (lvl == ((snd . fst) $ pe ^. pe_target))) pes


-- | To filter PhyloView's Edges by type
filterEdgesByType :: EdgeType -> [PhyloEdge] -> [PhyloEdge]
filterEdgesByType t pes = filter (\pe -> t == (pe ^. pe_type)) pes


-- | To filter PhyloView's Nodes by the oldest Period
filterNodesByFirstPeriod :: [PhyloNode] -> [PhyloNode]
filterNodesByFirstPeriod pns = filter (\pn -> fstPrd == ((fst . fst) $ pn ^. pn_id)) pns
    where 
        --------------------------------------
        fstPrd :: (Date,Date)
        fstPrd = (head' "filterNodesByFirstPeriod")
               $ sortOn fst 
               $ map (\pn -> (fst . fst) $ pn ^. pn_id) pns 
        --------------------------------------


-- | To filter PhyloView's Nodes by Branch
filterNodesByBranch :: PhyloBranchId -> [PhyloNode] -> [PhyloNode]
filterNodesByBranch bId pns = filter (\pn -> if isJust $ pn ^. pn_bid
                                             then if bId == (fromJust $ pn ^. pn_bid)
                                                  then True
                                                  else False
                                             else False ) pns           


-- | To filter PhyloView's Nodes by level
filterNodesByLevel :: Level -> [PhyloNode] -> [PhyloNode]
filterNodesByLevel lvl pns = filter (\pn -> lvl == ((snd . fst) $ pn ^. pn_id)) pns


-- | To filter PhyloView's Nodes by Period
filterNodesByPeriod :: PhyloPeriodId -> [PhyloNode] -> [PhyloNode]
filterNodesByPeriod prd pns = filter (\pn -> prd == ((fst . fst) $ pn ^. pn_id)) pns


-- | To get the first clustering method to apply to get the contextual units of a Phylo
getContextualUnit :: PhyloQueryBuild -> Cluster
getContextualUnit q = q ^. q_contextualUnit


-- | To get the metrics to apply to contextual units
getContextualUnitMetrics :: PhyloQueryBuild -> [Metric]
getContextualUnitMetrics q = q ^. q_contextualUnitMetrics


-- | To get the filters to apply to contextual units
getContextualUnitFilters :: PhyloQueryBuild -> [Filter]
getContextualUnitFilters q = q ^. q_contextualUnitFilters


-- | To get the cluster methods to apply to the Nths levels of a Phylo
getNthCluster :: PhyloQueryBuild -> Cluster
getNthCluster q = q ^. q_nthCluster


-- | To get the Sup Level of a reconstruction of a Phylo from a PhyloQuery
getNthLevel :: PhyloQueryBuild -> Level
getNthLevel q = q ^. q_nthLevel


-- | To get the Grain of the PhyloPeriods from a PhyloQuery
getPeriodGrain :: PhyloQueryBuild -> Int
getPeriodGrain q = q ^. q_periodGrain


-- | To get the intertemporal matching strategy to apply to a Phylo from a PhyloQuery
getInterTemporalMatching :: PhyloQueryBuild -> Proximity
getInterTemporalMatching q = q ^. q_interTemporalMatching


-- | To get the Steps of the PhyloPeriods from a PhyloQuery
getPeriodSteps :: PhyloQueryBuild -> Int
getPeriodSteps q = q ^. q_periodSteps


--------------------------------------------------
-- | PhyloQueryBuild & PhyloQueryView Constructors | --
--------------------------------------------------

-- | To get the threshold of a Proximity
getThreshold :: Proximity -> Double
getThreshold prox = case prox of 
  WeightedLogJaccard (WLJParams thr _) -> thr
  Hamming (HammingParams thr)          -> thr
  Filiation                            -> panic "[ERR][Viz.Phylo.Tools.getThreshold] Filiation"


-- | To get the Proximity associated to a given Clustering method
getProximity :: Cluster -> Proximity
getProximity cluster = case cluster of
  Louvain (LouvainParams proxi)      -> proxi
  RelatedComponents (RCParams proxi) -> proxi
  _   -> panic "[ERR][Viz.Phylo.Tools.getProximity] this cluster has no associated Proximity"


-- | To initialize all the Cluster / Proximity with their default parameters
initFis :: Maybe Bool -> Maybe Support -> Maybe Int -> FisParams
initFis (def True -> kmf) (def 0 -> min') (def 0 -> thr) = FisParams kmf min' thr

initHamming :: Maybe Double -> HammingParams
initHamming (def 0.01 -> sens) = HammingParams sens

initLonelyBranch :: Maybe Int -> Maybe Int -> Maybe Int -> LBParams
initLonelyBranch (def 2 -> periodsInf) (def 2 -> periodsSup) (def 1 -> minNodes) = LBParams periodsInf periodsSup minNodes

initSizeBranch :: Maybe Int -> SBParams
initSizeBranch (def 1 -> minSize) = SBParams minSize

initLonelyBranch' :: Maybe Int -> Maybe Int -> Maybe Int -> LBParams
initLonelyBranch' (def 0 -> periodsInf) (def 0 -> periodsSup) (def 1 -> minNodes) = LBParams periodsInf periodsSup minNodes

initLouvain :: Maybe Proximity -> LouvainParams
initLouvain (def defaultWeightedLogJaccard -> proxi) = LouvainParams proxi

initRelatedComponents :: Maybe Proximity -> RCParams
initRelatedComponents (def defaultWeightedLogJaccard -> proxi) = RCParams proxi

-- | TODO user param in main function
initWeightedLogJaccard :: Maybe Double -> Maybe Double -> WLJParams
initWeightedLogJaccard (def 0.3 -> thr) (def 20.0 -> sens) = WLJParams thr sens


-- | To initialize a PhyloQueryBuild from given and default parameters
initPhyloQueryBuild :: Text          -> Text            -> Maybe Int
                    -> Maybe Int     -> Maybe Cluster   -> Maybe [Metric]
                    -> Maybe [Filter]-> Maybe Proximity -> Maybe Int
                    -> Maybe Double  -> Maybe Double    -> Maybe Int
                    -> Maybe Level   -> Maybe Cluster   -> PhyloQueryBuild
initPhyloQueryBuild name desc (def 5 -> grain)
                    (def 1 -> steps)      (def defaultFis -> cluster) (def [] -> metrics)
                    (def [] -> filters)   (def defaultWeightedLogJaccard -> matching') (def 5 -> frame)
                    (def 0.8 -> frameThr) (def 0.5 -> reBranchThr) (def 4 -> reBranchNth)
                    (def 2 -> nthLevel)   (def defaultRelatedComponents -> nthCluster) =
    PhyloQueryBuild name  desc    grain
                    steps cluster metrics filters matching' frame frameThr reBranchThr reBranchNth nthLevel nthCluster


-- | To initialize a PhyloQueryView default parameters
initPhyloQueryView :: Maybe Level -> Maybe Filiation -> Maybe Bool -> Maybe Level -> Maybe [Metric] -> Maybe [Filter] -> Maybe [Tagger] -> Maybe (Sort, Order) -> Maybe ExportMode -> Maybe DisplayMode -> Maybe Bool -> PhyloQueryView
initPhyloQueryView (def 2 -> lvl) (def Descendant -> f) (def False -> c) (def 1 -> d) (def [] -> ms) (def [] -> fs) (def [] -> ts) s (def Json -> em) (def Flat -> dm) (def True -> v) =
  PhyloQueryView lvl f c d ms fs ts s em dm v


-- | To define some obvious boolean getters
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

defaultSizeBranch :: Filter
defaultSizeBranch = SizeBranch (initSizeBranch Nothing)

-- Params

defaultPhyloParam :: PhyloParam
defaultPhyloParam = initPhyloParam Nothing Nothing Nothing

-- Proximities

defaultHamming :: Proximity
defaultHamming = Hamming (initHamming Nothing)

defaultWeightedLogJaccard :: Proximity
defaultWeightedLogJaccard = WeightedLogJaccard (initWeightedLogJaccard Nothing Nothing)

-- Queries
type Title = Text
type Desc  = Text

defaultQueryBuild :: PhyloQueryBuild
defaultQueryBuild = defaultQueryBuild'
  "Cesar et Cleôpatre"
  "An example of Phylomemy (french without accent)"

defaultQueryBuild' :: Title -> Desc -> PhyloQueryBuild
defaultQueryBuild' t d = initPhyloQueryBuild t d
                              Nothing Nothing Nothing
                              Nothing Nothing Nothing
                              Nothing Nothing Nothing
                              Nothing Nothing Nothing

defaultQueryView :: PhyloQueryView
defaultQueryView = initPhyloQueryView
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing

-- Software

defaultSoftware :: Software
defaultSoftware = Software "Gargantext" "v4"

-- Version

defaultPhyloVersion :: Text
defaultPhyloVersion = "v1"

