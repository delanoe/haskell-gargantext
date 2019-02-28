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

import Control.Lens hiding (makeLenses, both, Level)
import Data.List (concat, union, intersect, tails, tail, head, last, null, zip, sort, length, any, (++), nub)
import qualified Data.List as List
import Data.Text (Text, unwords, toLower, words)
import Data.Tuple.Extra
import Data.Semigroup (Semigroup)

import Data.Map (Map, elems, member, adjust, singleton, (!), keys, restrictKeys, mapWithKey)
import qualified Data.Map as Map

import Data.Vector (Vector, fromList, elemIndex)
import qualified Data.Vector as Vector
import qualified Data.Maybe as Maybe

import Data.Tuple (fst, snd)
import qualified Data.Tuple as Tuple

import Data.Bool (Bool, not)
import qualified Data.Bool as Bool

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Matrix as DM'

import Gargantext.Text.Metrics.FrequentItemSet (fisWithSizePolyMap, Size(..))
import Gargantext.Text.Terms.Mono (monoTexts)
import Gargantext.Prelude hiding (head)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Tools

------------------------------------------------------------------------
-- | Types | --


-- | Document : a piece of Text linked to a Date
data Document = Document
      { date :: Date
      , text :: Text
      } deriving (Show)
-- | Corpus : a list of Documents
type Corpus   = [Document]



type PeriodeSize = Int
-- data Periodes b a = Map (b,b) a
type Occurrences = Int

data LinkLvlLabel = Link_m1_0 | Link_0_m1 | Link_1_0 | Link_0_1 | Link_x_y
  deriving (Show, Eq, Enum, Bounded)

data LinkLvl = LinkLvl
  { linkLvlLabel :: LinkLvlLabel
  , linkLvlFrom  :: Int
  , linkLvlTo    :: Int
  } deriving (Show)


data PhyloError = LevelDoesNotExist
                | LevelUnassigned
          deriving (Show)


--------------------------------------------------------------------
phyloExampleFinal :: Phylo
phyloExampleFinal = undefined

--------------------------------------------------------------------
appariement :: Map (Date, Date) (Map (Set Ngrams) Int)
appariement = undefined

------------------------------------------------------------------------
-- | STEP 10 | -- Incrementaly cluster the PhyloGroups n times, link them through the Periods and build level n of the Phylo   


------------------------------------------------------------------------
-- | STEP 8 | -- Cluster the Fis

------------------------------------------------------------------------
-- | STEP 9 | -- Link the PhyloGroups of level 1 through the Periods 

shouldPair :: PhyloGroup -> PhyloGroup -> Bool
shouldPair g g' = (not . null) $ intersect (getGroupNgrams g) (getGroupNgrams g')


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

-- | 
listToCombi :: forall a b. (a -> b) -> [a] -> [(b,b)]
listToCombi f l = [ (f x, f y) | (x:rest) <- tails l
                               ,  y       <- rest
                               ]

fisToCooc :: Map (Date, Date) Fis -> Phylo -> Map (Int, Int) Double
fisToCooc m p = map   (/docs)
              $ foldl (\mem x -> adjust (+1) (getKeyPair x mem) mem) cooc
              $ concat
              $ map (\x -> listToCombi (\x -> ngramsToIdx x p) $ (Set.toList . fst) x) fis
  where
    --------------------------------------
    fis :: [(Clique,Support)]
    fis = concat $ map (\x -> Map.toList x) (elems m)
    --------------------------------------
    fisNgrams :: [Ngrams]
    fisNgrams = foldl (\mem x -> union mem $ (Set.toList . fst) x) [] fis
    --------------------------------------
    docs :: Double
    docs = fromIntegral $ foldl (\mem x -> mem + (snd x)) 0 fis
    --------------------------------------
    cooc :: Map (Int, Int) (Double)
    cooc = Map.fromList $ map (\x -> (x,0)) (listToCombi (\x -> ngramsToIdx x p) fisNgrams)
    --------------------------------------



phyloWithAppariement1 :: Phylo
phyloWithAppariement1 = phyloLinked_0_1

------------------------------------------------------------------------
-- | STEP 8 | -- Find the Fis out of Documents and Ngrams and build level 1 of the Phylo 

phyloLinked_0_1 :: Phylo
phyloLinked_0_1 = phyloToLinks lvl_0_1  phyloLinked_1_0

lvl_0_1 :: LinkLvl
lvl_0_1 = (LinkLvl Link_0_1 0 1)

phyloLinked_1_0 :: Phylo
phyloLinked_1_0 = phyloToLinks lvl_1_0  phyloWithGroups1

lvl_1_0 :: LinkLvl
lvl_1_0 = (LinkLvl Link_1_0 1 0)

phyloWithGroups1 :: Phylo
phyloWithGroups1 = updatePhyloByLevel (initLevel 1 Level_1) phyloLinked_m1_0

cliqueToGroup :: PhyloPeriodId -> Int -> Int -> Ngrams -> (Clique,Support) -> Phylo -> PhyloGroup
cliqueToGroup period lvl idx label fis p = PhyloGroup ((period, lvl), idx)
                                                    label
                                                    (sort $ map (\x -> ngramsToIdx x p)
                                                          $ Set.toList
                                                          $ fst fis
                                                          )
                                                    (singleton "support" (fromIntegral $ snd fis))
                                                    [] [] [] []

fisToPhyloLevel :: Map (Date, Date) Fis -> Phylo -> Phylo
fisToPhyloLevel m p = over (phylo_periods . traverse)
                           (\period ->
                              let periodId = _phylo_periodId period
                                  fisList  = zip [1..] (Map.toList (m ! periodId))
                              in  over (phylo_periodLevels)
                                       (\levels ->
                                          let groups = map (\fis -> cliqueToGroup periodId 1 (fst fis) "" (snd fis) p) fisList
                                          in (PhyloLevel (periodId, 1) groups) : levels
                                       ) period
                           ) p

-- | To preserve nonempty periods from filtering, please use : filterFisBySupport False ...
phyloFisFiltered :: Map (Date, Date) Fis
phyloFisFiltered = filterFisBySupport True 1 (filterFisByNested phyloFis)

filterFisBySupport :: Bool -> Int -> Map (Date, Date) Fis -> Map (Date, Date) Fis
filterFisBySupport empty min m = case empty of
  True  -> Map.map (\fis -> filterMinorFis min fis) m
  False -> Map.map (\fis -> filterMinorFisNonEmpty min fis) m

filterMinorFis :: Int -> Fis -> Fis
filterMinorFis min fis = Map.filter (\s -> s > min) fis

filterMinorFisNonEmpty :: Int -> Fis -> Fis
filterMinorFisNonEmpty min fis = if (Map.null fis') && (not $ Map.null fis)
                                 then filterMinorFisNonEmpty (min - 1) fis
                                 else fis'
  where 
    fis' = filterMinorFis min fis

doesContains :: [Ngrams] -> [Ngrams] -> Bool
doesContains l l'
  | null l'               = True
  | length l' > length l  = False
  | elem (head l') l      = doesContains l (tail l')
  | otherwise             = False

doesAnyContains :: Set Ngrams -> [Set Ngrams] -> [Set Ngrams] -> Bool
doesAnyContains h l l' = any (\c -> doesContains (Set.toList c) (Set.toList h)) (l' ++ l)

filterNestedCliques :: Set Ngrams -> [Set Ngrams] -> [Set Ngrams] -> [Set Ngrams]
filterNestedCliques h l l'
  | null l                 = if doesAnyContains h l l'
                             then l'
                             else h : l'
  | doesAnyContains h l l' = filterNestedCliques (head l) (tail l) l'
  | otherwise              = filterNestedCliques (head l) (tail l) (h : l')


filterFisByNested :: Map (Date, Date) Fis -> Map (Date, Date) Fis
filterFisByNested = map (\fis -> restrictKeys fis
                              $ Set.fromList
                              $ filterNestedCliques (head (keys fis)) (keys fis) []
                            )

phyloFis :: Map (Date, Date) Fis
phyloFis = termsToFis phyloPeriods 

termsToFis :: Map (Date, Date) [Document]
    -> Map (Date, Date) Fis
termsToFis = corpusToFis (words . text)

-- | TODO: parameters has to be checked
-- | TODO FIS on monotexts
corpusToFis :: (Document -> [Ngrams])
      -> Map (Date, Date) [Document]
      -> Map (Date, Date) (Map (Set Ngrams) Int)
corpusToFis f = map (\d -> fisWithSizePolyMap (Segment 1 20) 1 (map f d))


------------------------------------------------------------------------
-- | STEP 7 | -- Link level -1 to level 0


phyloLinked_m1_0 :: Phylo
phyloLinked_m1_0 = phyloToLinks lvl_m1_0  phyloLinked_0_m1

lvl_m1_0 :: LinkLvl
lvl_m1_0 = (LinkLvl Link_m1_0 (-1) 0)


------------------------------------------------------------------------
-- | STEP 6 | -- Link level 0 to level -1


addPointer :: Semigroup field
           => ASetter current target identity (field -> field)
           -> field -> current -> target
addPointer field targetPointer current =
  set field (<> targetPointer) current

containsIdx :: [Int] -> [Int] -> Bool
containsIdx l l'
  | null l'          = False
  | last l < head l' = False
  | head l' `elem` l = True
  | otherwise        = containsIdx l (tail l')

shouldLink :: LinkLvl -> [Int] -> [Int] -> Bool
shouldLink lvl current target = case linkLvlLabel lvl of 
  Link_0_m1  -> containsIdx target current
  Link_m1_0  -> containsIdx target current
  Link_0_1   -> containsIdx target current
  Link_1_0   -> containsIdx target current
  Link_x_y   -> undefined
  _          -> panic ("error link level to be defined")

linkGroupToGroups :: LinkLvl -> PhyloGroup -> [PhyloGroup] -> PhyloGroup
linkGroupToGroups lvl current targets 
  | linkLvlFrom lvl < linkLvlTo lvl = setLevelParents current 
  | linkLvlFrom lvl > linkLvlTo lvl = setLevelChilds current
  | otherwise = current  
  where 
    setLevelChilds :: PhyloGroup -> PhyloGroup 
    setLevelChilds =  over (phylo_groupLevelChilds) addPointers

    setLevelParents :: PhyloGroup -> PhyloGroup 
    setLevelParents =  over (phylo_groupLevelParents) addPointers

    addPointers :: [Pointer] -> [Pointer]
    addPointers lp = lp ++ Maybe.mapMaybe (\target -> if shouldLink lvl (_phylo_groupNgrams current)
                                                                        (_phylo_groupNgrams target )
                                                      then Just ((getGroupId target),1)
                                                      else Nothing
                                               ) targets 

    addPointers' :: [Pointer] -> [Pointer]
    addPointers' lp = lp ++ map (\target -> ((getGroupId target),1)) targets

linkGroupsByLevel :: LinkLvl -> Phylo -> [PhyloGroup] -> [PhyloGroup]
linkGroupsByLevel lvl p groups  = map (\group ->
  if getGroupLevel group == linkLvlFrom lvl 
  then linkGroupToGroups lvl group (getGroupsWithFilters (linkLvlTo lvl) (getGroupPeriod group) p)
  else group ) groups

phyloToLinks :: LinkLvl -> Phylo -> Phylo
phyloToLinks lvl p = over ( phylo_periods
                          .  traverse
                          . phylo_periodLevels
                          .  traverse
                          . phylo_levelGroups
                          )
                          (linkGroupsByLevel lvl p) p

phyloLinked_0_m1 :: Phylo
phyloLinked_0_m1 = phyloToLinks lvl_0_m1  phyloWithGroups0

lvl_0_m1 :: LinkLvl
lvl_0_m1 = (LinkLvl Link_0_m1 0 (-1))


------------------------------------------------------------------------
-- | STEP 5 | -- Build level 0 as a copy of level -1


-- | To clone the last PhyloLevel of each PhyloPeriod and update it with a new LevelValue 
clonePhyloLevel :: Int -> Phylo -> Phylo
clonePhyloLevel lvl p = alterPhyloLevels (\l -> addPhyloLevel 
                                               (setPhyloLevelId lvl $ head l) 
                                               l) p 

phyloWithGroups0 :: Phylo
phyloWithGroups0 = updatePhyloByLevel (initLevel 0 Level_0) phyloWithGroupsm1


------------------------------------------------------------------------
-- | STEP 4 | -- Build level -1 


-- | To transform a list of Documents into a PhyloLevel
docsToPhyloLevel :: Int ->(Date, Date) -> [Document] -> Phylo -> PhyloLevel
docsToPhyloLevel lvl (d, d') docs p = initPhyloLevel 
                                        ((d, d'), lvl)
                                        (map (\(f,s) -> initGroup [s] s f lvl d d' p) 
                                             $ zip [1..] 
                                             $ (nub . concat) 
                                             $ map (words . text) docs) 

-- | To transform a Map of Periods and Documents into a list of [PhyloPeriod]
docsToPhyloPeriods :: Int -> Map (Date,Date) [Document] -> Phylo -> [PhyloPeriod]
docsToPhyloPeriods lvl docs p = map (\(id,l) -> initPhyloPeriod id l) 
                                    $ Map.toList levels 
  where
    -------------------------------------- 
    levels :: Map (Date,Date) [PhyloLevel]
    levels = mapWithKey (\k v -> [docsToPhyloLevel lvl k v p]) docs
    --------------------------------------

-- | To update a Phylo for a given Levels
updatePhyloByLevel :: Level -> Phylo -> Phylo 
updatePhyloByLevel lvl p 
  = case getLevelLabel lvl of
     
    Level_m1 -> appendPhyloPeriods (docsToPhyloPeriods (getLevelValue lvl) lvlData p) p
      where
        --------------------------------------
        lvlData :: Map (Date,Date) [Document]
        lvlData = phyloPeriods
        --------------------------------------

    Level_0  -> clonePhyloLevel (getLevelValue lvl) p

    Level_1  -> fisToPhyloLevel phyloFisFiltered p

    _        -> panic ("[ERR][Viz.Phylo.Example.updatePhyloByLevel] Level not defined")

phyloWithGroupsm1 :: Phylo
phyloWithGroupsm1 = updatePhyloByLevel (initLevel (-1) Level_m1) phylo


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
parseDocs l docs = map (\(Document d t) -> Document d (unwords 
                                                      $ filter (\x -> Vector.elem x l)
                                                      $ monoTexts t)) docs

-- | To group a list of Documents by fixed periods
groupDocsByPeriod :: Grain -> Step -> [Document] -> Phylo -> Map (Date, Date) [Document]
groupDocsByPeriod g s docs p = docsToPeriods date g s 
                             $ parseDocs (getPhyloNgrams p) docs 

phyloPeriods :: Map (Date, Date) [Document]
phyloPeriods = groupDocsByPeriod 5 3 phyloDocs phylo


------------------------------------------------------------------------
-- | STEP 2 | -- Init an initial list of Ngrams and a Phylo  


-- | To init a Phylomemy
initPhylo :: [Document] -> PhyloNgrams -> Phylo
initPhylo docs ngrams = Phylo (both date $ (last &&& head) docs) ngrams []

-- | To init a PhyloNgrams as a Vector of Ngrams 
initNgrams :: [Ngrams] -> PhyloNgrams
initNgrams l = Vector.fromList $ map toLower l

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

