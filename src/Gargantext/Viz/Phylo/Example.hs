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

import Control.Lens hiding (both)
import qualified Data.List as List
import Data.Text (Text, unwords, toLower, words)
import Data.Tuple.Extra
import Data.Semigroup (Semigroup)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple

import Data.Set (Set)
import qualified Data.Set as DS
import qualified Data.Matrix as DM'

import Gargantext.Text.Metrics.FrequentItemSet (fisWithSizePolyMap, Size(..))
import Gargantext.Text.Terms.Mono (monoTexts)
import Gargantext.Prelude
import Gargantext.Viz.Phylo

------------------------------------------------------------------------
-- | Types | --


-- | Date : a simple Integer
type Date = Int
-- | Document : a piece of Text linked to a Date
data Document = Document 
      { date :: Date
      , text :: Text
      } deriving (Show)
-- | Corpus : a list of Documents
type Corpus   = [Document]


type MapList = [Ngrams]
type PeriodeSize = Int
-- data Periodes b a = Map (b,b) a
type Occurrences = Int

data Levels = Level_m1 | Level_0 | Level_1 | Level_2 | Level_N
  deriving (Show, Eq, Enum, Bounded)




data LinkLvlLabel = Link_m1_0 | Link_0_m1 | Link_1_0 | Link_x_y
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
-- | STEP 9 | -- Link the PhyloGroups of level 1 through the Periods 


------------------------------------------------------------------------
-- | STEP 8 | -- Cluster the Fis and buil level 1 of the Phylo


------------------------------------------------------------------------
-- | STEP 7 | -- Find the Fis out of Documents and Ngrams and build level 1 of the Phylo 


phyloFis :: Map (Date, Date) Fis
phyloFis = termsToFis phyloTerms 

termsToFis :: Map (Date, Date) [Document]
    -> Map (Date, Date) Fis
termsToFis = corpusToFis (words . text)

-- | TODO: parameters has to be checked
-- | TODO FIS on monotexts
corpusToFis :: (Document -> [Ngrams])
      -> Map (Date, Date) [Document]
      -> Map (Date, Date) (Map (Set Ngrams) Int)
corpusToFis f = Map.map (\d -> fisWithSizePolyMap (Segment 1 20) 1 (map f d))

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

getGroups :: Phylo -> [PhyloGroup]
getGroups = view (phylo_periods . traverse . phylo_periodLevels . traverse . phylo_levelGroups) 

getGroupId :: PhyloGroup -> PhyloGroupId
getGroupId = view (phylo_groupId)

getGroupLvl :: PhyloGroup -> Int
getGroupLvl group = Tuple.snd $ Tuple.fst $ getGroupId group

getGroupPeriod :: PhyloGroup -> (Date,Date)
getGroupPeriod group = Tuple.fst $ Tuple.fst $ getGroupId group

getGroupsByLevelAndPeriod :: Int -> (Date,Date) -> Phylo -> [PhyloGroup]
getGroupsByLevelAndPeriod lvl period p = List.filter (\group -> (getGroupLvl group == lvl) && (getGroupPeriod group == period)) (getGroups p) 

containsIdx :: [Int] -> [Int] -> Bool
containsIdx l l'
  | List.null l' = False
  | List.last l < List.head l' = False
  | List.head l' `List.elem` l = True
  | otherwise = containsIdx l (List.tail l')

shouldLink :: LinkLvl -> [Int] -> [Int] -> Bool
shouldLink lvl current target = case linkLvlLabel lvl of 
  Link_0_m1  -> containsIdx target current
  Link_m1_0  -> containsIdx target current
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
    addPointers lp = lp List.++ Maybe.mapMaybe (\target -> if shouldLink lvl (_phylo_groupNgrams current) (_phylo_groupNgrams target)
                                                           then Just ((getGroupId target),1)
                                                           else Nothing
                                               ) targets 

    addPointers' :: [Pointer] -> [Pointer]
    addPointers' lp = lp List.++ map (\target -> ((getGroupId target),1)) targets    

linkGroupsByLevel :: LinkLvl -> Phylo -> [PhyloGroup] -> [PhyloGroup]
linkGroupsByLevel lvl p groups  = map (\group ->
  if getGroupLvl group == linkLvlFrom lvl 
  then linkGroupToGroups lvl group (getGroupsByLevelAndPeriod (linkLvlTo lvl) (getGroupPeriod group) p)
  else group ) groups

phyloToLinks :: LinkLvl -> Phylo -> Phylo
phyloToLinks lvl p = over (phylo_periods . traverse . phylo_periodLevels . traverse . phylo_levelGroups) (\groups -> linkGroupsByLevel lvl p groups) p

phyloLinked_0_m1 :: Phylo
phyloLinked_0_m1 = phyloToLinks lvl_0_m1  phyloWithGroups0

lvl_0_m1 :: LinkLvl
lvl_0_m1 = (LinkLvl Link_0_m1 0 (-1))

------------------------------------------------------------------------
-- | STEP 5 | -- Build level 0 (for the moment it's just a durty copy of level -1)


setGroupIdLvl :: Int -> PhyloGroup -> PhyloGroup
setGroupIdLvl lvl (PhyloGroup ((period, lvl'), idx) gLabel gNgrams gPP gPC gLP gLC)
  = PhyloGroup ((period, lvl), idx) gLabel gNgrams gPP gPC gLP gLC

setPhyloLevel :: Int -> PhyloLevel -> PhyloLevel
setPhyloLevel lvl (PhyloLevel (periodId, lvl') lvlGroups) 
  = PhyloLevel (periodId, lvl) lvlGroups'
    where
      lvlGroups' = map (\g -> setGroupIdLvl lvl g) lvlGroups

copyPhyloLevel :: Int -> [PhyloLevel] -> [PhyloLevel]
copyPhyloLevel lvl l = (setPhyloLevel lvl (List.head l)) : l 

alterLvl :: Int -> [PhyloPeriod] -> [PhyloPeriod]
alterLvl lvl l = map (\p -> PhyloPeriod (_phylo_periodId p) (copyPhyloLevel lvl $ _phylo_periodLevels p)) l

phyloWithGroups0 :: Phylo
phyloWithGroups0 = updatePhyloByLevel Level_0 phyloWithGroupsm1


------------------------------------------------------------------------
-- | STEP 4 | -- Build level -1 


findIdx :: Ngrams -> Int
findIdx n = case (Vector.elemIndex n (_phylo_ngrams phylo)) of
              Nothing -> panic "PhyloError"
              Just i  -> i

ngramsToGroup :: [Ngrams] -> Text -> Int -> Int -> Int -> Int -> PhyloGroup
ngramsToGroup terms label idx lvl from to = PhyloGroup (((from, to), lvl), idx) label (List.sort (map (\x -> findIdx x) terms)) [] [] [] []

docsToLevel :: (Date, Date) -> Corpus -> PhyloLevel
docsToLevel k v = PhyloLevel (k,(-1)) (map (\x ->
                          ngramsToGroup [Tuple.snd x] (Tuple.snd x) (Tuple.fst x) (-1) (Tuple.fst k) (Tuple.snd k) 
                       ) $ zip [1..] $ (List.nub . List.concat) $ map (words . text) v)

corpusToPhyloPeriod :: Map (Date,Date) Corpus -> [PhyloPeriod]
corpusToPhyloPeriod corpus = map (\x -> PhyloPeriod (Tuple.fst x) [(Tuple.snd x)]) $ zip (Map.keys mapLvl) (Map.elems mapLvl)
    where
      mapLvl :: Map (Date,Date) PhyloLevel 
      mapLvl = Map.mapWithKey docsToLevel corpus

updatePhyloByLevel :: Levels -> Phylo -> Phylo 
updatePhyloByLevel lvl (Phylo pDuration pNgrams pPeriods) 
  = case lvl of 
    
    Level_m1 -> Phylo pDuration pNgrams pPeriods'
      where pPeriods' = (corpusToPhyloPeriod phyloTerms) List.++ pPeriods
    
    Level_0  -> Phylo pDuration pNgrams pPeriods'
      where pPeriods' = alterLvl 0 pPeriods
    
    _        -> panic ("error level to be defined")

phyloWithGroupsm1 :: Phylo
phyloWithGroupsm1 = updatePhyloByLevel Level_m1 phylo


------------------------------------------------------------------------
-- | STEP 3 | -- Split the Corpus into Periods and reduce each Document as a list of Ngrams


phyloTerms :: Map (Date, Date) [Document]
phyloTerms = toPeriodes date 5 3 $ cleanCorpus cleanedActants phyloCorpus

toPeriodes :: (Ord date, Enum date) => (doc -> date)
     -> Grain -> Step -> [doc] -> Map (date, date) [doc]
toPeriodes _ _ _ [] = panic "Empty corpus can not have any periods"
toPeriodes f g s es = Map.fromList $ zip hs $ map (inPeriode f es) hs
  where
    hs = steps g s $ both f (List.head es, List.last es)
    --------------------------------------------------------------------
    -- | Define overlapping periods of time by following regular steps
    inPeriode :: Ord b => (t -> b) -> [t] -> (b, b) -> [t]
    inPeriode f' h (start,end) =
      fst $ List.partition (\d -> f' d >= start && f' d <= end) h
    --------------------------------------------------------------------
    -- | Find steps of linear and homogenous time of integer
    steps :: (Eq date, Enum date) => Grain -> Step -> (date, date) -> [(date, date)]
    steps s' o' (start,end) = map (\l -> (List.head l, List.last l))
                          $ chunkAlong s' o' [start .. end]

cleanCorpus :: MapList -> Corpus -> Corpus
cleanCorpus ml = map (\(Document d t) -> Document d (unwords $ filter (\x -> elem  x ml) $ monoTexts t))


------------------------------------------------------------------------
-- | STEP 2 | -- Find some Ngrams (ie: phyloGroup of level -1) out of the Corpus & init the phylo  


phylo = Phylo (both date $ (List.last &&& List.head) phyloCorpus) phyloNgrams []

phyloNgrams :: PhyloNgrams
phyloNgrams = Vector.fromList cleanedActants

cleanedActants :: [Ngrams]
cleanedActants = map toLower actants

actants :: [Ngrams]
actants = [ "Cleopatre"   , "Ptolemee", "Ptolemee-XIII", "Ptolemee-XIV"
          , "Marc-Antoine", "Cesar"   , "Antoine"      , "Octave"  , "Rome"
          , "Alexandrie"  , "Auguste" , "Pompee"       , "Cassius" , "Brutus"]


------------------------------------------------------------------------
-- | STEP 1 | -- Get a corpus of Documents 


phyloCorpus :: Corpus
phyloCorpus = map (\(d,t) -> Document d t) exampleDocuments

exampleDocuments :: [(Date, Text)]
exampleDocuments = List.sortOn fst [ (-51,"Cleopatre règne sur l’egypte entre 51 et 30 av. J.-C. avec ses frères-epoux Ptolemee-XIII et Ptolemee-XIV, puis aux côtes du general romain Marc-Antoine. Elle est celèbre pour avoir ete la compagne de Jules Cesar puis d'Antoine, avec lesquels elle a eu plusieurs enfants. Partie prenante dans la guerre civile opposant Antoine à Octave, elle est vaincue à la bataille d'Actium en 31 av. J.-C. Sa defaite va permettre aux Romains de mener à bien la conquête de l’egypte, evenement qui marquera la fin de l'epoque hellenistique."), (-40,"Il existe relativement peu d'informations sur son sejour à Rome, au lendemain de l'assassinat de Cesar, ou sur la periode passee à Alexandrie durant l'absence d'Antoine, entre -40 et -37."), (-48,"L'historiographie antique lui est globalement defavorable car inspiree par son vainqueur, l'empereur Auguste, et par son entourage, dont l'interêt est de la noircir, afin d'en faire l'adversaire malfaisant de Rome et le mauvais genie d'Antoine. On observe par ailleurs que Cesar ne fait aucune mention de sa liaison avec elle dans les Commentaires sur la Guerre civile"), (-69,"Cleopatre est nee au cours de l'hiver -69/-686 probablement à Alexandrie."), (-48,"Pompee a en effet ete le protecteur de Ptolemee XII, le père de Cleopatre et de Ptolemee-XIII dont il se considère comme le tuteur."), (-48,"Ptolemee-XIII et Cleopatre auraient d'ailleurs aide Pompee par l'envoi d'une flotte de soixante navires."), (-48,"Mais le jeune roi Ptolemee-XIII et ses conseillers jugent sa cause perdue et pensent s'attirer les bonnes graces du vainqueur en le faisant assassiner à peine a-t-il pose le pied sur le sol egyptien, près de Peluse, le 30 juillet 48 av. J.-C., sous les yeux de son entourage."), (-48,"Cesar fait enterrer la tête de Pompee dans le bosquet de Nemesis en bordure du mur est de l'enceinte d'Alexandrie. Pour autant la mort de Pompee est une aubaine pour Cesar qui tente par ailleurs de profiter des querelles dynastiques pour annexer l’egypte."), (-48,"Il est difficile de se prononcer clairement sur les raisons qui ont pousse Cesar à s'attarder à Alexandrie. Il y a des raisons politiques, mais aussi des raisons plus sentimentales (Cleopatre ?). Il tente d'abord d'obtenir le remboursement de dettes que Ptolemee XII"), (-46,"Les deux souverains sont convoques par Cesar au palais royal d'Alexandrie. Ptolemee-XIII s'y rend après diverses tergiversations ainsi que Cleopatre."), (-47,"A Rome, Cleopatre epouse alors un autre de ses frères cadets, à Alexandrie, Ptolemee-XIV, sur l'injonction de Jules Cesar"), (-46,"Cesar a-t-il comme objectif de montrer ce qu'il en coûte de se revolter contre Rome en faisant figurer dans son triomphe la sœur de Cleopatre et de Ptolemee-XIV, Arsinoe, qui s'est fait reconnaître reine par les troupes de Ptolemee-XIII ?"), (-44,"Au debut de l'annee -44, Cesar est assassine par Brutus. Profitant de la situation confuse qui s'ensuit, Cleopatre quitte alors Rome à la mi-avril, faisant escale en Grèce. Elle parvient à Alexandrie en juillet -44."), (-44,"La guerre que se livrent les assassins de Cesar, Cassius et Brutus et ses heritiers, Octave et Marc-Antoine, oblige Cleopatre à des contorsions diplomatiques."), (-41,"Nous ignorons depuis quand Cleopatre, agee de 29 ans en -41, et Marc-Antoine, qui a une quarantaine d'annees, se connaissent. Marc-Antoine est l'un des officiers qui ont participe au retablissement de Ptolemee XII.  Il est plus vraisemblable qu'ils se soient frequentes lors du sejour à Rome de Cleopatre."), (-42,"Brutus tient la Grèce tandis que Cassius s'installe en Syrie. Le gouverneur de Cleopatre à Chypre, Serapion, vient en aide à Cassius."), (-42,"Cassius aurait envisage de s'emparer d'Alexandrie quand le 'debarquement' en Grèce d'Antoine et d'Octave l'oblige à renoncer à ses projets")]

