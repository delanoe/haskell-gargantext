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
{-# LANGUAGE MultiParamTypeClasses #-}

module Gargantext.Viz.Phylo.Example where

import Control.Lens     hiding (makeLenses, both, Level)

import Data.Bool        (Bool, not)
import Data.List        (notElem, concat, union, intersect, tails, tail, head, last, null, zip, sort, length, any, (++), (!!), nub, sortOn, reverse, splitAt, take, delete, init, groupBy)
import Data.Map         (Map, elems, member, adjust, singleton, empty, (!), keys, restrictKeys, mapWithKey, filterWithKey, mapKeys, intersectionWith, unionWith)
import Data.Semigroup   (Semigroup)
import Data.Set         (Set)
import Data.Text        (Text, unwords, toLower, words)
import Data.Tuple       (fst, snd)
import Data.Tuple.Extra
import Data.Vector      (Vector, fromList, elemIndex, (!))

import Gargantext.Prelude          hiding (head)
import Gargantext.Text.Terms.Mono  (monoTexts)

import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Aggregates.Cluster 
import Gargantext.Viz.Phylo.Aggregates.Cooc
import Gargantext.Viz.Phylo.Aggregates.Document 
import Gargantext.Viz.Phylo.Aggregates.Fis                   
import Gargantext.Viz.Phylo.BranchMaker
import Gargantext.Viz.Phylo.LevelMaker
import Gargantext.Viz.Phylo.LinkMaker
import Gargantext.Viz.Phylo.Metrics.Proximity
import Gargantext.Viz.Phylo.Metrics.Clustering
import Gargantext.Viz.Phylo.Tools


import qualified Data.Bool   as Bool
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Maybe  as Maybe
import qualified Data.Set    as Set
import qualified Data.Tuple  as Tuple
import qualified Data.Vector as Vector


------------------------------------------------------------------------
-- | STEP 12 | -- Return a Phylo for upcomming visiualization tasks 


-- | To get all the single PhyloPeriodIds covered by a PhyloBranch
getBranchPeriods :: PhyloBranch -> [PhyloPeriodId]
getBranchPeriods b = nub $ map (fst . fst) $ getBranchGroupIds b 


-- | To get all the single PhyloPeriodIds covered by a PhyloBranch
getBranchGroupIds :: PhyloBranch -> [PhyloGroupId]
getBranchGroupIds =_phylo_branchGroups 


-- | To transform a list of Ngrams Indexes into a Label
ngramsToLabel :: Vector Ngrams -> [Int] -> Text 
ngramsToLabel ngrams l = unwords $ ngramsToText ngrams l 


-- | To transform a list of Ngrams Indexes into a list of Text 
ngramsToText :: Vector Ngrams -> [Int] -> [Text]
ngramsToText ngrams l = map (\idx -> ngrams Vector.! idx) l


-- | To get the nth most frequent Ngrams in a list of PhyloGroups
mostFreqNgrams :: Int -> [PhyloGroup] -> [Int]
mostFreqNgrams thr groups = map fst 
                          $ take thr 
                          $ reverse 
                          $ sortOn snd 
                          $ map (\g -> (head g,length g)) 
                          $ groupBy (==) 
                          $ (sort . concat) 
                          $ map getGroupNgrams groups


-- | To get the (nth `div` 2) most cooccuring Ngrams in a PhyloGroup 
mostOccNgrams :: Int -> PhyloGroup -> [Int]
mostOccNgrams thr group = (nub . concat ) 
                        $ map (\((f,s),d) -> [f,s]) 
                        $ take (thr `div` 2) 
                        $ reverse $ sortOn snd $ Map.toList $ getGroupCooc group


freqToLabel :: Int -> [PhyloGroup] -> Vector Ngrams -> Text
freqToLabel thr l ngs = ngramsToLabel ngs $ mostFreqNgrams thr l  


-- | To filter a list of Branches by avoiding the lone's one (ie: with just a few phyloGroups in the middle of the whole timeline)
filterLoneBranches :: Int -> Int -> Int -> [PhyloPeriodId] -> [PhyloBranch] -> [PhyloBranch]
filterLoneBranches nbPinf nbPsup nbG periods branches = filter (not . isLone) branches
  where
    --------------------------------------
    isLone :: PhyloBranch -> Bool
    isLone b = ((length . getBranchGroupIds) b <= nbG)
               && notElem ((head . getBranchPeriods) b) (take nbPinf periods)
               && notElem ((head . getBranchPeriods) b) (take nbPsup $ reverse periods)
    --------------------------------------

-- alterBranchLabel :: (Int -> [PhyloGroup] -> Vector Ngrams -> Text) -> PhyloBranch -> Phylo -> PhyloBranch
-- alterBranchLabel f b p = over (phylo_branchLabel) (\lbl -> f 2 (getGroupsFromIds (getBranchGroupIds b) p) (getVector Ngrams p)) b

-- toPhyloView1 :: Level -> Phylo -> [PhyloBranch]
-- toPhyloView1 lvl p = bs 
--   where 
--     bs = map (\b -> alterBranchLabel freqToLabel b p)
--        $ filterLoneBranches 1 1 1 (getPhyloPeriods p)
--        $ filter (\b -> (fst . _phylo_branchId) b == lvl) 
--        $ getPhyloBranches p 

-- view1 = toPhyloView1 2 phylo3 

------------------------------------------------------------------------
-- | STEP 11 | -- Incrementaly cluster the PhyloGroups n times, link them through the Periods and build level n of the Phylo   


phylo6 :: Phylo
phylo6 = toNthLevel 6 (WeightedLogJaccard,[0.01,0]) (RelatedComponents, []) (WeightedLogJaccard,[0.01,0]) phylo3  


phylo3 :: Phylo
phylo3 = pairGroupsToGroups Childs  3 (WeightedLogJaccard,[0.01,0])
       $ pairGroupsToGroups Parents 3 (WeightedLogJaccard,[0.01,0]) 
       $ setLevelLinks (2,3) 
       $ addPhyloLevel 3 
          (phyloToClusters 2 (WeightedLogJaccard,[0.01,0]) (RelatedComponents, []) phyloBranch2) 
          phyloBranch2


------------------------------------------------------------------------
-- | STEP 10 | -- Cluster the Fis

phyloBranch2 :: Phylo
phyloBranch2 = phylo2_c
-- phyloBranch2 = setPhyloBranches 2 phylo2_c


phylo2_c :: Phylo
phylo2_c = pairGroupsToGroups Childs 2 (WeightedLogJaccard,[0.01,0]) phylo2_p


phylo2_p :: Phylo
phylo2_p = pairGroupsToGroups Parents 2 (WeightedLogJaccard,[0.01,0]) phylo2_1_2


phylo2_1_2 :: Phylo
phylo2_1_2 = setLevelLinks (1,2) phylo2


-- | phylo2 allready contains the LevelChilds links from 2 to 1
phylo2 :: Phylo
phylo2 = addPhyloLevel 2 phyloCluster phyloBranch1


phyloCluster :: Map (Date,Date) [Cluster] 
phyloCluster = phyloToClusters 1 (WeightedLogJaccard,[0.01,0]) (RelatedComponents, []) phyloBranch1


------------------------------------------------------------------------
-- | STEP 9 | -- Find the Branches


phyloBranch1 = phylo1_c

-- phyloBranch1 :: Phylo
-- phyloBranch1 = setPhyloBranches 1 phylo1_c


------------------------------------------------------------------------
-- | STEP 8 | -- Link the PhyloGroups of level 1 through the Periods  


phylo1_c :: Phylo
phylo1_c = pairGroupsToGroups Childs 1 (WeightedLogJaccard,[0.01,0]) phylo1_p


phylo1_p :: Phylo
phylo1_p = pairGroupsToGroups Parents 1 (WeightedLogJaccard,[0.01,0]) phylo1_0_1


------------------------------------------------------------------------
-- | STEP 7 | -- Build the coocurency Matrix of the Phylo 


phyloCooc :: Map (Int, Int) Double
phyloCooc = fisToCooc phyloFis phylo1_0_1


------------------------------------------------------------------------
-- | STEP 6 | -- Build the level 1 of the Phylo 


phylo1_0_1 :: Phylo
phylo1_0_1 = setLevelLinks (0,1) phylo1_1_0


phylo1_1_0 :: Phylo
phylo1_1_0 = setLevelLinks (1,0) phylo1


phylo1 :: Phylo
phylo1 =  addPhyloLevel (1) phyloFis phylo


------------------------------------------------------------------------
-- | STEP 5 | -- Create lists of Frequent Items Set and filter them


phyloFis :: Map (Date, Date) [Fis]
phyloFis = filterFisBySupport False 1 (filterFisByNested (docsToFis phyloDocs))


------------------------------------------------------------------------
-- | STEP 2 | -- Init a Phylo of level 0


-- phylo' :: Phylo
-- phylo' = initPhylo 5 3 corpus actants groupNgramsWithTrees


phylo :: Phylo
phylo = addPhyloLevel 0 phyloDocs phyloBase


phyloDocs :: Map (Date, Date) [Document]
phyloDocs = corpusToDocs groupNgramsWithTrees corpus phyloBase


------------------------------------------------------------------------
-- | STEP 1 | -- Init the Base of the Phylo from Periods and Foundations


phyloBase :: Phylo
phyloBase = initPhyloBase periods foundations


periods :: [(Date,Date)] 
periods = initPeriods 5 3 
        $ both fst (head corpus,last corpus)


foundations :: Vector Ngrams
foundations = initFoundations actants


------------------------------------------------------------------------
-- | STEP 0 | -- Let's start with an example


actants :: [Ngrams]
actants = [ "Cleopatre"   , "Ptolemee", "Ptolemee-XIII", "Ptolemee-XIV"
          , "Marc-Antoine", "Cesar"   , "Antoine"      , "Octave"  , "Rome"
          , "Alexandrie"  , "Auguste" , "Pompee"       , "Cassius" , "Brutus"]


corpus :: [(Date, Text)]
corpus = List.sortOn fst [ (-51,"Cleopatre règne sur l’egypte entre 51 et 30 av. J.-C. avec ses frères-epoux Ptolemee-XIII et Ptolemee-XIV, puis aux côtes du general romain Marc-Antoine. Elle est celèbre pour avoir ete la compagne de Jules Cesar puis d'Antoine, avec lesquels elle a eu plusieurs enfants. Partie prenante dans la guerre civile opposant Antoine à Octave, elle est vaincue à la bataille d'Actium en 31 av. J.-C. Sa defaite va permettre aux Romains de mener à bien la conquête de l’egypte, evenement qui marquera la fin de l'epoque hellenistique."), (-40,"Il existe relativement peu d'informations sur son sejour à Rome, au lendemain de l'assassinat de Cesar, ou sur la periode passee à Alexandrie durant l'absence d'Antoine, entre -40 et -37."), (-48,"L'historiographie antique lui est globalement defavorable car inspiree par son vainqueur, l'empereur Auguste, et par son entourage, dont l'interêt est de la noircir, afin d'en faire l'adversaire malfaisant de Rome et le mauvais genie d'Antoine. On observe par ailleurs que Cesar ne fait aucune mention de sa liaison avec elle dans les Commentaires sur la Guerre civile"), (-69,"Cleopatre est nee au cours de l'hiver -69/-686 probablement à Alexandrie."), (-48,"Pompee a en effet ete le protecteur de Ptolemee XII, le père de Cleopatre et de Ptolemee-XIII dont il se considère comme le tuteur."), (-48,"Ptolemee-XIII et Cleopatre auraient d'ailleurs aide Pompee par l'envoi d'une flotte de soixante navires."), (-48,"Mais le jeune roi Ptolemee-XIII et ses conseillers jugent sa cause perdue et pensent s'attirer les bonnes graces du vainqueur en le faisant assassiner à peine a-t-il pose le pied sur le sol egyptien, près de Peluse, le 30 juillet 48 av. J.-C., sous les yeux de son entourage."), (-48,"Cesar fait enterrer la tête de Pompee dans le bosquet de Nemesis en bordure du mur est de l'enceinte d'Alexandrie. Pour autant la mort de Pompee est une aubaine pour Cesar qui tente par ailleurs de profiter des querelles dynastiques pour annexer l’egypte."), (-48,"Il est difficile de se prononcer clairement sur les raisons qui ont pousse Cesar à s'attarder à Alexandrie. Il y a des raisons politiques, mais aussi des raisons plus sentimentales (Cleopatre ?). Il tente d'abord d'obtenir le remboursement de dettes que Ptolemee XII"), (-46,"Les deux souverains sont convoques par Cesar au palais royal d'Alexandrie. Ptolemee-XIII s'y rend après diverses tergiversations ainsi que Cleopatre."), (-47,"A Rome, Cleopatre epouse alors un autre de ses frères cadets, à Alexandrie, Ptolemee-XIV, sur l'injonction de Jules Cesar"), (-46,"Cesar a-t-il comme objectif de montrer ce qu'il en coûte de se revolter contre Rome en faisant figurer dans son triomphe la sœur de Cleopatre et de Ptolemee-XIV, Arsinoe, qui s'est fait reconnaître reine par les troupes de Ptolemee-XIII ?"), (-44,"Au debut de l'annee -44, Cesar est assassine par Brutus. Profitant de la situation confuse qui s'ensuit, Cleopatre quitte alors Rome à la mi-avril, faisant escale en Grèce. Elle parvient à Alexandrie en juillet -44."), (-44,"La guerre que se livrent les assassins de Cesar, Cassius et Brutus et ses heritiers, Octave et Marc-Antoine, oblige Cleopatre à des contorsions diplomatiques."), (-41,"Nous ignorons depuis quand Cleopatre, agee de 29 ans en -41, et Marc-Antoine, qui a une quarantaine d'annees, se connaissent. Marc-Antoine est l'un des officiers qui ont participe au retablissement de Ptolemee XII.  Il est plus vraisemblable qu'ils se soient frequentes lors du sejour à Rome de Cleopatre."), (-42,"Brutus tient la Grèce tandis que Cassius s'installe en Syrie. Le gouverneur de Cleopatre à Chypre, Serapion, vient en aide à Cassius."), (-42,"Cassius aurait envisage de s'emparer d'Alexandrie quand le 'debarquement' en Grèce d'Antoine et d'Octave l'oblige à renoncer à ses projets")]