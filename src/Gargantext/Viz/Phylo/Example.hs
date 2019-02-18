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

import qualified Data.List as List
import Data.Text (Text, unwords, toLower, words)
import Data.Tuple.Extra

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

--------------------------------------------------------------------


data PhyloField = PhyloField {
    phyloField_id :: Int
}

--------------------------------------------------------------------
phyloExampleFinal :: Phylo
phyloExampleFinal = undefined

--------------------------------------------------------------------
appariement :: Map (Date, Date) (Map (Set Ngrams) Int)
appariement = undefined

--------------------------------------------------------------------

fisToFields :: Fis
    -> [PhyloField]
fisToFields = undefined

phyloClusters :: Map (Date,Date) [PhyloField]
phyloClusters = undefined

------------------------------------------------------------------------
-- | STEP 8 | -- Incrementaly cluster the PhyloGroups n times, link them through the Periods and build level n of the Phylo   


------------------------------------------------------------------------
-- | STEP 7 | -- Link the PhyloGroups of level 1 through the Periods 


------------------------------------------------------------------------
-- | STEP 6 | -- Cluster the Fis and buil level 1 of the Phylo


------------------------------------------------------------------------
-- | STEP 5 | -- Find the Fis out of Documents and Ngrams and build level 1 of the Phylo 


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
-- | STEP 4 | -- Build level -1 and 0 of the Phylo 

-- makePointer :: a -> PhyloGroup -> PhyloGroup -> PhyloGroup
-- makePointer field source target = source {field = _phylo_groupId target : field source}

alterLvl :: PhyloGroup -> PhyloGroup
alterLvl g = g {_phylo_groupId = ((Tuple.fst $ Tuple.fst $ _phylo_groupId g, 0), Tuple.snd $ _phylo_groupId g)} 

-- | for the moment level 0 is just a copy of level -1
level0PhyloGroups :: [PhyloGroup]
level0PhyloGroups = map alterLvl initPhyloGroups

findIdx :: Ngrams -> Int
findIdx n = Maybe.fromJust $ Vector.elemIndex n phyloNgrams

ngramsToGroup :: [Ngrams] -> Text -> Int -> Int -> Int -> Int -> PhyloGroup
ngramsToGroup terms label idx lvl from to = PhyloGroup (((from, to), lvl), idx) label (map (\x -> findIdx x) terms) [] [] [] []

docsToGroups :: (Date, Date) -> [Document] -> [PhyloGroup]
docsToGroups k v = map (\x ->
                          ngramsToGroup [Tuple.snd x] (Tuple.snd x) (Tuple.fst x) (-1) (Tuple.fst k) (Tuple.snd k) 
                       ) $ zip [1,2..] $ (List.nub . List.concat) $ map (words . text) v

toPhyloGroups :: a -> Int -> [PhyloGroup]
toPhyloGroups m lvl = case lvl of 
  (-1) -> List.concat $ Map.elems $ Map.mapWithKey docsToGroups phyloTerms

-- | aka: level -1
initPhyloGroups :: [PhyloGroup]
initPhyloGroups = toPhyloGroups phyloTerms (-1)


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


-- phylo = Phylo (both date $ (List.last &&& List.head) phyloCorpus) (initPhyloNgrams cleanedActants) undefined

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

