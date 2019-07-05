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

import Data.GraphViz.Types.Generalised (DotGraph)

import Control.Lens hiding (both, Level)
import Data.Text (Text, toLower)
import Data.List        ((++))
import Data.Map         (Map,empty)
import Data.Tuple       (fst)
import Data.Vector      (Vector)
import Gargantext.Prelude
import Gargantext.Text.Context (TermList)
import Gargantext.Viz.Phylo
import Gargantext.Viz.Phylo.Cluster
import Gargantext.Viz.Phylo.Aggregates
import Gargantext.Viz.Phylo.BranchMaker
import Gargantext.Viz.Phylo.LevelMaker
import Gargantext.Viz.Phylo.LinkMaker
import Gargantext.Viz.Phylo.Tools
import Gargantext.Viz.Phylo.View.ViewMaker
import Gargantext.Viz.Phylo.View.Export
import Gargantext.Viz.Phylo.Main (writePhylo)
import GHC.IO (FilePath)
import qualified Data.List   as List

------------------------------------------------------
-- | STEP 12 | -- Create a PhyloView from a user Query
------------------------------------------------------


export :: IO ()
export = dotToFile "/home/qlobbe/data/phylo/output/cesar_cleopatre.dot" phyloDot 

phyloDot :: DotGraph DotId
phyloDot = viewToDot phyloView


phyloExport :: FilePath -> IO FilePath
phyloExport fp = writePhylo fp phyloView

phyloView :: PhyloView
phyloView = toPhyloView (queryParser' queryViewEx) phyloFromQuery

-- | To do : create an other request handler and an other query parser
queryParser' :: [Char] -> PhyloQueryView
queryParser' _q = phyloQueryView

queryViewEx :: [Char]
queryViewEx = "level=3"
              ++ "&childs=false"
              ++ "&filter=LonelyBranchFilter"
              ++ "&metric=BranchAge"
              ++ "&tagger=BranchPeakFreq"
              ++ "&tagger=GroupLabelCooc"


phyloQueryView :: PhyloQueryView
phyloQueryView = PhyloQueryView 2 Merge False 2 [BranchAge,BranchBirth] [] [BranchPeakInc,GroupLabelIncDyn] (Just (ByBranchBirth,Asc)) Json Flat True


--------------------------------------------------
-- | STEP 11 | -- Create a Phylo from a user Query
--------------------------------------------------


phyloFromQuery :: Phylo
phyloFromQuery = toPhylo phyloQueryBuild docs termList empty

-- | To do : create a request handler and a query parser
queryParser :: [Char] -> PhyloQueryBuild
queryParser _q = phyloQueryBuild

queryEx :: [Char]
queryEx = "title=Cesar et Cleôpatre"
          ++ "&desc=An example of Phylomemy (french without accent)"
          ++ "grain=5&steps=3"
          ++ "cluster=FrequentItemSet"
          ++ "interTemporalMatching=WeightedLogJaccard"
          ++ "nthLevel=2"
          ++ "nthCluster=RelatedComponents"
          ++ "nthProximity=Filiation"

phyloQueryBuild :: PhyloQueryBuild
phyloQueryBuild = PhyloQueryBuild "Cesar et Cleôpatre" "An example of Phylomemy (french without accent)"
             3 1 defaultFis [] [] (WeightedLogJaccard $ WLJParams 0.5 20) 5 0.8 0.5 4 2 (RelatedComponents $ RCParams $ WeightedLogJaccard $ WLJParams 0.3 0) 



----------------------------------------------------------------------------------------------------------------------------
-- | STEP 10 | -- Incrementaly cluster the PhyloGroups n times, link them through the Periods and build level n of the Phylo
----------------------------------------------------------------------------------------------------------------------------


phylo6 :: Phylo
phylo6 = toNthLevel 6 defaultWeightedLogJaccard (RelatedComponents (initRelatedComponents (Just defaultWeightedLogJaccard))) phylo3


phylo3 :: Phylo
phylo3 = setPhyloBranches 3
       $ interTempoMatching Descendant 3 defaultWeightedLogJaccard
       $ interTempoMatching Ascendant 3 defaultWeightedLogJaccard
       $ setLevelLinks (2,3)
       $ addPhyloLevel 3
          (phyloToClusters 2 (RelatedComponents (initRelatedComponents (Just defaultWeightedLogJaccard))) phyloBranch2)
          phyloBranch2


--------------------------------
-- | STEP 9 | -- Cluster the Fis
--------------------------------


phyloBranch2 :: Phylo
phyloBranch2 = setPhyloBranches 2 phylo2_c


phylo2_c :: Phylo
phylo2_c = interTempoMatching Descendant 2 defaultWeightedLogJaccard phylo2_p


phylo2_p :: Phylo
phylo2_p = interTempoMatching Ascendant 2 defaultWeightedLogJaccard phylo2_1_2


phylo2_1_2 :: Phylo
phylo2_1_2 = setLevelLinks (1,2) phylo2


-- | phylo2 allready contains the LevelChilds links from 2 to 1
phylo2 :: Phylo
phylo2 = addPhyloLevel 2 phyloCluster phyloBranch1


phyloCluster :: Map (Date,Date) [PhyloCluster]
phyloCluster = phyloToClusters 2 (RelatedComponents $ RCParams $ WeightedLogJaccard $ WLJParams 0.05 10) phyloBranch1


----------------------------------
-- | STEP 8 | -- Find the Branches
----------------------------------


phyloBranch1 :: Phylo
phyloBranch1 = setPhyloBranches 1 phylo1_c


--------------------------------------------------------------------
-- | STEP 7 | -- Link the PhyloGroups of level 1 through the Periods
--------------------------------------------------------------------


phylo1_c :: Phylo
phylo1_c = interTempoMatching Descendant 1 defaultWeightedLogJaccard phylo1_p


phylo1_p :: Phylo
phylo1_p = interTempoMatching Ascendant 1 defaultWeightedLogJaccard phylo1_0_1


-----------------------------------------------
-- | STEP 6 | -- Build the level 1 of the Phylo
-----------------------------------------------


phylo1_0_1 :: Phylo
phylo1_0_1 = setLevelLinks (0,1) phylo1


-- phylo1_1_0 :: Phylo
-- phylo1_1_0 = setLevelLinks (1,0) phylo1


phylo1 :: Phylo
phylo1 =  addPhyloLevel (1) phyloFis phylo'


-------------------------------------------------------------------
-- | STEP 5 | -- Create lists of Frequent Items Set and filter them
-------------------------------------------------------------------

phylo' :: Phylo 
phylo' = phylo & phylo_fis .~ phyloFis

phyloFis :: Map (Date, Date) [PhyloFis]
phyloFis = refineFis (docsToFis phyloDocs phylo) True 1 1

----------------------------------------
-- | STEP 2 | -- Init a Phylo of level 0
----------------------------------------


phylo :: Phylo
phylo = addPhyloLevel 0 phyloDocs phyloBase


phyloDocs :: Map (Date, Date) [Document]
phyloDocs = groupDocsByPeriod date (getPhyloPeriods phyloBase) docs


------------------------------------------------------------------------
-- | STEP 1 | -- Init the Base of the Phylo from Periods and Foundations
------------------------------------------------------------------------


phyloBase :: Phylo 
phyloBase = toPhyloBase phyloQueryBuild phyloParam docs termList empty

phyloParam :: PhyloParam
phyloParam = (initPhyloParam (Just defaultPhyloVersion) (Just defaultSoftware) (Just phyloQueryBuild))

docs :: [Document]
docs = parseDocs foundationsRoots corpus

foundationsRoots :: Vector Ngrams
foundationsRoots = initFoundationsRoots (termListToNgrams termList)


--------------------------------------------
-- | STEP 0 | -- Let's start with an example
--------------------------------------------

termList :: TermList
termList = map (\a -> ([toLower a],[])) actants

actants :: [Ngrams]
actants = [ "Cleopatre"   , "Ptolemee", "Ptolemee-XIII", "Ptolemee-XIV"
          , "Marc-Antoine", "Cesar"   , "Antoine"      , "Octave"  , "Rome"
          , "Alexandrie"  , "Auguste" , "Pompee"       , "Cassius" , "Brutus"]

corpus :: [(Date, Text)]
corpus = List.sortOn fst [ (-51,"Cleopatre règne sur l’egypte entre 51 et 30 av. J.-C. avec ses frères-epoux Ptolemee-XIII et Ptolemee-XIV, puis aux côtes du general romain Marc-Antoine. Elle est celèbre pour avoir ete la compagne de Jules Cesar puis d'Antoine, avec lesquels elle a eu plusieurs enfants. Partie prenante dans la guerre civile opposant Antoine à Octave, elle est vaincue à la bataille d'Actium en 31 av. J.-C. Sa defaite va permettre aux Romains de mener à bien la conquête de l’egypte, evenement qui marquera la fin de l'epoque hellenistique."), (-40,"Il existe relativement peu d'informations sur son sejour à Rome, au lendemain de l'assassinat de Cesar, ou sur la periode passee à Alexandrie durant l'absence d'Antoine, entre -40 et -37."), (-48,"L'historiographie antique lui est globalement defavorable car inspiree par son vainqueur, l'empereur Auguste, et par son entourage, dont l'interêt est de la noircir, afin d'en faire l'adversaire malfaisant de Rome et le mauvais genie d'Antoine. On observe par ailleurs que Cesar ne fait aucune mention de sa liaison avec elle dans les Commentaires sur la Guerre civile"), (-69,"Cleopatre est nee au cours de l'hiver -69/-686 probablement à Alexandrie."), (-48,"Pompee a en effet ete le protecteur de Ptolemee XII, le père de Cleopatre et de Ptolemee-XIII dont il se considère comme le tuteur."), (-48,"Ptolemee-XIII et Cleopatre auraient d'ailleurs aide Pompee par l'envoi d'une flotte de soixante navires."), (-48,"Mais le jeune roi Ptolemee-XIII et ses conseillers jugent sa cause perdue et pensent s'attirer les bonnes graces du vainqueur en le faisant assassiner à peine a-t-il pose le pied sur le sol egyptien, près de Peluse, le 30 juillet 48 av. J.-C., sous les yeux de son entourage."), (-48,"Cesar fait enterrer la tête de Pompee dans le bosquet de Nemesis en bordure du mur est de l'enceinte d'Alexandrie. Pour autant la mort de Pompee est une aubaine pour Cesar qui tente par ailleurs de profiter des querelles dynastiques pour annexer l’egypte."), (-48,"Il est difficile de se prononcer clairement sur les raisons qui ont pousse Cesar à s'attarder à Alexandrie. Il y a des raisons politiques, mais aussi des raisons plus sentimentales (Cleopatre ?). Il tente d'abord d'obtenir le remboursement de dettes que Ptolemee XII"), (-46,"Les deux souverains sont convoques par Cesar au palais royal d'Alexandrie. Ptolemee-XIII s'y rend après diverses tergiversations ainsi que Cleopatre."), (-47,"A Rome, Cleopatre epouse alors un autre de ses frères cadets, à Alexandrie, Ptolemee-XIV, sur l'injonction de Jules Cesar"), (-46,"Cesar a-t-il comme objectif de montrer ce qu'il en coûte de se revolter contre Rome en faisant figurer dans son triomphe la sœur de Cleopatre et de Ptolemee-XIV, Arsinoe, qui s'est fait reconnaître reine par les troupes de Ptolemee-XIII ?"), (-44,"Au debut de l'annee -44, Cesar est assassine par Brutus. Profitant de la situation confuse qui s'ensuit, Cleopatre quitte alors Rome à la mi-avril, faisant escale en Grèce. Elle parvient à Alexandrie en juillet -44."), (-44,"La guerre que se livrent les assassins de Cesar, Cassius et Brutus et ses heritiers, Octave et Marc-Antoine, oblige Cleopatre à des contorsions diplomatiques."), (-41,"Nous ignorons depuis quand Cleopatre, agee de 29 ans en -41, et Marc-Antoine, qui a une quarantaine d'annees, se connaissent. Marc-Antoine est l'un des officiers qui ont participe au retablissement de Ptolemee XII.  Il est plus vraisemblable qu'ils se soient frequentes lors du sejour à Rome de Cleopatre."), (-42,"Brutus tient la Grèce tandis que Cassius s'installe en Syrie. Le gouverneur de Cleopatre à Chypre, Serapion, vient en aide à Cassius."), (-42,"Cassius aurait envisage de s'emparer d'Alexandrie quand le 'debarquement' en Grèce d'Antoine et d'Octave l'oblige à renoncer à ses projets")]
