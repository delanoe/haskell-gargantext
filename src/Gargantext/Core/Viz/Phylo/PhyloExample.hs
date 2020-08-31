{-|
Module      : Gargantext.Core.Viz.Phylo.PhyloExample
Description : Phylomemy example based on history of Cleopatre.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-- | Cesar et Cleôpatre
-- | Exemple de phylomemie
-- | French without accents
-}


module Gargantext.Core.Viz.Phylo.PhyloExample where

import Data.List (sortOn, nub, sort)
import Data.Map (Map)
import Data.Text (Text, toLower)

import Gargantext.Prelude
import Gargantext.Core.Text.Context (TermList)
import Gargantext.Core.Text.Terms.Mono (monoTexts)
import Gargantext.Core.Viz.AdaptativePhylo
import Gargantext.Core.Viz.Phylo.PhyloTools
import Gargantext.Core.Viz.Phylo.PhyloMaker
import Gargantext.Core.Viz.Phylo.PhyloExport
import Gargantext.Core.Viz.Phylo.TemporalMatching (adaptativeTemporalMatching, constanteTemporalMatching)
import Gargantext.Core.Viz.Phylo.SynchronicClustering (synchronicClustering)

import Control.Lens
import Data.GraphViz.Types.Generalised (DotGraph)

import qualified Data.Vector as Vector

---------------------------------
-- | STEP 5 | -- Export the phylo
---------------------------------

phyloExport :: IO ()
phyloExport = dotToFile "/home/qlobbe/data/phylo/output/cesar_cleopatre_V2.dot" phyloDot 

phyloDot :: DotGraph DotId
phyloDot = toPhyloExport phylo2

--------------------------------------------------
-- | STEP 4 | -- Process the synchronic clustering
--------------------------------------------------

phylo2 :: Phylo
phylo2 = synchronicClustering phylo1

-----------------------------------------------
-- | STEP 3 | -- Build the Level 1 of the Phylo
-----------------------------------------------

phylo1 :: Phylo
phylo1 = case (getSeaElevation phyloBase) of 
    Constante s g   -> constanteTemporalMatching s g 
       $ toGroupsProxi 1
       $ appendGroups cliqueToGroup 1 phyloClique phyloBase
    Adaptative s    -> adaptativeTemporalMatching s
       $ toGroupsProxi 1
       $ appendGroups cliqueToGroup 1 phyloClique phyloBase


---------------------------------------------
-- | STEP 2 | -- Build the cliques
---------------------------------------------


phyloClique :: Map (Date,Date) [PhyloClique]
phyloClique = toPhyloClique phyloBase docsByPeriods


docsByPeriods :: Map (Date,Date) [Document]
docsByPeriods = groupDocsByPeriod date periods docs


--------------------------------------------
-- | STEP 1 | -- Init the Base of the Phylo
--------------------------------------------


phyloBase :: Phylo
phyloBase = toPhyloBase docs mapList config


phyloCooc :: Map Date Cooc
phyloCooc = docsToTimeScaleCooc docs (foundations ^. foundations_roots)


periods :: [(Date,Date)]
periods = toPeriods (sort $ nub $ map date docs) (getTimePeriod $ timeUnit config) (getTimeStep $ timeUnit config)


nbDocsByYear :: Map Date Double
nbDocsByYear = docsToTimeScaleNb docs


config :: Config
config = 
    defaultConfig { phyloName  = "Cesar et Cleopatre"
                  , phyloLevel = 2
                  , exportFilter = [ByBranchSize 0]
                  , clique = MaxClique 0 }


docs :: [Document]
docs = map (\(d,t)
    -> Document d ( filter (\n -> isRoots n (foundations ^. foundations_roots)) 
                  $ monoTexts t)) corpus


foundations :: PhyloFoundations
foundations = PhyloFoundations (Vector.fromList $ map toLower actants) mapList 


--------------------------------------------
-- | STEP 0 | -- Let's start with an example
--------------------------------------------


mapList :: TermList
mapList = map (\a -> ([toLower a],[])) actants


actants :: [Ngrams]
actants = [ "Cleopatre"   , "Ptolemee", "Ptolemee-XIII", "Ptolemee-XIV", "Ptolemee-X", "Berenice-III"
          , "Marc-Antoine", "Cesar"   , "Antoine"      , "Octave"  , "Rome"
          , "Alexandrie"  , "Auguste" , "Pompee"       , "Cassius" , "Brutus", "Caesar-III", "Aurelia-Cotta", "Pisae", "Pline"]

corpus :: [(Date, Text)]
corpus = sortOn fst [ 
  (-101,"La tutelle de sa mère lui étant difficile à endurer, en septembre 101 av. J.-C., Ptolemee-X la fait assassiner et peut enfin régner presque seul puisqu'il partage le pouvoir avec son épouse Berenice-III Cléopâtre Philopator."),
  (-99,"Caesar-III est questeur en 99 av. J.-C. ou 98 av. J.-C., et préteur en 92 av. J.-C.."),
  (-100,"Caius Julius Caesar-IV — dit Jules Cesar — naît vers 100 av. J.-C., il est le fils de Caius Julius Caesar-III et de Aurelia-Cotta"),
  (-85,"Caesar-III meurt à Pisae de cause naturelle en 85 av. J.-C. : selon Pline l'Ancien, il décède brusquement en mettant ses chaussures"),
  (-53,"Aurelia-Cotta décède peu de temps avant le meurtre de Clodius Pulcher, vers 53 av. J.-C."),
  (-51,"Cleopatre règne sur l’egypte entre 51 et 30 av. J.-C. avec ses frères-epoux Ptolemee-XIII et Ptolemee-XIV, puis aux côtes du general romain Marc-Antoine. Elle est celèbre pour avoir ete la compagne de Jules Cesar puis d'Antoine, avec lesquels elle a eu plusieurs enfants. Partie prenante dans la guerre civile opposant Antoine à Octave, elle est vaincue à la bataille d'Actium en 31 av. J.-C. Sa defaite va permettre aux Romains de mener à bien la conquête de l’egypte, evenement qui marquera la fin de l'epoque hellenistique."), 
  (-40,"Il existe relativement peu d'informations sur son sejour à Rome, au lendemain de l'assassinat de Cesar, ou sur la periode passee à Alexandrie durant l'absence d'Antoine, entre -40 et -37."), 
  (-48,"L'historiographie antique lui est globalement defavorable car inspiree par son vainqueur, l'empereur Auguste, et par son entourage, dont l'interêt est de la noircir, afin d'en faire l'adversaire malfaisant de Rome et le mauvais genie d'Antoine. On observe par ailleurs que Cesar ne fait aucune mention de sa liaison avec elle dans les Commentaires sur la Guerre civile"), 
  (-69,"Cleopatre est nee au cours de l'hiver -69/-686 probablement à Alexandrie."), 
  (-48,"Pompee a en effet ete le protecteur de Ptolemee XII, le père de Cleopatre et de Ptolemee-XIII dont il se considère comme le tuteur."), 
  (-48,"Ptolemee-XIII et Cleopatre auraient d'ailleurs aide Pompee par l'envoi d'une flotte de soixante navires."), 
  (-48,"Mais le jeune roi Ptolemee-XIII et ses conseillers jugent sa cause perdue et pensent s'attirer les bonnes graces du vainqueur en le faisant assassiner à peine a-t-il pose le pied sur le sol egyptien, près de Peluse, le 30 juillet 48 av. J.-C., sous les yeux de son entourage."), 
  (-48,"Cesar fait enterrer la tête de Pompee dans le bosquet de Nemesis en bordure du mur est de l'enceinte d'Alexandrie. Pour autant la mort de Pompee est une aubaine pour Cesar qui tente par ailleurs de profiter des querelles dynastiques pour annexer l’egypte."), 
  (-48,"Il est difficile de se prononcer clairement sur les raisons qui ont pousse Cesar à s'attarder à Alexandrie. Il y a des raisons politiques, mais aussi des raisons plus sentimentales (Cleopatre ?). Il tente d'abord d'obtenir le remboursement de dettes que Ptolemee XII"), 
  (-46,"Les deux souverains sont convoques par Cesar au palais royal d'Alexandrie. Ptolemee-XIII s'y rend après diverses tergiversations ainsi que Cleopatre."), 
  (-47,"A Rome, Cleopatre epouse alors un autre de ses frères cadets, à Alexandrie, Ptolemee-XIV, sur l'injonction de Jules Cesar"), 
  (-46,"Cesar a-t-il comme objectif de montrer ce qu'il en coûte de se revolter contre Rome en faisant figurer dans son triomphe la sœur de Cleopatre et de Ptolemee-XIV, Arsinoe, qui s'est fait reconnaître reine par les troupes de Ptolemee-XIII ?"), 
  (-44,"Au debut de l'annee -44, Cesar est assassine par Brutus. Profitant de la situation confuse qui s'ensuit, Cleopatre quitte alors Rome à la mi-avril, faisant escale en Grèce. Elle parvient à Alexandrie en juillet -44."), 
  (-44,"La guerre que se livrent les assassins de Cesar, Cassius et Brutus et ses heritiers, Octave et Marc-Antoine, oblige Cleopatre à des contorsions diplomatiques."), 
  (-41,"Nous ignorons depuis quand Cleopatre, agee de 29 ans en -41, et Marc-Antoine, qui a une quarantaine d'annees, se connaissent. Marc-Antoine est l'un des officiers qui ont participe au retablissement de Ptolemee XII.  Il est plus vraisemblable qu'ils se soient frequentes lors du sejour à Rome de Cleopatre."), 
  (-42,"Brutus tient la Grèce tandis que Cassius s'installe en Syrie. Le gouverneur de Cleopatre à Chypre, Serapion, vient en aide à Cassius."), 
  (-42,"Cassius aurait envisage de s'emparer d'Alexandrie quand le 'debarquement' en Grèce d'Antoine et d'Octave l'oblige à renoncer à ses projets")]          