-- | Cesar et Cleôpatre
-- Exemple de phylomemie
-- French without accents

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Gargantext.Viz.Phylo.Example where


import qualified Data.List as DL
import Data.String (String)
import Data.Text (Text, pack, unwords, toLower)
import Data.Tuple.Extra (both)

import Data.Map (Map)
import qualified Data.Map as DM

import Gargantext.Text.Terms.Mono (monoTexts)
import Gargantext.Prelude

------------------------------------------------------------------------
type Histoire = [Event]
data Event = Event {date:: Double, text :: Text}
  deriving (Show)

type MapList = [Text]
type PeriodeSize = Int
-- data Periodes b a = Map (b,b) a
------------------------------------------------------------------------

toPeriodes :: (Enum b, Fractional b, Ord b) => (t -> b) -> b -> [t] -> Map (b, b) [t]
toPeriodes _ _ [] = panic "Empty history can not have any periode"
toPeriodes f s hs = periodes f st hs
  where
    hs' = DL.sortOn f hs
    st  = steps s $ both f (DL.head hs', DL.last hs')

periodes :: Ord b => (t -> b) -> [(b, b)] -> [t] -> Map (b, b) [t]
periodes f ds h = DM.fromList $ zip ds $ periodes' f ds h

periodes' :: Ord b => (t -> b) -> [(b, b)] -> [t] -> [[t]]
periodes' _ [] _  = []
periodes' f [a] h = [x] <> [y]
  where
    (x,y) = periode f a h
periodes' f (a:b:bs) h = [x] <> periodes' f (b:bs) y
  where
    (x,y) = periode f a h

periode :: Ord b => (t -> b) -> (b, b) -> [t] -> ([t],[t])
periode f (start,end) h = DL.partition (\d -> f d >= start && f d <= end) h

------------------------------------------------------------------------
steps :: (Ord a, Fractional a, Enum a) => a -> (a, a) -> [(a, a)]
steps s (b,e) = zip (DL.init ss) (DL.tail ss)
  where
    ss = steps' s (b,e)

steps' :: (Enum b, Fractional b, Ord b) => b -> (b, b) -> [b]
steps' s (b,e) = case s > 0 of
                  False -> panic "Steps size can not be < 0"
                  True  -> steps'' s (b,e)

steps'' :: (Fractional b, Enum b) => b -> (b, b) -> [b]
steps'' s (start,end) = map (\s' -> s' * s + start) $ [0 .. end']
  where
    end' = ((end + 1)- start) / s

------------------------------------------------------------------------
clean :: MapList -> Histoire -> Histoire
clean ml = map (\(Event d t) -> Event d (unwords $ filter (\x -> elem  x ml) $ monoTexts t))

mapList :: [Text]
mapList = map (toLower . pack) actants

actants :: [String]
actants = [ "Cleopatre"   , "Ptolemee", "Ptolemee-XIII", "Ptolemee-XIV"
          , "Marc-Antoine", "Cesar"   , "Antoine"      , "Octave"  , "Rome"
          , "Alexandrie"  , "Auguste" , "Pompee"       , "Cassius" , "Brutus"]

phyloCorpus :: Histoire
phyloCorpus = map (\(d,t) -> Event d (pack t)) corpus

corpus :: [(Double, String)]
corpus = [ (-51,"Cleopatre règne sur l’egypte entre 51 et 30 av. J.-C. avec ses frères-epoux Ptolemee-XIII et Ptolemee-XIV, puis aux côtes du general romain Marc-Antoine. Elle est celèbre pour avoir ete la compagne de Jules Cesar puis d'Antoine, avec lesquels elle a eu plusieurs enfants. Partie prenante dans la guerre civile opposant Antoine à Octave, elle est vaincue à la bataille d'Actium en 31 av. J.-C. Sa defaite va permettre aux Romains de mener à bien la conquête de l’egypte, evenement qui marquera la fin de l'epoque hellenistique."), (-40,"Il existe relativement peu d'informations sur son sejour à Rome, au lendemain de l'assassinat de Cesar, ou sur la periode passee à Alexandrie durant l'absence d'Antoine, entre -40 et -37."), (-48,"L'historiographie antique lui est globalement defavorable car inspiree par son vainqueur, l'empereur Auguste, et par son entourage, dont l'interêt est de la noircir, afin d'en faire l'adversaire malfaisant de Rome et le mauvais genie d'Antoine. On observe par ailleurs que Cesar ne fait aucune mention de sa liaison avec elle dans les Commentaires sur la Guerre civile"), (-69,"Cleopatre est nee au cours de l'hiver -69/-686 probablement à Alexandrie."), (-48,"Pompee a en effet ete le protecteur de Ptolemee XII, le père de Cleopatre et de Ptolemee-XIII dont il se considère comme le tuteur."), (-48,"Ptolemee-XIII et Cleopatre auraient d'ailleurs aide Pompee par l'envoi d'une flotte de soixante navires."), (-48,"Mais le jeune roi Ptolemee-XIII et ses conseillers jugent sa cause perdue et pensent s'attirer les bonnes graces du vainqueur en le faisant assassiner à peine a-t-il pose le pied sur le sol egyptien, près de Peluse, le 30 juillet 48 av. J.-C., sous les yeux de son entourage."), (-48,"Cesar fait enterrer la tête de Pompee dans le bosquet de Nemesis en bordure du mur est de l'enceinte d'Alexandrie. Pour autant la mort de Pompee est une aubaine pour Cesar qui tente par ailleurs de profiter des querelles dynastiques pour annexer l’egypte."), (-48,"Il est difficile de se prononcer clairement sur les raisons qui ont pousse Cesar à s'attarder à Alexandrie. Il y a des raisons politiques, mais aussi des raisons plus sentimentales (Cleopatre ?). Il tente d'abord d'obtenir le remboursement de dettes que Ptolemee XII"), (-46,"Les deux souverains sont convoques par Cesar au palais royal d'Alexandrie. Ptolemee-XIII s'y rend après diverses tergiversations ainsi que Cleopatre."), (-47,"A Rome, Cleopatre epouse alors un autre de ses frères cadets, à Alexandrie, Ptolemee-XIV, sur l'injonction de Jules Cesar"), (-46,"Cesar a-t-il comme objectif de montrer ce qu'il en coûte de se revolter contre Rome en faisant figurer dans son triomphe la sœur de Cleopatre et de Ptolemee-XIV, Arsinoe, qui s'est fait reconnaître reine par les troupes de Ptolemee-XIII ?"), (-44,"Au debut de l'annee -44, Cesar est assassine par Brutus. Profitant de la situation confuse qui s'ensuit, Cleopatre quitte alors Rome à la mi-avril, faisant escale en Grèce. Elle parvient à Alexandrie en juillet -44."), (-44,"La guerre que se livrent les assassins de Cesar, Cassius et Brutus et ses heritiers, Octave et Marc-Antoine, oblige Cleopatre à des contorsions diplomatiques."), (-41,"Nous ignorons depuis quand Cleopatre, agee de 29 ans en -41, et Marc-Antoine, qui a une quarantaine d'annees, se connaissent. Marc-Antoine est l'un des officiers qui ont participe au retablissement de Ptolemee XII.  Il est plus vraisemblable qu'ils se soient frequentes lors du sejour à Rome de Cleopatre."), (-42,"Brutus tient la Grèce tandis que Cassius s'installe en Syrie. Le gouverneur de Cleopatre à Chypre, Serapion, vient en aide à Cassius."), (-42,"Cassius aurait envisage de s'emparer d'Alexandrie quand le 'debarquement' en Grèce d'Antoine et d'Octave l'oblige à renoncer à ses projets")]


