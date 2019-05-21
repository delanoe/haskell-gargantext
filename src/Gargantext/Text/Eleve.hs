{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Implementation of EleVe Python version of papers:


-}
module Gargantext.Text.Eleve where


import Data.Ord (Ord)
import qualified Data.List as List
import Data.Monoid
import Data.Text hiding (map)
import Data.Map (Map)
import qualified Data.Map as Map
import Gargantext.Prelude

-- prop (Noeud c _e f) = c == Map.size f
-- TODO remove Feuille

example :: [[Terminal]]
example = map terminal
        $ chunkAlong 3 1
        $ words "New York and New York is a big apple"

data Terminal = Debut | Terminal Text | Fin
  deriving (Ord, Eq, Show)

isDebutFin :: Terminal -> Bool
isDebutFin x = case x of
        Debut -> True
        Fin   -> True
        _     -> False

terminal :: [Text] -> [Terminal]
--terminal xs = [Debut] <> (map Terminal xs) <> [Fin]
terminal xs = (map Terminal xs) <> [Fin]

data Arbre k e = Noeud { _noeud_count  :: Double
                       , _noeud_entropy :: e
                       , _noeud_fils    :: Map k (Arbre k e)
                       }
           | Feuille { _noeud_count :: Double }
           deriving (Show)

arbreVide :: Arbre k e
arbreVide = Feuille 0

mkArbre :: Monoid e => Double -> Map Terminal (Arbre Terminal e) -> Arbre Terminal e
mkArbre c fils
  | Map.null fils = Feuille c
  | otherwise     = Noeud c mempty fils


insertArbre :: [Terminal] -> Arbre Terminal () -> Arbre Terminal ()
insertArbre [] n = n
insertArbre (x:xs) (Feuille c)    = mkArbre (c+1) (Map.singleton x $ insertArbre xs arbreVide)
insertArbre (x:xs) (Noeud c _e f) = mkArbre (c+1) (case Map.lookup x f of
                                                      Nothing    -> Map.insert x (insertArbre xs arbreVide) f
                                                      Just arbre -> Map.insert x (insertArbre xs arbre    ) f
                                                      )

insertArbres :: [[Terminal]] -> Arbre Terminal ()
insertArbres = List.foldr insertArbre arbreVide

entropyArbre :: Arbre Terminal () -> Arbre Terminal Double
entropyArbre (Feuille c)       = Feuille c
entropyArbre (Noeud c _e fils) = (Noeud c e (map entropyArbre fils))
  where
    e = sum $ map (\(k, f) -> case isDebutFin k of
                           True ->   (_noeud_count f) / c * log c
                           False  -> - c' * log c'
                              where
                                c' = (_noeud_count f) / c
                          )
            $ Map.toList fils

normalizeArbre :: Arbre Terminal Double -> Arbre Terminal Double
normalizeArbre (Feuille c)   = Feuille c
normalizeArbre (Noeud c e f) = Noeud c e (Map.map (\n -> normalizeLevel n $ Map.elems f) f)

normalizeLevel :: Arbre Terminal Double -> [Arbre Terminal Double] -> Arbre Terminal Double
normalizeLevel (Feuille c) _ = Feuille c
normalizeLevel (Noeud c e f) ns = Noeud c ( (e-m) / v) f
  where
    es = map _noeud_entropy ns
    m  = mean es
    v  = variance es

buildArbre :: [[Terminal]] -> Arbre Terminal Double
buildArbre = normalizeArbre . entropyArbre . insertArbres




