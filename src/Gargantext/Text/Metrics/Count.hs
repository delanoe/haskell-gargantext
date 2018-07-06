{-|
Module      : Gargantext.Text.Metrics.Count
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Token and occurrence

An occurrence is not necessarily a token. Considering the sentence:
"A rose is a rose is a rose". We may equally correctly state that there
are eight or three words in the sentence. There are, in fact, three word
types in the sentence: "rose", "is" and "a". There are eight word tokens
in a token copy of the line. The line itself is a type. There are not
eight word types in the line. It contains (as stated) only the three
word types, 'a', 'is' and 'rose', each of which is unique. So what do we
call what there are eight of? They are occurrences of words. There are
three occurrences of the word type 'a', two of 'is' and three of 'rose'.
Source : https://en.wikipedia.org/wiki/Type%E2%80%93token_distinction#Occurrences

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Metrics.Count
  where

import Data.Text (Text)
import Control.Arrow (Arrow(..), (***))
import qualified Data.List as List

import qualified Data.Map.Strict as DMS
import Data.Map.Strict  ( Map, empty, singleton
                        , insertWith, unionWith, unionsWith
                        , mapKeys
                        )
import Data.Set (Set)
import Data.Text (pack)


------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Core.Types
------------------------------------------------------------------------
type Occ  a = Map      a  Int
type Cooc a = Map (a,  a) Int
type FIS  a = Map (Set a) Int

data Group = ByStem | ByOntology

type Grouped = Stems


{-
-- >> let testData = ["blue lagoon", "blues lagoon", "red lagoon"]
-- >> map occurrences <$> Prelude.mapM (terms Mono EN) 
-- [fromList [(fromList ["blue"],1),(fromList ["lagoon"],1)],fromList [(fromList ["blue"],1),(fromList ["lagoon"],1)],fromList [(fromList ["lagoon"],1),(fromList ["red"],1)]]
--λ:   cooc <$> Prelude.map occurrences <$> Prelude.mapM (terms Mono EN) ["blue lagoon", "blues lagoon", "red lagoon"]
--fromList [((fromList ["blue"],fromList ["lagoon"]),2),((fromList ["lagoon"],fromList ["red"]),1)]
--λ:   cooc <$> Prelude.map occurrences <$> Prelude.mapM (terms Mono EN) ["blue lagoon", "blues lagoon", "red lagoon", "red lagoon"]
--fromList [((fromList ["blue"],fromList ["lagoon"]),2),((fromList ["lagoon"],fromList ["red"]),2)]
--λ:   cooc <$> Prelude.map occurrences <$> Prelude.mapM (terms Mono EN) ["blue lagoon", "blues lagoon", "red lagoon red lagoon", "red lagoon"]
--fromList [((fromList ["blue"],fromList ["lagoon"]),2),((fromList ["lagoon"],fromList ["red"]),2)]
--λ:   cooc <$> Prelude.map occurrences <$> Prelude.mapM (terms Mono EN) ["blue lagoon", "blues lagoon blues lagoon", "red lagoon red lagoon", "red lagoon"]
--fromList [((fromList ["blue"],fromList ["lagoon"]),2),((fromList ["lagoon"],fromList ["red"]),2)]
---- 
           -}

type Occs      = Int
type Coocs     = Int
type Threshold = Int

removeApax :: Threshold -> Map (Label, Label) Int -> Map (Label, Label) Int
removeApax t = DMS.filter (> t)

cooc :: [[Terms]] -> Map (Label, Label) Int
cooc tss = coocOnWithLabel _terms_stem (useLabelPolicy label_policy) tss
  where
    terms_occs = occurrencesOn _terms_stem (List.concat tss)
    label_policy = mkLabelPolicy terms_occs


coocOnWithLabel :: (Ord label, Ord b) => (a -> b) -> (b -> label)
                                      -> [[a]] -> Map (label, label) Coocs
coocOnWithLabel on' policy tss = mapKeys (delta policy) $ coocOn on' tss
  where
    delta :: Arrow a => a b' c' -> a (b', b') (c', c')
    delta f = f *** f


mkLabelPolicy :: Map Grouped (Map Terms Occs) -> Map Grouped Label
mkLabelPolicy = DMS.map f where
  f = _terms_label . fst . maximumWith snd . DMS.toList
     -- TODO use the Foldable instance of Map instead of building a list

useLabelPolicy :: Map Grouped Label -> Grouped -> Label
useLabelPolicy m g = case DMS.lookup g m of
  Just label -> label
  Nothing    -> panic $ "Label of Grouped not found: " <> (pack $ show g)
{-
labelPolicy :: Map Grouped (Map Terms Occs) -> Grouped -> Label
labelPolicy m g =  case _terms_label <$> fst <$> maximumWith snd <$> DMS.toList <$> lookup g m of
                     Just label -> label
                     Nothing    -> panic $ "Label of Grouped not found: " <> (pack $ show g)
-}

coocOn :: Ord b => (a -> b) -> [[a]] -> Map (b, b) Coocs
coocOn f as = DMS.unionsWith (+) $ map (coocOn' f) as

coocOn' :: Ord b => (a -> b) -> [a] -> Map (b, b) Coocs
coocOn' fun ts = DMS.fromListWith (+) xs
  where
      ts' = List.nub $ map fun ts
      xs = [ ((x, y), 1)
           | x <- ts'
           , y <- ts'
           , x >= y
           ]

coocOnContexts :: (a -> [Text]) -> [[a]] -> Map ([Text], [Text]) Int
coocOnContexts fun = DMS.fromListWith (+) . List.concat . map (coocOnSingleContext fun)

coocOnSingleContext :: (a -> [Text]) -> [a] -> [(([Text], [Text]), Int)]
coocOnSingleContext fun ts = xs
  where
      ts' = List.nub $ map fun ts
      xs = [ ((x, y), 1)
           | x <- ts'
           , y <- ts'
           , x >= y
           ]


-- | Compute the grouped occurrences (occ)
occurrences :: [Terms] -> Map Grouped (Map Terms Int)
occurrences = occurrencesOn _terms_stem

occurrencesOn :: (Ord a, Ord b) => (a -> b) -> [a] -> Map b (Map a Int)
occurrencesOn f = foldl' (\m a -> insertWith (unionWith (+)) (f a) (singleton a 1) m) empty

-- TODO add groups and filter stops

sumOcc :: Ord a => [Occ a] -> Occ a
sumOcc xs = unionsWith (+) xs


