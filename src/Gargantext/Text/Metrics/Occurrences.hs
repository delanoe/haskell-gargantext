{-|
Module      : Gargantext.Text.Metrics.Occurrences
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

module Gargantext.Text.Metrics.Occurrences
  where


import Data.Map.Strict  (Map
                        , empty
                        , insertWith, insertWithKey, unionWith
                        , toList
                        )

import qualified Data.Map.Strict as DMS
import Control.Monad ((>>),(>>=))
import Data.String (String())
import Data.Attoparsec.Text

------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Core.Types

------------------------------------------------------------------------

type Occ  a = Map      a  Int
type Cooc a = Map (a,  a) Int
type FIS  a = Map (Set a) Int

data Group = ByStem | ByOntology

type Grouped = Stems


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


cooc :: (Ord b, Num a) => [Map b a] -> Map (b, b) a
cooc ts = cooc' $ map cooc'' ts

cooc' :: (Ord b, Num a) => [Map (b, b) a] -> Map (b,b) a
cooc' = foldl' (\x y -> unionWith (+) x y) empty

cooc'' :: (Ord b, Num a) => Map b a -> Map (b, b) a
cooc'' m = foldl' (\x (y,c) -> insertWith (+) y c x) empty xs
  where
      xs =[ ((x'',y''), c') | x' <- toList m
                            , y' <- toList m
                            , let x'' = fst x'
                            , let y'' = fst y'
                            , x'' < y''
                            , let c' = 1
                            --, let c' = snd x' + snd y'
                            ]


-- | Compute the grouped occurrences (occ)
occurrences :: [Terms] -> Map Grouped Int
occurrences = occurrences' _terms_stem

occurrences' :: Ord b => (a -> b) -> [a] -> Occ b
occurrences' f xs = foldl' (\x y -> insertWith (+) (f y) 1 x) empty xs


-- TODO add groups and filter stops
sumOcc :: Ord a => [Occ a] -> Occ a
sumOcc xs = foldl' (unionWith (+)) empty xs


