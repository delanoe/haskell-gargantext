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

import Gargantext.Prelude

import Data.Map.Strict  (Map
                        , empty
                        , insertWith, unionWith
                        )

import Control.Monad ((>>),(>>=))
import Data.String (String())
import Data.Attoparsec.Text
import Data.Text (Text)

import Data.Either.Extra(Either(..))
import qualified Data.Text as T
import Control.Applicative hiding (empty)
-----------------------------------------------------------

type Occ = Int

-- | Compute the occurrences (occ)
occ :: Ord a => [a] -> Map a Occ
occ xs = foldl' (\x y -> insertWith (+) y 1 x) empty xs

-- TODO add groups and filter stops
sumOcc :: Ord a => [Map a Occ] -> Map a Occ
sumOcc xs = foldl' (unionWith (+)) empty xs


occurrenceParser :: Text -> Parser Bool
occurrenceParser txt = manyTill anyChar (string txt) >> pure True

occurrencesParser :: Text -> Parser Int
occurrencesParser txt = case txt of
                    "" -> pure 0
                    _  -> many (occurrenceParser txt') >>= \matches -> pure (length matches)
    where
        txt' = T.toLower txt

parseOccurrences :: Text -> Text -> Either String Int
parseOccurrences x = parseOnly (occurrencesParser x)
