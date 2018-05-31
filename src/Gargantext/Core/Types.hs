{-|
Module      : Gargantext.Types
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}

module Gargantext.Core.Types ( module Gargantext.Core.Types.Main
                             , module Gargantext.Core.Types.Node
                             , Term, Terms(..)
                             , TokenTag(..), POS(..), NER(..)
                             , Label, Stems
                             ) where

import GHC.Generics
import Data.Aeson
import Data.Monoid
import Data.Set (Set, empty)
--import qualified Data.Set as S

import Data.Text (Text, unpack)

import Gargantext.Core.Types.Main
import Gargantext.Core.Types.Node
import Gargantext.Prelude

------------------------------------------------------------------------

type Term  = Text
type Stems = Set Text
type Label = [Text]

data Terms = Terms { _terms_label :: Label
                   , _terms_stem  :: Stems
                   } deriving (Ord)

instance Show Terms where
  show (Terms l s) = show l
-- class Inclusion where include
--instance Eq Terms where
--  (==) (Terms _ s1) (Terms _ s2) = s1 `S.isSubsetOf` s2
--                                || s2 `S.isSubsetOf` s1

instance Eq Terms where
  (==) (Terms _ s1) (Terms _ s2) = s1 == s2


------------------------------------------------------------------------
data Tag = POS | NER
  deriving (Show, Eq)
------------------------------------------------------------------------
data POS = NP
         | JJ  | VB
         | CC  | IN | DT
         | NoPos
  deriving (Show, Generic, Eq)
------------------------------------------------------------------------
instance FromJSON POS where
  parseJSON = withText "String" (\x -> pure (pos $ unpack x))
    where
      pos :: [Char] -> POS
      pos "NP"  = NP
      pos "NN"  = NP
      pos "NC"  = NP
      pos "NNS" = NP
      pos "NNP" = NP
      pos "JJ"  = JJ
      pos "ADJ" = JJ
      pos "VB"  = VB
      pos "VBN" = VB
      pos "VBG" = VB
      pos "CC"  = CC
      pos "IN"  = IN
      pos "DT"  = DT
      -- French specific
      pos "P"  = IN
      pos  _    = NoPos

instance ToJSON POS
------------------------------------------------------------------------
data NER = PERSON | ORGANIZATION | LOCATION | NoNER
  deriving (Show, Generic)
------------------------------------------------------------------------
instance FromJSON NER where
  parseJSON = withText "String" (\x -> pure (ner $ unpack x))
    where
      ner :: [Char] -> NER
      ner "PERSON"       = PERSON
      ner "ORGANIZATION" = ORGANIZATION
      ner "LOCATION"     = LOCATION
      ner  _             = NoNER

instance ToJSON NER

data TokenTag  = TokenTag { _my_token_word :: [Text]
                          , _my_token_stem :: Set Text
                          , _my_token_pos  :: Maybe POS
                          , _my_token_ner  :: Maybe NER
                          } deriving (Show)

instance Monoid TokenTag where
  mempty = TokenTag [] empty Nothing Nothing

  mappend (TokenTag w1 s1 p1 n1) (TokenTag w2 s2 p2 _)
         = TokenTag (w1 <> w2) (s1 <> s2) p3 n1
          where
            p3 = case (p1,p2) of
                   (Just JJ, Just NP) -> Just NP
                   (Just VB, Just NP) -> Just NP
                   _                  -> p1

  mconcat = foldl mappend mempty

