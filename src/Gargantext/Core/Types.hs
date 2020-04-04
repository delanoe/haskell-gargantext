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
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell      #-}

module Gargantext.Core.Types ( module Gargantext.Core.Types.Main
                             , module Gargantext.Database.Types.Node
                             , Term, Terms(..)
                             , TokenTag(..), POS(..), NER(..)
                             , Label, Stems
                             , HasInvalidError(..), assertValid
                             , Name
                             , TableResult(..)
                             , NodeTableResult
                             , TODO(..)
                             ) where

--import qualified Data.Set as S
import Control.Lens (Prism', (#))
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Monoid
import Data.Semigroup
import Data.Set (Set, empty)
import Data.Swagger (ToParamSchema)
import Data.Swagger (ToSchema(..), genericDeclareNamedSchema)
import Data.Text (Text, unpack)
import Data.Validity
import GHC.Generics
import Gargantext.Core.Types.Main
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Types.Node
import Gargantext.Prelude
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------
------------------------------------------------------------------------
type Name = Text
type Term  = Text
type Stems = Set Text
type Label = [Text]

data Terms = Terms { _terms_label :: Label
                   , _terms_stem  :: Stems
                   } deriving (Ord)

instance Show Terms where
  show (Terms l _) = show l

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

instance Semigroup TokenTag where
  (<>) (TokenTag w1 s1 p1 n1) (TokenTag w2 s2 p2 _) = TokenTag (w1 <> w2) (s1 <> s2) p3 n1
    where
      p3 = case (p1,p2) of
        (Just JJ, Just NP) -> Just NP
        (Just VB, Just NP) -> Just NP
        _                  -> p1


instance Monoid TokenTag where
  mempty = TokenTag [] empty Nothing Nothing

  mappend t1 t2 = (<>) t1 t2

  mconcat = foldl mappend mempty


class HasInvalidError e where
  _InvalidError :: Prism' e Validation

assertValid :: (MonadError e m, HasInvalidError e) => Validation -> m ()
assertValid v = when (not $ validationIsValid v) $ throwError $ _InvalidError # v
-- assertValid :: MonadIO m => Validation -> m ()
-- assertValid v = when (not $ validationIsValid v) $ fail $ show v


data TableResult a = TableResult { tr_count :: Int
                                 , tr_docs :: [a]
                                 } deriving (Generic)

$(deriveJSON (unPrefix "tr_") ''TableResult)

instance ToSchema a => ToSchema (TableResult a) where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "tr_")

instance Arbitrary a => Arbitrary (TableResult a) where
  arbitrary = TableResult <$> arbitrary <*> arbitrary

type NodeTableResult a = TableResult (Node a)

-- TO BE removed
data TODO = TODO
  deriving (Generic)

instance ToSchema TODO where
instance ToParamSchema TODO where



