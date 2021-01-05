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
{-# LANGUAGE TemplateHaskell      #-}

module Gargantext.Core.Types ( module Gargantext.Core.Types.Main
                             , module Gargantext.Database.Admin.Types.Node
                             , Term, Terms(..)
                             , TokenTag(..), POS(..), NER(..)
                             , Label, Stems
                             , HasInvalidError(..), assertValid
                             , Name
                             , TableResult(..), NodeTableResult
                             , Ordering(..), randomOrdering, randomBool, genWith
                             , TODO(..)
                             ) where

import Control.Lens (Prism', (#))
import Control.Monad.Except (MonadError(throwError))
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Set (Set, empty)
import Data.Swagger (ToParamSchema)
import Data.Swagger (ToSchema(..))
import Data.Text (Text, unpack)
import Data.Validity
import GHC.Generics
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import System.Random
import Prelude (fromEnum, toEnum)
import Gargantext.Core.Types.Main
import Gargantext.Database.Admin.Types.Node
import Gargantext.Core.Utils.Prefix (unPrefix, wellNamedSchema)
import Gargantext.Prelude

------------------------------------------------------------------------
data Ordering = Down | Up
  deriving (Enum, Show, Eq, Bounded)

------------------------------------------------------------------------
-- Random work (WIP)
-- TODO mv in Prelude.Random

instance Random Ordering where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g


type Seed = Int

{- | Crypto work
TODO XOR to share secret
-}
randomOrdering :: Maybe Seed -> Int -> IO [Ordering]
randomOrdering = randomWith

randomBool :: Maybe Seed -> Int -> IO [Bool]
randomBool= randomWith

randomWith :: Random a => Maybe Seed -> Int -> IO [a]
randomWith seed n = do
  g <- case seed of
    Nothing -> newStdGen
    Just  s -> pure $ mkStdGen s

  pure $ take n $ (randoms g)

newtype PrivateSeed  = PrivateSeed Int
newtype PublicSeed = PublicSeed Int

genWith :: PrivateSeed -> PublicSeed -> Int -> IO [Bool]
genWith (PrivateSeed x) (PublicSeed o) n = do
  xs <- randomBool (Just  x) n
  ys <- randomBool (Just  o) n
  pure $ zipWith xor xs ys

{-
searchSeeds :: Int -> IO [Int]
searchSeeds xs = mapM (\n -> randomWith (Just n) l) [1..]
  where
    l = length xs
-}

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

data TokenTag  = TokenTag { _my_token_word  :: [Text]
                          , _my_token_lemma :: Set Text
                          , _my_token_pos   :: Maybe POS
                          , _my_token_ner   :: Maybe NER
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
  mconcat = foldl mappend mempty
  -- mappend t1 t2 = (<>) t1 t2


class HasInvalidError e where
  _InvalidError :: Prism' e Validation

assertValid :: (MonadError e m, HasInvalidError e) => Validation -> m ()
assertValid v = when (not $ validationIsValid v) $ throwError $ _InvalidError # v
-- assertValid :: MonadBase IO m => Validation -> m ()
-- assertValid v = when (not $ validationIsValid v) $ fail $ show v

-- | NodeTableResult (Table computations)
type NodeTableResult a = TableResult (Node a)


data TableResult a = TableResult { tr_count :: Int
                                 , tr_docs :: [a]
                                 } deriving (Generic)

$(deriveJSON (unPrefix "tr_") ''TableResult)

instance (Typeable a, ToSchema a) => ToSchema (TableResult a) where
  declareNamedSchema = wellNamedSchema "tr_"

instance Arbitrary a => Arbitrary (TableResult a) where
  arbitrary = TableResult <$> arbitrary <*> arbitrary


-- TO BE removed
data TODO = TODO
  deriving (Generic)

instance ToSchema TODO where
instance ToParamSchema TODO where

----------------------------------------------------------------------------


