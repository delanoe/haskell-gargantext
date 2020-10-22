{-|
Module      : Gargantext.API.Ngrams.NTree
Description : Tree of Ngrams
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.API.Ngrams.NTree
  where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Data.Tree
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Data.Set (Set)
import Data.Swagger
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import GHC.Generics (Generic)
import Test.QuickCheck

import Gargantext.Prelude

import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types (ListType(..), NodeId)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)

type Children = Text
type Root = Text

data MyTree = MyTree { mt_label :: Text
                     , mt_value :: Double
                     , mt_children :: [MyTree]
                  } deriving (Generic, Show)

toMyTree :: Tree (Text,Double) -> MyTree
toMyTree (Node (l,v) xs) = MyTree l v (map toMyTree xs)

deriveJSON (unPrefix "mt_") ''MyTree

instance ToSchema MyTree where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "mt_")
instance Arbitrary MyTree
  where
    arbitrary = MyTree <$> arbitrary <*> arbitrary <*> arbitrary

toTree :: ListType -> Map Text (Set NodeId) -> Map Text NgramsRepoElement -> [MyTree]
toTree lt vs m = map toMyTree $ unfoldForest buildNode roots
  where
    buildNode r = maybe ((r, value r),[])
                        (\x -> ((r, value r), unNgramsTerm <$> (mSetToList $ _nre_children x)))
                        (Map.lookup r m)

    value l = maybe 0 (fromIntegral . Set.size) $ Map.lookup l vs

    rootsCandidates :: [NgramsTerm]
    rootsCandidates = catMaybes
                    $ List.nub
                    $ map (\(c, c') -> case _nre_root c' of
                                       Nothing -> Just $ NgramsTerm c
                                       _ -> _nre_root c') (Map.toList m)

    roots = map fst
          $ filter (\(_,l) -> l == lt)
          $ catMaybes
          $ map (\c -> (,) <$> Just c <*> (_nre_list <$> Map.lookup c m))
          $ (unNgramsTerm <$> rootsCandidates)
