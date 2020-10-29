{-|
Module      : Gargantext.Core.Text.List.Social.Group
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social.Group
  where

-- findList imports
import Gargantext.Core.Types.Individu
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Prelude

-- filterList imports
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Set (Set)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Gargantext.API.Ngrams.Tools -- (getListNgrams)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main
import Gargantext.Database.Schema.Ngrams
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Set   as Set

----------------------
-- | Tools to inherit groupings
----------------------
type Parent = Text

parentUnionsMerge :: (Ord a, Ord b, Num c)
                   => [Map a (Map b c)]
                   ->  Map a (Map b c)
parentUnionsMerge = Map.unionsWith (Map.unionWith (+))

-- This Parent union is specific
-- [Private, Shared, Public]
-- means the following preferences:
-- Private > Shared > Public
-- if data have not been tagged privately, then use others tags
-- This unions behavior takes first key only and ignore others
parentUnionsExcl :: Ord a
                 => [Map a b]
                 ->  Map a b
parentUnionsExcl = Map.unions


hasParent :: Text
          -> Map Text (Map Parent Int)
          -> Maybe Parent
hasParent t m = case Map.lookup t m of
  Nothing  -> Nothing
  Just  m' -> (fst . fst) <$> Map.maxViewWithKey m'


toMapTextParent :: Set Text
                ->  Map Text (Map Parent Int)
                -> [Map Text NgramsRepoElement]
                -> Map Text (Map Parent Int)
toMapTextParent ts = foldl' (toMapTextParent' ts)
  where

    toMapTextParent' :: Set Text
                    -> Map Text (Map Parent Int)
                    -> Map Text NgramsRepoElement
                    -> Map Text (Map Parent Int)
    toMapTextParent' ts' to from = Set.foldl' (toMapTextParent'' ts' from) to ts'


    toMapTextParent'' :: Set Text
                      -> Map Text NgramsRepoElement
                      -> Map Text (Map Parent Int)
                      -> Text
                      -> Map Text (Map Parent Int)
    toMapTextParent'' ss from to t = case Map.lookup t from of
      Nothing  -> to
      Just nre -> case _nre_parent nre of
        Just (NgramsTerm p')  -> if Set.member p' ss
                                    then Map.alter (addParent p') t to
                                    else to
          where
            addParent p'' Nothing   = Just $ addCountParent p'' Map.empty
            addParent p'' (Just ps) = Just $ addCountParent p'' ps

            addCountParent :: Parent -> Map Parent Int -> Map Parent Int
            addCountParent p m = Map.alter addCount p m
              where
                addCount Nothing  = Just 1
                addCount (Just n) = Just $ n + 1

        _ -> to


------------------------------------------------------------------------
