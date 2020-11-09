{-|
Module      : Gargantext.Core.Text.List.Social
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social
  where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.API.Ngrams.Tools -- (getListNgrams)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Text.List.Social.Find
import Gargantext.Core.Text.List.Social.ListType
import Gargantext.Core.Text.List.Social.Group
import Gargantext.Core.Types.Individu
import Gargantext.Core.Types.Main
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree
import Gargantext.Database.Schema.Ngrams
import Gargantext.Prelude
import qualified Data.Map   as Map
import qualified Data.Set   as Set

------------------------------------------------------------------------
flowSocialList :: ( RepoCmdM env err m
                  , CmdM     env err m
                  , HasNodeError err
                  , HasTreeError err
                  )
                  => User -> NgramsType -> Set Text
                  -> m (Map ListType (Set Text))
flowSocialList user nt ngrams' = do
  -- Here preference to privateLists (discutable: let user choice)
  privateListIds <- findListsId user Private
  privateLists <- flowSocialListByMode privateListIds nt ngrams'
  -- printDebug "* privateLists *: \n" privateLists

  sharedListIds <- findListsId user Shared
  sharedLists  <- flowSocialListByMode sharedListIds nt (termsByList CandidateTerm privateLists)
  -- printDebug "* sharedLists *: \n" sharedLists

  -- TODO publicMapList:
  -- Note: if both produce 3 identic repetition => refactor mode
  -- publicListIds <- findListsId Public user
  -- publicLists   <- flowSocialListByMode' publicListIds nt (termsByList CandidateTerm privateLists)

  let result = parentUnionsExcl
             [ Map.mapKeys (fromMaybe CandidateTerm) privateLists
             , Map.mapKeys (fromMaybe CandidateTerm) sharedLists
             -- , Map.mapKeys (fromMaybe CandidateTerm) publicLists
             ]
  -- printDebug "* socialLists *: results \n" result
  pure result

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | FlowSocialListPriority
-- Sociological assumption: either private or others (public) first
-- This parameter depends on the user choice
data FlowSocialListPriority = MySelfFirst | OthersFirst

flowSocialListPriority :: FlowSocialListPriority -> [NodeMode]
flowSocialListPriority MySelfFirst = [Private, Shared{-, Public -}]
flowSocialListPriority OthersFirst = reverse $ flowSocialListPriority MySelfFirst

------------------------------------------------------------------------
flowSocialList' :: ( RepoCmdM env err m
                   , CmdM     env err m
                   , HasNodeError err
                   , HasTreeError err
                   )
                  => FlowSocialListPriority
                  -> User -> NgramsType -> Set Text
                  -> m (Map Text FlowListScores)
flowSocialList' flowPriority user nt ngrams' =
  parentUnionsExcl <$> mapM (flowSocialListByMode' user nt ngrams')
                            (flowSocialListPriority flowPriority)

------------------------------------------------------------------------
flowSocialListByMode :: ( RepoCmdM env err m
                       , CmdM     env err m
                       , HasNodeError err
                       , HasTreeError err
                       )
                    => [NodeId]-> NgramsType -> Set Text
                    -> m (Map (Maybe ListType) (Set Text))
flowSocialListByMode      [] _nt ngrams' = pure $ Map.fromList [(Nothing, ngrams')]
flowSocialListByMode listIds  nt ngrams' = do
    counts  <- countFilterList ngrams' nt listIds Map.empty
    let r = toSocialList counts ngrams'
    pure r


flowSocialListByMode' :: ( RepoCmdM env err m
                         , CmdM     env err m
                         , HasNodeError err
                         , HasTreeError err
                         )
                      => User -> NgramsType -> Set Text -> NodeMode 
                      -> m (Map Text FlowListScores)
flowSocialListByMode' user nt st mode =
      findListsId user mode
  >>= flowSocialListByModeWith nt st


flowSocialListByModeWith :: ( RepoCmdM env err m
                         , CmdM     env err m
                         , HasNodeError err
                         , HasTreeError err
                         )
                      => NgramsType -> Set Text -> [NodeId]
                      -> m (Map Text FlowListScores)
flowSocialListByModeWith nt st ns =
      mapM (\l -> getListNgrams [l] nt) ns
  >>= pure
    . toFlowListScores (keepAllParents nt) st Map.empty


-- | We keep the parents for all ngrams but terms
keepAllParents :: NgramsType -> KeepAllParents
keepAllParents NgramsTerms = KeepAllParents False
keepAllParents _           = KeepAllParents True

------------------------------------------------------------------------
-- TODO: maybe use social groups too
-- | TODO what if equality ?
-- choice depends on Ord instance of ListType
-- for now : data ListType  =  StopTerm | CandidateTerm | MapTerm
-- means MapTerm > CandidateTerm > StopTerm in case of equality of counts
-- (we minimize errors on MapTerms if doubt)
toSocialList :: Map Text (Map ListType Int)
             -> Set Text
             -> Map (Maybe ListType) (Set Text)
toSocialList m = Map.fromListWith (<>)
               . Set.toList
               . Set.map (toSocialList1 m)

toSocialList1 :: Map Text (Map ListType Int)
             -> Text
             -> (Maybe ListType, Set Text)
toSocialList1 m t = case Map.lookup t m of
  Nothing -> (Nothing, Set.singleton t)
  Just  m' -> ( (fst . fst) <$> Map.maxViewWithKey m'
              , Set.singleton t
              )

toSocialList1_testIsTrue :: Bool
toSocialList1_testIsTrue = result == (Just MapTerm, Set.singleton token)
  where
    result = toSocialList1 (Map.fromList [(token, m)]) token
    token  = "token"
    m = Map.fromList [ (CandidateTerm, 1)
                     , (MapTerm      , 2)
                     , (StopTerm     , 3)
                     ]

------------------------------------------------------------------------
-- | Tools
------------------------------------------------------------------------
termsByList :: ListType -> (Map (Maybe ListType) (Set Text)) -> Set Text
termsByList CandidateTerm m = Set.unions
                          $ map (\lt -> fromMaybe Set.empty $ Map.lookup lt m)
                          [ Nothing, Just CandidateTerm ]
termsByList l m =
  fromMaybe Set.empty $ Map.lookup (Just l) m

------------------------------------------------------------------------
unions :: (Ord a, Semigroup a, Semigroup b, Ord b)
      => [Map a (Set b)] -> Map a (Set b)
unions = invertBack . Map.unionsWith (<>) . map invertForw

invertForw :: (Ord b, Semigroup a) => Map a (Set b) -> Map b a
invertForw = Map.unionsWith (<>)
           . (map (\(k,sets) -> Map.fromSet (\_ -> k) sets))
           . Map.toList

invertBack :: (Ord a, Ord b) => Map b a -> Map a (Set b)
invertBack = Map.fromListWith (<>)
           . (map (\(b,a) -> (a, Set.singleton b)))
           .  Map.toList

unions_test :: Map ListType (Set Text)
unions_test = unions [m1, m2]
  where
    m1 = Map.fromList [ (StopTerm     , Set.singleton "Candidate")]
    m2 = Map.fromList [ (CandidateTerm, Set.singleton "Candidate")
                      , (MapTerm      , Set.singleton "Candidate")
                      ]
