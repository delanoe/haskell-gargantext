{-|
Module      : Gargantext.Core.Text.List.Social
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core.Text.List.Social
  where

import Control.Monad (mzero)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Monoid (mconcat)
import Data.Swagger
import GHC.Generics
import qualified Data.Scientific as Scientific
import qualified Data.Text       as T
import qualified Data.Vector     as V

import Gargantext.API.Ngrams.Tools
import Gargantext.API.Ngrams.Types
import Gargantext.Core.NodeStory
import Gargantext.Core.Text.List.Social.Find
import Gargantext.Core.Text.List.Social.History
import Gargantext.Core.Text.List.Social.Patch
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Types.Individu
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree
import Gargantext.Database.Schema.Ngrams
import Gargantext.Prelude
import qualified Prelude as Prelude

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Main parameters

-- | FlowSocialListPriority
-- Sociological assumption: either private or others (public) first
-- This parameter depends on the user choice

data FlowSocialListWith = FlowSocialListWithPriority { fslw_priority :: FlowSocialListPriority }
                        | FlowSocialListWithLists    { fslw_lists :: [ListId] }
  deriving (Show, Generic)
instance FromJSON FlowSocialListWith where
  parseJSON (Object v) = do
    typ :: T.Text <- v .: "type"
    value <- v .:? "value" .!= []
    case typ of
      "MyListsFirst" -> pure $ FlowSocialListWithPriority { fslw_priority = MySelfFirst }
      "OtherListsFirst" -> pure $ FlowSocialListWithPriority { fslw_priority = OthersFirst }
      "SelectedLists" -> pure $ FlowSocialListWithLists { fslw_lists = value }
      _ -> pure $ FlowSocialListWithPriority { fslw_priority = MySelfFirst }
  parseJSON _ = mzero
instance ToJSON FlowSocialListWith where
  toJSON (FlowSocialListWithPriority { fslw_priority = MySelfFirst }) =
    object [ ("type", String "MyListsFirst") ]
  toJSON (FlowSocialListWithPriority { fslw_priority = OthersFirst }) =
    object [ ("type", String "ListsFirst") ]
  toJSON (FlowSocialListWithLists { fslw_lists = ids }) =
    object [ ("type", String "SelectedLists")
           , ("value", Array $ V.fromList $ (map (\(NodeId id) -> Number $ Scientific.scientific (Prelude.toInteger id) 1) ids)) ]
instance ToSchema FlowSocialListWith where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

data FlowSocialListPriority = MySelfFirst | OthersFirst
  deriving (Show, Generic)
instance ToSchema FlowSocialListPriority where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

flowSocialListPriority :: FlowSocialListPriority -> [NodeMode]
flowSocialListPriority MySelfFirst = [Private{-, Shared, Public -}]
flowSocialListPriority OthersFirst = reverse $ flowSocialListPriority MySelfFirst

{-
-- | We keep the parents for all ngrams but terms
keepAllParents :: NgramsType -> KeepAllParents
keepAllParents NgramsTerms = KeepAllParents False
keepAllParents _           = KeepAllParents True
-}

------------------------------------------------------------------------
flowSocialList :: ( HasNodeStory env err m
                   , CmdM     env err m
                   , HasNodeError err
                   , HasTreeError err
                   )
                  => Maybe FlowSocialListWith
                  -> User
                  -> NgramsType
                  -> FlowCont NgramsTerm FlowListScores
                  -> m (FlowCont NgramsTerm FlowListScores)
flowSocialList Nothing u = flowSocialList' MySelfFirst u
flowSocialList (Just (FlowSocialListWithPriority p)) u = flowSocialList' p u
flowSocialList (Just (FlowSocialListWithLists    ls)) _ = getHistoryScores ls History_User

flowSocialList' :: ( HasNodeStory env err m
                   , CmdM     env err m
                   , HasNodeError err
                   , HasTreeError err
                   )
                  => FlowSocialListPriority
                  -> User -> NgramsType
                  -> FlowCont NgramsTerm FlowListScores
                  -> m (FlowCont NgramsTerm FlowListScores)
flowSocialList' flowPriority user nt flc =
  mconcat <$> mapM (flowSocialListByMode'   user nt flc)
                   (flowSocialListPriority flowPriority)
    where

      flowSocialListByMode' :: ( HasNodeStory env err m
                               , CmdM     env err m
                               , HasNodeError err
                               , HasTreeError err
                               )
                            => User -> NgramsType
                            -> FlowCont NgramsTerm FlowListScores
                            -> NodeMode
                            -> m (FlowCont NgramsTerm FlowListScores)
      flowSocialListByMode' user' nt' flc' mode =
            findListsId user' mode
        >>= flowSocialListByModeWith nt' flc'


      flowSocialListByModeWith :: ( HasNodeStory env err m
                                  , CmdM     env err m
                                  , HasNodeError err
                                  , HasTreeError err
                                  )
                               => NgramsType
                               -> FlowCont NgramsTerm FlowListScores
                               -> [ListId]
                               -> m (FlowCont NgramsTerm FlowListScores)
      flowSocialListByModeWith nt'' flc'' listes =
        getHistoryScores listes History_User nt'' flc''


-----------------------------------------------------------------
getHistoryScores :: ( HasNodeStory env err m
                    , CmdM         env err m
                    , HasNodeError     err
                    , HasTreeError     err
                    )
                 => [ListId]
                 -> History
                 -> NgramsType
                 -> FlowCont NgramsTerm FlowListScores
                 -> m (FlowCont NgramsTerm FlowListScores)
getHistoryScores lists hist nt fl =
  addScorePatches nt lists fl <$> getHistory hist nt lists

getHistory :: ( HasNodeStory env err m
              , CmdM     env err m
              , HasNodeError err
              , HasTreeError err
              )
           => History
           -> NgramsType
           -> [ListId]
           -> m (Map ListId (Map NgramsType [HashMap NgramsTerm NgramsPatch]))
getHistory hist nt listes =
  history hist [nt] listes  <$> getRepo' listes

