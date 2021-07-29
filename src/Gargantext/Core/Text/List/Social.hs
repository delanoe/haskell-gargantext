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

import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Monoid (mconcat)
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

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Main parameters

-- | FlowSocialListPriority
-- Sociological assumption: either private or others (public) first
-- This parameter depends on the user choice
data FlowSocialListPriority = MySelfFirst | OthersFirst

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
                  => FlowSocialListPriority
                  -> User -> NgramsType
                  -> FlowCont NgramsTerm FlowListScores
                  -> m (FlowCont NgramsTerm FlowListScores)
flowSocialList flowPriority user nt flc =
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
        getHistoryScores History_User nt'' flc'' listes
        {-
            mapM (\l -> getListNgrams [l] nt'') listes
        >>= pure
          . toFlowListScores (keepAllParents nt'') flc''
        -}
-----------------------------------------------------------------
getHistoryScores :: ( HasNodeStory env err m
                    , CmdM         env err m
                    , HasNodeError     err
                    , HasTreeError     err
                    )
                 => History
                 -> NgramsType
                 -> FlowCont NgramsTerm FlowListScores
                 -> [ListId]
                 -> m (FlowCont NgramsTerm FlowListScores)
getHistoryScores hist nt fl listes =
  addScorePatches nt listes fl <$> getHistory hist nt listes

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

