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

import Data.Monoid (mconcat)
import Data.Text (Text)
import Gargantext.API.Ngrams.Tools
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Text.List.Social.Find
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.List.Social.Scores
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
flowSocialListPriority MySelfFirst = [Private, Shared{-, Public -}]
flowSocialListPriority OthersFirst = reverse $ flowSocialListPriority MySelfFirst


-- | We keep the parents for all ngrams but terms
keepAllParents :: NgramsType -> KeepAllParents
keepAllParents NgramsTerms = KeepAllParents False
keepAllParents _           = KeepAllParents True

------------------------------------------------------------------------
flowSocialList' :: ( RepoCmdM env err m
                   , CmdM     env err m
                   , HasNodeError err
                   , HasTreeError err
                   )
                  => FlowSocialListPriority
                  -> User -> NgramsType
                  -> FlowCont Text FlowListScores
                  -> m (FlowCont Text FlowListScores)
flowSocialList' flowPriority user nt flc =
  mconcat <$> mapM (flowSocialListByMode'   user nt flc)
                   (flowSocialListPriority flowPriority)
    where

      flowSocialListByMode' :: ( RepoCmdM env err m
                               , CmdM     env err m
                               , HasNodeError err
                               , HasTreeError err
                               )
                            => User -> NgramsType
                            -> FlowCont Text FlowListScores
                            -> NodeMode
                            -> m (FlowCont Text FlowListScores)
      flowSocialListByMode' user' nt' flc' mode =
            findListsId user' mode
        >>= flowSocialListByModeWith nt' flc'


      flowSocialListByModeWith :: ( RepoCmdM env err m
                                  , CmdM     env err m
                                  , HasNodeError err
                                  , HasTreeError err
                                  )
                               => NgramsType
                               -> FlowCont Text FlowListScores
                               -> [NodeId]
                               -> m (FlowCont Text FlowListScores)
      flowSocialListByModeWith nt'' flc'' ns =
            mapM (\l -> getListNgrams [l] nt'') ns
        >>= pure
          . toFlowListScores (keepAllParents nt'') flc''
