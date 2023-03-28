{-|
Module      : Gargantext.Core.Text.Ngrams.List.Management
Description : Tools to manage lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Gargantext.Core.Text.List.Management
  where

{-
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Gargantext.API.Ngrams
import Gargantext.API.Ngrams.Types (NgramsElement, NgramsTerm(..))
import Gargantext.Database.Action.Flow.Types
import Gargantext.API.Ngrams.Tools (getListNgrams)
import Gargantext.Core.NodeStory
import Gargantext.Core.Text (size)
import Gargantext.Core.Text.List.Group
import Gargantext.Core.Text.List.Group.Prelude
import Gargantext.Core.Text.List.Group.WithStem
import Gargantext.Core.Text.List.Social
import Gargantext.Core.Text.List.Social.Prelude
import Gargantext.Core.Text.Metrics (scored', Scored(..), scored_speExc, scored_genInc, normalizeGlobal, normalizeLocal, scored_terms)
import Gargantext.Core.Types (ListType(..), CorpusId, ListId)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Metrics.NgramsByContext (getContextsByNgramsUser, getContextsByNgramsOnlyUser)
import Gargantext.Database.Action.Metrics.TFICF (getTficf_withSample)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Database.Prelude (CmdM)
import Gargantext.Database.Query.Table.Ngrams (text2ngrams)
import Gargantext.Database.Query.Table.NgramsPostag (selectLems)
import Gargantext.Database.Query.Table.Node (defaultList, getClosestParentIdByType)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError())
import Gargantext.Database.Query.Tree.Error (HasTreeError)
import Gargantext.Database.Action.Metrics.NgramsByContext (getOccByNgramsOnlyFast')
import Gargantext.Database.Schema.Ngrams (NgramsType(..), Ngrams(..))
import Gargantext.Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List    as List
import qualified Data.Map     as Map
import qualified Data.Set     as Set
import qualified Gargantext.Data.HashMap.Strict.Utils as HashMap




restrictListSize
  :: forall env err m.
     (HasNodeStory env err m, FlowCmdM env err m)
  => CorpusId
  -> ListId
  -> NgramsType
  -> ListType
  -> Int -- ^ number of ngram pairs to keep
  -> m ()
restrictListSize corpusId listId ngramsType listType size = do
  ngrams <- getListNgrams [listId] ngramsType
  -- corpus_id <- getClosestParentIdByType
  occurrences <- getOccByNgramsOnlyFast' corpusId
                                         listId
                                         ngramsType
                                         (HashMap.keys ngrams)

  ngrams' <- filterWith listType size occurrences ngrams

  _ <- setListNgrams listId ngramsType ngrams'
  return ()

  where filterWith :: ListType -> Int -> HashMap NgramsTerm Int
                   -> HashMap NgramsTerm NgramsRepoElement
                   -> m (Map NgramsTerm NgramsRepoElement)
        filterWith listType' size occs ngrams =
          HashMap.filter with ngrams
          where
            with nre = case (&&) <$> Just (nre^.nre_list == listType)
                                 <*> ( HashMap.lookup (nre^.nre_root) occs
                              &&






-}
