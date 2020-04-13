{-|
Module      : Gargantext.Database.Ngrams
Description : Deal with in Gargantext Database.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Action.Query.Ngrams
    where

import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.Text (Text)
import Gargantext.Core.Types
import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Admin.Utils (runOpaQuery, Cmd)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.NodeNodeNgrams
import Gargantext.Prelude
import Opaleye

selectNgramsByDoc :: [ListId] -> DocId -> NgramsType -> Cmd err [Text]
selectNgramsByDoc lIds dId nt = runOpaQuery (query lIds dId nt)
  where

    join :: Query (NgramsRead, NodeNodeNgramsReadNull)
    join = leftJoin queryNgramsTable queryNodeNodeNgramsTable on1
      where
        on1 (ng,nnng) = ng^.ngrams_id .== nnng^.nnng_ngrams_id

    query cIds' dId' nt' = proc () -> do
      (ng,nnng) <- join -< ()
      restrict -< foldl (\b cId -> ((toNullable $ pgNodeId cId) .== nnng^.nnng_node1_id) .|| b) (pgBool True) cIds'
      restrict -< (toNullable $ pgNodeId dId')    .== nnng^.nnng_node2_id
      restrict -< (toNullable $ pgNgramsType nt') .== nnng^.nnng_ngramsType
      returnA  -< ng^.ngrams_terms


postNgrams :: CorpusId -> DocId -> [Text] -> Cmd err Int
postNgrams = undefined

