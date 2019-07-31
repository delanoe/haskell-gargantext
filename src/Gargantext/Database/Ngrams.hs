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

module Gargantext.Database.Ngrams
    where

import Data.Text (Text)
import Gargantext.Core.Types
import Gargantext.Database.Utils (runOpaQuery, Cmd)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.NodeNodeNgrams
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Opaleye
import Control.Arrow (returnA)

selectNgramsByDoc :: [CorpusId] -> DocId -> NgramsType -> Cmd err [Text]
selectNgramsByDoc cIds dId nt = runOpaQuery (query cIds dId nt)
  where
    
    join :: Query (NgramsRead, NodeNodeNgramsReadNull)
    join = leftJoin queryNgramsTable queryNodeNodeNgramsTable on1
      where
        on1 (ng,nnng) = ngrams_id ng .== nnng_ngrams_id nnng

    query cIds' dId' nt' = proc () -> do
      (ng,nnng) <- join -< ()
      restrict -< foldl (\b cId -> ((toNullable $ pgNodeId cId) .== nnng_node1_id nnng) .|| b) (pgBool True) cIds'
      restrict -< (toNullable $ pgNodeId dId') .== nnng_node2_id nnng
      restrict -< (toNullable $ pgNgramsType nt') .== nnng_ngramsType nnng
      returnA -< ngrams_terms ng


postNgrams :: CorpusId -> DocId -> [Text] -> Cmd err Int
postNgrams = undefined

