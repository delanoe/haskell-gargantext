{-|
Module      : Gargantext.Database.Flow.Utils
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Flow.Utils
    where

import Data.Map (Map)
import qualified Data.Map as DM
import Gargantext.Prelude
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Types.Node (NodeId, Node, NodePoly(..))
import Gargantext.Database.Utils (Cmd)
import Gargantext.Database.Schema.NodeNgram
import Gargantext.Core.Types.Main (ListType(..), listTypeId)

toMaps :: (a -> Map (NgramsT Ngrams) Int) -> [Node a] -> Map (NgramsT Ngrams) (Map NodeId Int)
toMaps fun ns = mapNodeIdNgrams $ documentIdWithNgrams fun ns'
  where
    ns' = map (\(Node nId _ _ _ _ _ json) -> DocumentWithId nId json) ns

mapNodeIdNgrams :: [DocumentIdWithNgrams a] -> Map (NgramsT Ngrams) (Map NodeId Int)
mapNodeIdNgrams ds = DM.map (DM.fromListWith (+)) $ DM.fromListWith (<>) xs
  where
    xs  = [(ng, [(nId, i)]) | (nId, n2i') <- n2i ds, (ng, i) <- DM.toList n2i']
    n2i = map (\d -> ((documentId . documentWithId) d, document_ngrams d))


documentIdWithNgrams :: (a -> Map (NgramsT Ngrams) Int)
                                     -> [DocumentWithId a] -> [DocumentIdWithNgrams a]
documentIdWithNgrams f = map (\d -> DocumentIdWithNgrams d ((f . documentData) d))


data DocumentWithId a =
     DocumentWithId { documentId   :: NodeId
                    , documentData :: a
                    } deriving (Show)


data DocumentIdWithNgrams a =
     DocumentIdWithNgrams
     { documentWithId  :: DocumentWithId a
     , document_ngrams :: Map (NgramsT Ngrams) Int
     } deriving (Show)

-- | TODO for now, list Type is CandidateTerm because Graph Terms
-- have to be detected in next step in the flow
insertToNodeNgrams :: Map NgramsIndexed (Map NgramsType (Map NodeId Int)) -> Cmd err Int
insertToNodeNgrams m = insertNodeNgrams [ NodeNgram n (_ngramsId ng) Nothing (ngramsTypeId t) (listTypeId CandidateTerm) (fromIntegral i)
                                        | (ng, t2n2i) <- DM.toList m
                                        , (t,  n2i)   <- DM.toList t2n2i
                                        , (n,  i)     <- DM.toList n2i
                                        ]

