{-|
Module      : Gargantext.Database.Flow.Utils
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Action.Flow.Utils
    where

import Data.Map (Map)
import qualified Data.Map as DM
import Gargantext.Prelude
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Types.Node (NodeId, Node, NodePoly(..), Hyperdata)
import Gargantext.Database.Utils (Cmd)
import Gargantext.Database.Schema.NodeNodeNgrams
import Gargantext.Database.Types.Node

toMaps :: Hyperdata a
       => (a -> Map (NgramsT Ngrams) Int)
       -> [Node a]
       -> Map (NgramsT Ngrams) (Map NodeId Int)
toMaps fun ns = mapNodeIdNgrams $ documentIdWithNgrams fun ns'
  where
    ns' = map (\(Node nId _ _ _ _ _ json) -> DocumentWithId nId json) ns

mapNodeIdNgrams :: Hyperdata a
                => [DocumentIdWithNgrams a]
                -> Map (NgramsT Ngrams) (Map NodeId Int)
mapNodeIdNgrams ds = DM.map (DM.fromListWith (+)) $ DM.fromListWith (<>) xs
  where
    xs  = [(ng, [(nId, i)]) | (nId, n2i') <- n2i ds, (ng, i) <- DM.toList n2i']
    n2i = map (\d -> ((documentId . documentWithId) d, document_ngrams d))


documentIdWithNgrams :: Hyperdata a
                     => (a -> Map (NgramsT Ngrams) Int)
                     -> [DocumentWithId a]
                     -> [DocumentIdWithNgrams a]
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


docNgrams2nodeNodeNgrams :: CorpusId
                         -> DocNgrams
                         -> NodeNodeNgrams
docNgrams2nodeNodeNgrams cId (DocNgrams d n nt w) =
  NodeNodeNgrams cId d n nt w

data DocNgrams = DocNgrams { dn_doc_id :: DocId
                           , dn_ngrams_id :: Int
                           , dn_ngrams_type :: NgramsTypeId
                           , dn_weight  :: Double
                           }

insertDocNgramsOn :: CorpusId
                  -> [DocNgrams]
                  -> Cmd err Int
insertDocNgramsOn cId dn =
  insertNodeNodeNgrams
  $ (map (docNgrams2nodeNodeNgrams cId) dn)

insertDocNgrams :: CorpusId
                -> Map NgramsIndexed (Map NgramsType (Map NodeId Int))
                -> Cmd err Int
insertDocNgrams cId m =
  insertDocNgramsOn cId [ DocNgrams n (_ngramsId ng) (ngramsTypeId t) (fromIntegral i)
                          | (ng, t2n2i) <- DM.toList m
                          , (t,  n2i)   <- DM.toList t2n2i
                          , (n,  i)     <- DM.toList n2i
                        ]

