{-|
Module      : Gargantext.Database.Flow.Utils
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Database.Action.Flow.Utils
    where

import Data.Map (Map)
import Gargantext.Database.Admin.Types.Hyperdata (Hyperdata)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.NodeNodeNgrams
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node
import Gargantext.Database.Types
import Gargantext.Prelude
import qualified Data.Map as DM


toMaps :: Hyperdata a
       => (a -> Map (NgramsT Ngrams) Int)
       -> [Node a]
       -> Map (NgramsT Ngrams) (Map NodeId Int)
toMaps fun ns = mapNodeIdNgrams $ documentIdWithNgrams fun ns'
  where
    ns' = map (\(Node nId _ _ _ _ _ _ json) -> DocumentWithId nId json) ns

mapNodeIdNgrams :: Hyperdata a
                => [DocumentIdWithNgrams a]
                -> Map (NgramsT Ngrams) (Map NodeId Int)
mapNodeIdNgrams ds = DM.map (DM.fromListWith (+)) $ DM.fromListWith (<>) xs
  where
    xs  = [(ng, [(nId, i)]) | (nId, n2i') <- n2i ds, (ng, i) <- DM.toList n2i']
    n2i = map (\d -> ((documentId . documentWithId) d, documentNgrams d))


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
     , documentNgrams :: Map (NgramsT Ngrams) Int
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
                -> Map (Indexed Ngrams) (Map NgramsType (Map NodeId Int))
                -> Cmd err Int
insertDocNgrams cId m =
  insertDocNgramsOn cId [ DocNgrams n (_index ng) (ngramsTypeId t) (fromIntegral i)
                          | (ng, t2n2i) <- DM.toList m
                          , (t,  n2i)   <- DM.toList t2n2i
                          , (n,  i)     <- DM.toList n2i
                        ]

