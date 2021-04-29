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
import Data.HashMap.Strict (HashMap)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.NodeNodeNgrams
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Types
import Gargantext.Prelude
import qualified Data.Map as DM
import qualified Data.HashMap.Strict as HashMap


data DocumentIdWithNgrams a b =
     DocumentIdWithNgrams
     { documentWithId :: Indexed NodeId a
     , documentNgrams :: HashMap b (Map NgramsType Int)
     } deriving (Show)

docNgrams2nodeNodeNgrams :: CorpusId
                         -> DocNgrams
                         -> NodeNodeNgrams
docNgrams2nodeNodeNgrams cId (DocNgrams d n nt w) =
  NodeNodeNgrams cId d n nt w

data DocNgrams = DocNgrams { dn_doc_id      :: DocId
                           , dn_ngrams_id   :: Int
                           , dn_ngrams_type :: NgramsTypeId
                           , dn_weight      :: Double
                           }

insertDocNgramsOn :: CorpusId
                  -> [DocNgrams]
                  -> Cmd err Int
insertDocNgramsOn cId dn =
  insertNodeNodeNgrams
  $ (map (docNgrams2nodeNodeNgrams cId) dn)

insertDocNgrams :: CorpusId
                -> HashMap (Indexed Int Ngrams) (Map NgramsType (Map NodeId Int))
                -> Cmd err Int
insertDocNgrams cId m =
  insertDocNgramsOn cId [ DocNgrams n (_index ng) (ngramsTypeId t) (fromIntegral i)
                          | (ng, t2n2i) <- HashMap.toList m
                          , (t,  n2i)   <- DM.toList t2n2i
                          , (n,  i)     <- DM.toList n2i
                        ]

