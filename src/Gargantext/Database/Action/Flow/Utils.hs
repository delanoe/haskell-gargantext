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
import Gargantext.Database.Query.Table.ContextNodeNgrams
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

docNgrams2contextNodeNgrams :: ListId
                         -> DocNgrams
                         -> ContextNodeNgrams
docNgrams2contextNodeNgrams lId (DocNgrams d n nt w) =
  ContextNodeNgrams d lId n nt w

data DocNgrams = DocNgrams { dn_doc_id      :: DocId
                           , dn_ngrams_id   :: Int
                           , dn_ngrams_type :: NgramsTypeId
                           , dn_weight      :: Double
                           }

insertDocNgramsOn :: ListId
                  -> [DocNgrams]
                  -> Cmd err Int
insertDocNgramsOn cId dn =
  insertContextNodeNgrams
  $ (map (docNgrams2contextNodeNgrams cId) dn)

insertDocNgrams :: ListId
                -> HashMap (Indexed Int Ngrams) (Map NgramsType (Map DocId Int))
                -> Cmd err Int
insertDocNgrams cId m =
  insertDocNgramsOn cId [ DocNgrams { dn_doc_id = n
                                    , dn_ngrams_id = _index ng
                                    , dn_ngrams_type = ngramsTypeId t
                                    , dn_weight = fromIntegral i }
                          | (ng, t2n2i) <- HashMap.toList m
                          , (t,  n2i)   <- DM.toList t2n2i
                          , (n,  i)     <- DM.toList n2i
                        ]

