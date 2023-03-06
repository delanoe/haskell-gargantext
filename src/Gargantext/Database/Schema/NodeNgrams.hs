{-|
Module      : Gargantext.Database.Schema.NodeNgrams
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

NodeNgrams register Context of Ngrams (named Cgrams then)

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNgrams where

import Data.Text (Text)
import Gargantext.Core.Types
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Database.Schema.Prelude
import Gargantext.Prelude


data NodeNgramsPoly id
                    node_id'
                    node_subtype
                    ngrams_id
                    ngrams_type
                    ngrams_field
                    ngrams_tag
                    ngrams_class
                    weight
                   = NodeNgrams { _nng_id            :: !id
                                , _nng_node_id       :: !node_id'
                                , _nng_node_subtype  :: !node_subtype
                                , _nng_ngrams_id     :: !ngrams_id
                                , _nng_ngrams_type   :: !ngrams_type
                                , _nng_ngrams_field  :: !ngrams_field
                                , _nng_ngrams_tag    :: !ngrams_tag
                                , _nng_ngrams_class  :: !ngrams_class
                                , _nng_ngrams_weight :: !weight
                              } deriving (Show, Eq, Ord)


type NodeNgramsWrite = NodeNgramsPoly (Maybe (Column (SqlInt4)))
                                      (Column (SqlInt4))
                                      (Maybe  (Column (SqlInt4)))
                                      (Column (SqlInt4))
                                      (Maybe  (Column (SqlInt4)))
                                      (Maybe  (Column (SqlInt4)))
                                      (Maybe  (Column (SqlInt4)))
                                      (Maybe  (Column (SqlInt4)))
                                      (Maybe  (Column (SqlFloat8)))

type NodeNgramsRead    = NodeNgramsPoly (Column SqlInt4)
                                      (Column SqlInt4)
                                      (Column SqlInt4)
                                      (Column SqlInt4)
                                      (Column SqlInt4)
                                      (Column SqlInt4)
                                      (Column SqlInt4)
                                      (Column SqlInt4)
                                      (Column SqlFloat8)

type NodeNgramsId = Int
type NgramsField  = Int
type NgramsTag    = Int
type NgramsClass  = Int
type NgramsText   = Text

-- Example of list Ngrams
-- type ListNgrams = NodeNgramsPoly (Maybe Int) ListType Text

type NodeNgramsW =
  NodeNgramsPoly (Maybe NodeNgramsId) NodeId ListType NgramsText
                  NgramsType (Maybe NgramsField) (Maybe NgramsTag) (Maybe NgramsClass)
                  Double

$(makeAdaptorAndInstance "pNodeNgrams" ''NodeNgramsPoly)
makeLenses ''NodeNgramsPoly

nodeNgramsTable :: Table NodeNgramsWrite NodeNgramsRead
nodeNgramsTable  =
  Table "node_ngrams"
         ( pNodeNgrams
           NodeNgrams { _nng_id            = optionalTableField "id"
                      , _nng_node_id       = requiredTableField "node_id"
                      , _nng_node_subtype  = optionalTableField "node_subtype"
                      , _nng_ngrams_id     = requiredTableField "ngrams_id"
                      , _nng_ngrams_type   = optionalTableField "ngrams_type"
                      , _nng_ngrams_field  = optionalTableField "ngrams_field"
                      , _nng_ngrams_tag    = optionalTableField "ngrams_tag"
                      , _nng_ngrams_class  = optionalTableField "ngrams_class"
                      , _nng_ngrams_weight = optionalTableField "weight"
                      }
                  )
