{-|
Module      : Gargantext.Database.Schema.NodeNodeNgrams
Description : TODO: remove this module and table in database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-#  OPTIONS_GHC -fno-warn-orphans  #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNodeNgrams
  where

import Prelude
import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Schema.Ngrams (NgramsTypeId, NgramsId)
import Gargantext.Database.Admin.Types.Node

data NodeNodeNgramsPoly n1 n2 ngrams_id ngt w
   = NodeNodeNgrams { _nnng_node1_id   :: !n1
                    , _nnng_node2_id   :: !n2
                    , _nnng_ngrams_id  :: !ngrams_id
                    , _nnng_ngramsType :: !ngt
                    , _nnng_weight     :: !w
                    } deriving (Show)

type NodeNodeNgramsWrite =
     NodeNodeNgramsPoly (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGFloat8)

type NodeNodeNgramsRead  =
     NodeNodeNgramsPoly (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGFloat8)

type NodeNodeNgramsReadNull =
     NodeNodeNgramsPoly (Column (Nullable PGInt4  ))
                        (Column (Nullable PGInt4  ))
                        (Column (Nullable PGInt4  ))
                        (Column (Nullable PGInt4  ))
                        (Column (Nullable PGFloat8))

type NodeNodeNgrams =
  NodeNodeNgramsPoly CorpusId DocId NgramsId NgramsTypeId Double

$(makeAdaptorAndInstance "pNodeNodeNgrams" ''NodeNodeNgramsPoly)
makeLenses ''NodeNodeNgramsPoly


nodeNodeNgramsTable :: Table NodeNodeNgramsWrite NodeNodeNgramsRead
nodeNodeNgramsTable  = Table "node_node_ngrams"
                          ( pNodeNodeNgrams NodeNodeNgrams
                               { _nnng_node1_id   = required "node1_id"
                               , _nnng_node2_id   = required "node2_id"
                               , _nnng_ngrams_id  = required "ngrams_id"
                               , _nnng_ngramsType = required "ngrams_type"
                               , _nnng_weight     = required "weight"
                               }
                          )

