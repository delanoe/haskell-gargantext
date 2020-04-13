{-|
Module      : Gargantext.Database.Schema.NodeNodeNgrams
Description : TODO: remove this module and table in database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNodeNgrams
  where

import Prelude
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLenses)
import Gargantext.Database.Admin.Utils (Cmd, mkCmd)
import Gargantext.Database.Schema.Ngrams (NgramsTypeId, pgNgramsTypeId, NgramsId)
import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Admin.Types.Node
import Opaleye

data NodeNodeNgramsPoly n1 n2 ngrams_id ngt w
   = NodeNodeNgrams { _nnng_node1_id   :: n1
                    , _nnng_node2_id   :: n2
                    , _nnng_ngrams_id  :: ngrams_id
                    , _nnng_ngramsType :: ngt
                    , _nnng_weight     :: w
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

------------------------------------------------

queryNodeNodeNgramsTable :: Query NodeNodeNgramsRead
queryNodeNodeNgramsTable = queryTable nodeNodeNgramsTable

-- | Insert utils
insertNodeNodeNgrams :: [NodeNodeNgrams] -> Cmd err Int
insertNodeNodeNgrams = insertNodeNodeNgramsW
                     . map (\(NodeNodeNgrams n1 n2 ng nt w) ->
                              NodeNodeNgrams (pgNodeId n1)
                                             (pgNodeId n2)
                                             (pgInt4   ng)
                                             (pgNgramsTypeId nt)
                                             (pgDouble w)
                                                  )

insertNodeNodeNgramsW :: [NodeNodeNgramsWrite] -> Cmd err Int
insertNodeNodeNgramsW nnnw =
  mkCmd $ \c -> fromIntegral <$> runInsert_ c insertNothing
    where
      insertNothing = (Insert { iTable = nodeNodeNgramsTable
                              , iRows  = nnnw
                              , iReturning = rCount
                              , iOnConflict = (Just DoNothing)
      })

