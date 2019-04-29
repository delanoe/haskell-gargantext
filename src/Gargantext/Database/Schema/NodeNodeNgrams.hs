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
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.NodeNodeNgrams
  where

import Prelude
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
--import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Gargantext.Database.Utils (Cmd, mkCmd)
import Gargantext.Database.Schema.Ngrams (NgramsTypeId, pgNgramsTypeId, NgramsId)
import Gargantext.Database.Schema.Node (pgNodeId)
import Gargantext.Database.Types.Node
import Opaleye



data NodeNodeNgramsPoly id' n1 n2 ngrams_id ngt w
   = NodeNodeNgrams { nnng_id         :: id'
                    , nnng_node1_id   :: n1
                    , nnng_node2_id   :: n2
                    , nnng_ngrams_id  :: ngrams_id
                    , nnng_ngramsType :: ngt
                    , nnng_weight     :: w
                    } deriving (Show)


type NodeNodeNgramsWrite =
     NodeNodeNgramsPoly (Maybe (Column PGInt4  ))
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGFloat8)

type NodeNodeNgramsRead  =
     NodeNodeNgramsPoly (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGInt4  )
                        (Column PGFloat8)

type NodeNodeNgramsReadNull =
     NodeNodeNgramsPoly (Column (Nullable PGInt4  ))
                        (Column (Nullable PGInt4  ))
                        (Column (Nullable PGInt4  ))
                        (Column (Nullable PGInt4  ))
                        (Column (Nullable PGInt4  ))
                        (Column (Nullable PGFloat8))

type NodeNodeNgrams =
  NodeNodeNgramsPoly (Maybe Int) CorpusId DocId NgramsId NgramsTypeId Double

--{-
$(makeAdaptorAndInstance "pNodeNodeNgrams" ''NodeNodeNgramsPoly)
-- $(makeLensesWith          abbreviatedFields ''NodeNodeNgramsPoly)

nodeNodeNgramsTable :: Table NodeNodeNgramsWrite NodeNodeNgramsRead
nodeNodeNgramsTable  = Table "node_node_ngrams"
                          ( pNodeNodeNgrams NodeNodeNgrams
                               { nnng_id         = optional "id"
                               , nnng_node1_id   = required "node1_id"
                               , nnng_node2_id   = required "node2_id"
                               , nnng_ngrams_id  = required "ngrams_id"
                               , nnng_ngramsType = required "ngrams_type"
                               , nnng_weight     = required "weight"
                               }
                          )

queryNodeNodeNgramsTable :: Query NodeNodeNgramsRead
queryNodeNodeNgramsTable = queryTable nodeNodeNgramsTable


-- | Insert utils
insertNodeNodeNgrams :: [NodeNodeNgrams] -> Cmd err Int
insertNodeNodeNgrams = insertNodeNodeNgramsW
                     . map (\(NodeNodeNgrams id'' n1 n2 ng nt w) ->
                              NodeNodeNgrams (pgInt4 <$> id'')
                                             (pgNodeId n1)
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


