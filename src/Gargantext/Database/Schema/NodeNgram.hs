{-|
Module      : Gargantext.Database.Schema.NodeNgrams
Description : NodeNgram for Ngram indexation or Lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

NodeNgram: relation between a Node and a Ngrams

if Node is a Document then it is indexing
if Node is a List     then it is listing (either Stop, Candidate or Map)

-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}


-- TODO NodeNgrams
module Gargantext.Database.Schema.NodeNgram where

import Data.Text (Text)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Control.Monad (void)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core.Types.Main (ListId, ListTypeId)
import Gargantext.Database.Utils (mkCmd, Cmd, execPGSQuery)
import Gargantext.Database.Schema.NodeNgramsNgrams
import Gargantext.Prelude
import Opaleye
import qualified Database.PostgreSQL.Simple as PGS (Only(..))

-- | TODO : remove id
data NodeNgramPoly id node_id ngram_id weight ngrams_type
   = NodeNgram { nodeNgram_id      :: id
               , nodeNgram_node_id  :: node_id
               , nodeNgram_ngrams_id :: ngram_id
               , nodeNgram_weight  :: weight
               , nodeNgram_type    :: ngrams_type
               } deriving (Show)

type NodeNgramWrite =
     NodeNgramPoly
        (Maybe (Column PGInt4  ))
               (Column PGInt4  )
               (Column PGInt4  )
               (Column PGFloat8)
               (Column PGInt4  )

type NodeNgramRead =
     NodeNgramPoly
       (Column PGInt4  )
       (Column PGInt4  )
       (Column PGInt4  )
       (Column PGFloat8)
       (Column PGInt4  )

type NodeNgramReadNull =
     NodeNgramPoly
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGInt4  ))
       (Column (Nullable PGFloat8))
       (Column (Nullable PGInt4  ))

type NodeNgram =
     NodeNgramPoly (Maybe Int) Int Int Double Int

$(makeAdaptorAndInstance "pNodeNgram" ''NodeNgramPoly)
$(makeLensesWith abbreviatedFields    ''NodeNgramPoly)


nodeNgramTable :: Table NodeNgramWrite NodeNgramRead
nodeNgramTable  = Table "nodes_ngrams"
  ( pNodeNgram NodeNgram
    { nodeNgram_id        = optional "id"
    , nodeNgram_node_id   = required "node_id"
    , nodeNgram_ngrams_id = required "ngram_id"
    , nodeNgram_weight    = required "weight"
    , nodeNgram_type      = required "ngrams_type"
    }
  )

queryNodeNgramTable :: Query NodeNgramRead
queryNodeNgramTable = queryTable nodeNgramTable

insertNodeNgrams :: [NodeNgram] -> Cmd err Int
insertNodeNgrams = insertNodeNgramW
                 . map (\(NodeNgram _ n g w t) ->
                          NodeNgram Nothing (pgInt4 n)   (pgInt4 g)
                                            (pgDouble w) (pgInt4 t)
                        )

insertNodeNgramW :: [NodeNgramWrite] -> Cmd err Int
insertNodeNgramW nns =
  mkCmd $ \c -> fromIntegral <$> runInsert_ c insertNothing
    where
      insertNothing = (Insert { iTable = nodeNgramTable
                              , iRows  = nns
                              , iReturning = rCount
                              , iOnConflict = (Just DoNothing)
                              })

type NgramsText = Text

updateNodeNgrams' :: [(ListId, NgramsText, ListTypeId)] -> Cmd err ()
updateNodeNgrams' [] = pure ()
updateNodeNgrams' input = void $ execPGSQuery updateQuery (PGS.Only $ Values fields input)
  where
    fields = map (\t-> QualifiedIdentifier Nothing t) ["int4","text","int4"]
    updateQuery = [sql| UPDATE nodes_ngrams as old SET
                 ngrams_type = new.typeList
                 from (?) as new(node_id,terms,typeList)
                 JOIN ngrams ON ngrams.terms = new.terms
                 WHERE old.node_id = new.node_id
                 AND   old.ngram_id = ngrams.id
                 RETURNING old.ngram_id;
                 |]

data NodeNgramsUpdate = NodeNgramsUpdate
  { _nnu_lists_update :: [(ListId, NgramsText, ListTypeId)]
  , _nnu_add_children :: [(ListId, NgramsParent, NgramsChild, Maybe Double)]
  , _nnu_rem_children :: [(ListId, NgramsParent, NgramsChild, Maybe Double)]
  }

-- TODO wrap these updates in a transaction.
updateNodeNgrams :: NodeNgramsUpdate -> Cmd err ()
updateNodeNgrams nnu = do
  updateNodeNgrams' $ _nnu_lists_update nnu
  ngramsGroup Del   $ _nnu_rem_children nnu
  ngramsGroup Add   $ _nnu_add_children nnu
