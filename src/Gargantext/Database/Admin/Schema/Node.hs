{-|
Module      : Gargantext.Database.Schema.Node
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Schema.Node where

import Control.Arrow (returnA)
import Control.Lens (set, view)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Aeson
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Int (Int64)
import Gargantext.Core.Types
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Node.Contact (HyperdataContact(..), arbitraryHyperdataContact)
import Gargantext.Database.Query.Filter (limit', offset')
import Gargantext.Database.Types.Errors
import Gargantext.Database.Types.Node (NodeType(..), defaultCorpus, Hyperdata, HyperData(..))
import Gargantext.Database.Utils
import Gargantext.Prelude hiding (sum, head)
import Gargantext.Viz.Graph (HyperdataGraph(..))
import Opaleye hiding (FromField)
import Opaleye.Internal.QueryArr (Query)
import Prelude hiding (null, id, map, sum)

------------------------------------------------------------------------
instance FromField HyperdataAny where
    fromField = fromField'

instance FromField HyperdataCorpus
  where
    fromField = fromField'

instance FromField HyperdataDocument
  where
    fromField = fromField'

instance FromField HyperdataDocumentV3
  where
    fromField = fromField'

instance FromField HyperData
  where
    fromField = fromField'

instance FromField HyperdataListModel
  where
    fromField = fromField'

instance FromField HyperdataGraph
  where
    fromField = fromField'

instance FromField HyperdataPhylo
  where
    fromField = fromField'

instance FromField HyperdataAnnuaire
  where
    fromField = fromField'

instance FromField HyperdataList
  where
    fromField = fromField'

instance FromField (NodeId, Text)
  where
    fromField = fromField'
------------------------------------------------------------------------
instance QueryRunnerColumnDefault PGJsonb HyperdataAny
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataList
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperData
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


instance QueryRunnerColumnDefault PGJsonb HyperdataDocument
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataDocumentV3
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataListModel
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataGraph
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataPhylo
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataAnnuaire
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGTSVector (Maybe TSVector)
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGInt4 (Maybe NodeId)
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGInt4 NodeId
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault (Nullable PGInt4) NodeId
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


------------------------------------------------------------------------
$(makeAdaptorAndInstance "pNode" ''NodePoly)
$(makeLensesWith abbreviatedFields ''NodePoly)

$(makeAdaptorAndInstance "pNodeSearch" ''NodePolySearch)
$(makeLensesWith abbreviatedFields ''NodePolySearch)

type NodeWrite = NodePoly (Maybe (Column PGInt4)      )
                                 (Column PGInt4)
                                 (Column PGInt4)
                          (Maybe (Column PGInt4)      )
                                 (Column PGText)
                          (Maybe (Column PGTimestamptz))
                                 (Column PGJsonb)

type NodeRead = NodePoly (Column PGInt4        )
                         (Column PGInt4        )
                         (Column PGInt4        )
                         (Column PGInt4        )
                         (Column PGText        )
                         (Column PGTimestamptz )
                         (Column PGJsonb       )

type NodeReadNull = NodePoly (Column (Nullable PGInt4))
                             (Column (Nullable PGInt4))
                             (Column (Nullable PGInt4))
                             (Column (Nullable PGInt4))
                             (Column (Nullable PGText))
                             (Column (Nullable PGTimestamptz))
                             (Column (Nullable PGJsonb))

nodeTable :: Table NodeWrite NodeRead
nodeTable = Table "nodes" (pNode Node { _node_id         = optional "id"
                                      , _node_typename   = required "typename"
                                      , _node_userId     = required "user_id"

                                      , _node_parentId   = optional "parent_id"
                                      , _node_name       = required "name"
                                      , _node_date       = optional "date"

                                      , _node_hyperdata  = required "hyperdata"
                                      }
                            )

queryNodeTable :: Query NodeRead
queryNodeTable = queryTable nodeTable

------------------------------------------------------------------------
-- | Node(Read|Write)Search is slower than Node(Write|Read) use it
-- for full text search only
type NodeSearchWrite =
  NodePolySearch
    (Maybe  (Column  PGInt4)      )
    (Column  PGInt4               )
    (Column  PGInt4               )
    (Column (Nullable PGInt4)     )
    (Column PGText                )
    (Maybe  (Column PGTimestamptz))
    (Column  PGJsonb              )
    (Maybe  (Column PGTSVector)   )

type NodeSearchRead =
  NodePolySearch
    (Column  PGInt4           )
    (Column  PGInt4           )
    (Column  PGInt4           )
    (Column (Nullable PGInt4 ))
    (Column  PGText           )
    (Column  PGTimestamptz    )
    (Column  PGJsonb          )
    (Column  PGTSVector       )

type NodeSearchReadNull =
  NodePolySearch
    (Column (Nullable PGInt4)       )
    (Column (Nullable PGInt4)       )
    (Column (Nullable PGInt4)       )
    (Column (Nullable PGInt4)       )
    (Column (Nullable PGText)       )
    (Column (Nullable PGTimestamptz))
    (Column (Nullable PGJsonb)      )
    (Column (Nullable PGTSVector)   )

nodeTableSearch :: Table NodeSearchWrite NodeSearchRead
nodeTableSearch = Table "nodes" (pNodeSearch NodeSearch { _ns_id         = optional "id"
                                      , _ns_typename   = required "typename"
                                      , _ns_userId     = required "user_id"

                                      , _ns_parentId   = required "parent_id"
                                      , _ns_name       = required "name"
                                      , _ns_date       = optional "date"

                                      , _ns_hyperdata  = required "hyperdata"
                                      , _ns_search     = optional "search"
                                      }
                            )


