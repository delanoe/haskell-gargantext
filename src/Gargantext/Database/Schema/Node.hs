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

import Control.Lens hiding (elements, (&))
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Swagger hiding (required)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Utils
import Opaleye hiding (FromField)
import Opaleye.Internal.QueryArr (Query)
import Prelude hiding (null, id, map, sum)
import Test.QuickCheck.Arbitrary


------------------------------------------------------------------------
data NodePoly id        typename userId 
              parentId  name     date 
              hyperdata  = Node { _node_id        :: id
                                , _node_typename  :: typename

                                , _node_userId    :: userId
                                , _node_parentId  :: parentId

                                , _node_name      :: name
                                , _node_date      :: date

                                , _node_hyperdata :: hyperdata
                                } deriving (Show, Generic)

$(deriveJSON (unPrefix "_node_") ''NodePoly)
$(makeLenses ''NodePoly)

$(makeAdaptorAndInstance "pNode"   ''NodePoly)
$(makeLensesWith abbreviatedFields ''NodePoly)

------------------------------------------------------------------------
nodeTable :: Table NodeWrite NodeRead
nodeTable = Table "nodes" (pNode Node { _node_id         = optional "id"
                                      , _node_typename   = required "typename"
                                      , _node_userId     = required "user_id"

                                      , _node_parentId   = optional "parent_id"
                                      , _node_name       = required "name"
                                      , _node_date       = optional "date"

                                      , _node_hyperdata  = required "hyperdata"
                                      -- ignoring ts_vector field here
                                      }
                            )

queryNodeTable :: Query NodeRead
queryNodeTable = queryTable nodeTable
------------------------------------------------------------------------
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


data NodePolySearch id        typename userId 
              parentId  name     date 
              hyperdata search = NodeSearch { _ns_id        :: id
                                      , _ns_typename  :: typename
                                      , _ns_userId    :: userId
                                                                --   , nodeUniqId    :: shaId
                                      , _ns_parentId  :: parentId
                                      , _ns_name      :: name
                                      , _ns_date      :: date

                                      , _ns_hyperdata :: hyperdata
                                      , _ns_search    :: search
                                      } deriving (Show, Generic)

$(makeAdaptorAndInstance "pNodeSearch" ''NodePolySearch)
$(makeLensesWith abbreviatedFields ''NodePolySearch)
$(deriveJSON (unPrefix "_ns_")     ''NodePolySearch)
$(makeLenses ''NodePolySearch)

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
------------------------------------------------------------------------
