{-|
Module      : Gargantext.Database.Schema.Node
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Schema.Node where

import Control.Lens hiding (elements, (&))
import Gargantext.Database.Schema.Prelude
import Prelude hiding (null, id, map, sum)

------------------------------------------------------------------------
-- Main polymorphic Node definition
data NodePoly id
              hash_id
              typename
              user_id
              parent_id
              name
              date
              hyperdata  =
     Node { _node_id        :: !id
          , _node_hash_id   :: !hash_id
          , _node_typename  :: !typename

          , _node_user_id    :: !user_id
          , _node_parent_id  :: !parent_id

          , _node_name      :: !name
          , _node_date      :: !date

          , _node_hyperdata :: !hyperdata
          } deriving (Show, Generic)

------------------------------------------------------------------------
-- Automatic instances derivation
$(deriveJSON (unPrefix "_node_") ''NodePoly)
$(makeLenses ''NodePoly)

$(makeAdaptorAndInstance "pNode"   ''NodePoly)
$(makeLensesWith abbreviatedFields ''NodePoly)

nodeTable :: Table NodeWrite NodeRead
nodeTable = Table "nodes" (pNode Node { _node_id         = optionalTableField "id"
                                      , _node_hash_id    = optionalTableField "hash_id"
                                      , _node_typename   = requiredTableField "typename"
                                      , _node_user_id    = requiredTableField "user_id"

                                      , _node_parent_id  = optionalTableField "parent_id"
                                      , _node_name       = requiredTableField "name"
                                      , _node_date       = optionalTableField "date"

                                      , _node_hyperdata  = requiredTableField "hyperdata"
                                      -- ignoring ts_vector field here
                                      }
                            )

queryNodeTable :: Query NodeRead
queryNodeTable = selectTable nodeTable
------------------------------------------------------------------------
type NodeWrite = NodePoly (Maybe (Column SqlInt4)      )
                          (Maybe (Column SqlText)      )
                                 (Column SqlInt4)
                                 (Column SqlInt4)
                          (Maybe (Column SqlInt4)      )
                                 (Column SqlText)
                          (Maybe (Column SqlTimestamptz))
                                 (Column SqlJsonb)

type NodeRead = NodePoly (Column SqlInt4        )
                         (Column SqlText        )
                         (Column SqlInt4        )
                         (Column SqlInt4        )
                         (Column SqlInt4        )
                         (Column SqlText        )
                         (Column SqlTimestamptz )
                         (Column SqlJsonb       )

type NodeReadNull = NodePoly (Column (Nullable SqlInt4))
                             (Column (Nullable SqlText))
                             (Column (Nullable SqlInt4))
                             (Column (Nullable SqlInt4))
                             (Column (Nullable SqlInt4))
                             (Column (Nullable SqlText))
                             (Column (Nullable SqlTimestamptz))
                             (Column (Nullable SqlJsonb))
------------------------------------------------------------------------
-- | Node(Read|Write)Search is slower than Node(Write|Read) use it
-- for full text search only

type NodeSearchWrite =
  NodePolySearch
    (Maybe  (Column  SqlInt4)      )
    (Column  SqlInt4               )
    (Column  SqlInt4               )
    (Column (Nullable SqlInt4)     )
    (Column SqlText                )
    (Maybe  (Column SqlTimestamptz))
    (Column  SqlJsonb              )
    (Maybe  (Column SqlTSVector)   )

type NodeSearchRead =
  NodePolySearch
    (Column  SqlInt4           )
    (Column  SqlInt4           )
    (Column  SqlInt4           )
    (Column (Nullable SqlInt4 ))
    (Column  SqlText           )
    (Column  SqlTimestamptz    )
    (Column  SqlJsonb          )
    (Column  SqlTSVector       )

type NodeSearchReadNull =
  NodePolySearch
    (Column (Nullable SqlInt4)       )
    (Column (Nullable SqlInt4)       )
    (Column (Nullable SqlInt4)       )
    (Column (Nullable SqlInt4)       )
    (Column (Nullable SqlText)       )
    (Column (Nullable SqlTimestamptz))
    (Column (Nullable SqlJsonb)      )
    (Column (Nullable SqlTSVector)   )


data NodePolySearch id
                    typename
                    user_id
                    parent_id
                    name
                    date
                    hyperdata
                    search =
     NodeSearch { _ns_id           :: id
                , _ns_typename     :: typename
                , _ns_user_id      :: user_id
           --   , nodeUniqId       :: shaId
                , _ns_parent_id    :: parent_id
                , _ns_name         :: name
                , _ns_date         :: date

                , _ns_hyperdata    :: hyperdata
                , _ns_search       :: search
                } deriving (Show, Generic)

$(makeAdaptorAndInstance "pNodeSearch" ''NodePolySearch)
$(makeLensesWith abbreviatedFields ''NodePolySearch)
$(deriveJSON (unPrefix "_ns_")     ''NodePolySearch)
$(makeLenses ''NodePolySearch)

nodeTableSearch :: Table NodeSearchWrite NodeSearchRead
nodeTableSearch = Table "nodes" ( pNodeSearch
                                   NodeSearch { _ns_id           = optionalTableField "id"
                                              , _ns_typename     = requiredTableField "typename"
                                              , _ns_user_id      = requiredTableField "user_id"

                                              , _ns_parent_id    = requiredTableField "parent_id"
                                              , _ns_name         = requiredTableField "name"
                                              , _ns_date         = optionalTableField "date"

                                              , _ns_hyperdata    = requiredTableField "hyperdata"
                                              , _ns_search       = optionalTableField "search"
                                              }
                                )
------------------------------------------------------------------------
