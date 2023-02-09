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

module Gargantext.Database.Schema.Context where

import Control.Lens hiding (elements, (&), Context)
import Gargantext.Database.Schema.Prelude
import Prelude hiding (null, id, map, sum)


------------------------------------------------------------------------
-- Main polymorphic Node definition
data ContextPoly id
                 hash_id
                 typename
                 user_id
                 parent_id
                 name
                 date
                 hyperdata  =
     Context { _context_id        :: !id
             , _context_hash_id   :: !hash_id
             , _context_typename  :: !typename

             , _context_user_id    :: !user_id
             , _context_parent_id  :: !parent_id

             , _context_name      :: !name
             , _context_date      :: !date

             , _context_hyperdata :: !hyperdata
             } deriving (Show, Generic)

------------------------------------------------------------------------
-- Automatic instances derivation
$(deriveJSON (unPrefix "_context_") ''ContextPoly)
$(makeLenses ''ContextPoly)

$(makeAdaptorAndInstance "pContext" ''ContextPoly)
$(makeLensesWith abbreviatedFields  ''ContextPoly)

contextTable :: Table ContextWrite ContextRead
contextTable = Table "contexts" (pContext Context { _context_id         = optionalTableField "id"
                                                  , _context_hash_id    = optionalTableField "hash_id"
                                                  , _context_typename   = requiredTableField "typename"
                                                  , _context_user_id    = requiredTableField "user_id"

                                                  , _context_parent_id  = optionalTableField "parent_id"
                                                  , _context_name       = requiredTableField "name"
                                                  , _context_date       = optionalTableField "date"

                                                  , _context_hyperdata  = requiredTableField "hyperdata"
                                                  -- ignoring ts_vector field here
                                                  }
                                        )

queryContextTable :: Query ContextRead
queryContextTable = selectTable contextTable
------------------------------------------------------------------------
type ContextWrite = ContextPoly (Maybe (Field SqlInt4)      )
                          (Maybe (Field SqlText)      )
                                 (Field SqlInt4)
                                 (Field SqlInt4)
                          (Maybe (Field SqlInt4)      )
                                 (Field SqlText)
                          (Maybe (Field SqlTimestamptz))
                                 (Field SqlJsonb)

type ContextRead = ContextPoly (Field SqlInt4        )
                         (Field SqlText        )
                         (Field SqlInt4        )
                         (Field SqlInt4        )
                         (Field SqlInt4        )
                         (Field SqlText        )
                         (Field SqlTimestamptz )
                         (Field SqlJsonb       )

type ContextReadNull = ContextPoly (FieldNullable SqlInt4)
                             (FieldNullable SqlText)
                             (FieldNullable SqlInt4)
                             (FieldNullable SqlInt4)
                             (FieldNullable SqlInt4)
                             (FieldNullable SqlText)
                             (FieldNullable SqlTimestamptz)
                             (FieldNullable SqlJsonb)
------------------------------------------------------------------------
-- | Context(Read|Write)Search is slower than Context(Write|Read) use it
-- for full text search only

type ContextSearchWrite =
  ContextPolySearch
    (Maybe  (Field  SqlInt4)      )
    (Field  SqlInt4               )
    (Field  SqlInt4               )
    (FieldNullable SqlInt4)
    (Field SqlText                )
    (Maybe  (Field SqlTimestamptz))
    (Field  SqlJsonb              )
    (Maybe  (Field SqlTSVector)   )

type ContextSearchRead =
  ContextPolySearch
    (Field  SqlInt4           )
    (Field  SqlInt4           )
    (Field  SqlInt4           )
    (FieldNullable SqlInt4 )
    (Field  SqlText           )
    (Field  SqlTimestamptz    )
    (Field  SqlJsonb          )
    (Field  SqlTSVector       )

type ContextSearchReadNull =
  ContextPolySearch
    (FieldNullable SqlInt4)
    (FieldNullable SqlInt4)
    (FieldNullable SqlInt4)
    (FieldNullable SqlInt4)
    (FieldNullable SqlText)
    (FieldNullable SqlTimestamptz)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlTSVector)


data ContextPolySearch id
                    typename
                    user_id
                    parent_id
                    name
                    date
                    hyperdata
                    search =
     ContextSearch { _cs_id           :: id
                   , _cs_typename     :: typename
                   , _cs_user_id      :: user_id
              --   , ContextUniqId       :: shaId
                   , _cs_parent_id    :: parent_id
                   , _cs_name         :: name
                   , _cs_date         :: date

                   , _cs_hyperdata    :: hyperdata
                   , _cs_search       :: search
                   } deriving (Show, Generic)

$(makeAdaptorAndInstance "pContextSearch" ''ContextPolySearch)
$(makeLensesWith abbreviatedFields        ''ContextPolySearch)
$(deriveJSON (unPrefix "_cs_")            ''ContextPolySearch)
$(makeLenses                              ''ContextPolySearch)

contextTableSearch :: Table ContextSearchWrite ContextSearchRead
contextTableSearch = Table "contexts" ( pContextSearch
                                   ContextSearch { _cs_id           = optionalTableField "id"
                                                 , _cs_typename     = requiredTableField "typename"
                                                 , _cs_user_id      = requiredTableField "user_id"

                                                 , _cs_parent_id    = requiredTableField "parent_id"
                                                 , _cs_name         = requiredTableField "name"
                                                 , _cs_date         = optionalTableField "date"

                                                 , _cs_hyperdata    = requiredTableField "hyperdata"
                                                 , _cs_search       = optionalTableField "search"
                                                 }
                                     )
------------------------------------------------------------------------
