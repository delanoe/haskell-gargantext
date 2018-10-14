{-|
Module      : Gargantext.Database.Node.Update
Description : Update Node in Database (Postgres)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Gargantext.Database.Node.Update (Update(..), update) where

import Data.Text (Text)
import qualified Data.Text as DT
import Database.PostgreSQL.Simple

import Gargantext.Prelude

-- import Data.ByteString
--rename :: Connection -> NodeId -> Text -> IO ByteString
--rename conn nodeId name = formatQuery conn "UPDATE nodes SET name=? where id=?" (name,nodeId)
------------------------------------------------------------------------
type NodeId = Int
type Name   = Text
type ParentId = Int

data Update = Rename NodeId Name
            | Move   NodeId ParentId

-- | Update a Node
-- TODO : Field as parameter
-- TODO jsonb values, consider this: 
-- https://stackoverflow.com/questions/26703476/how-to-perform-update-operations-on-columns-of-type-jsonb-in-postgres-9-4
update :: Update -> Connection -> IO [Only Int]
update (Rename nId name) conn = query conn "UPDATE nodes SET name=? where id=? returning id"
                                           (DT.take 255 name,nId)
update (Move nId pId) conn    = query conn "UPDATE nodes SET parent_id= ? where id=? returning id"
                                           (pId, nId)


