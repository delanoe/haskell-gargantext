{-|
Module      : Gargantext.Database.Node.Select
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE Arrows            #-}

module Gargantext.Database.Query.Table.Node.Select
  where

import Control.Arrow (returnA)
import Opaleye
import Protolude

import Gargantext.Core.Types
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Database.Admin.Config
import Gargantext.Database.Prelude
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.User
import Gargantext.Database.Query.Table.User

selectNodesWithUsername :: NodeType -> Username -> Cmd err [NodeId]
selectNodesWithUsername nt u = runOpaQuery (q u)
  where
    q u' = proc () -> do
      (n,usrs) <- join' -< ()
      restrict -< user_username usrs .== (toNullable $ pgStrictText u')
      restrict -< _node_typename n .== (pgInt4 $ nodeTypeId nt)
      returnA  -< _node_id n

    join' :: Query (NodeRead, UserReadNull)
    join' = leftJoin queryNodeTable queryUserTable on1
      where
        on1 (n,us) = _node_userId n .== user_id us

