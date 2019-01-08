{-|
Module      : Gargantext.Database.Node.Children
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
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}

module Gargantext.Database.Node.Children where

import Opaleye
import Gargantext.Core.Types
import Gargantext.Database.Schema.Node
import Gargantext.Database.Utils
import Gargantext.Database.Schema.NodeNode
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Queries.Filter
import Gargantext.Database.Node.Contact (HyperdataContact)
import Gargantext.Database.Schema.Node (pgNodeId)
import Control.Arrow (returnA)

-- | TODO: use getChildren with Proxy ?
getContacts :: ParentId -> Maybe NodeType -> Cmd err [Node HyperdataContact]
getContacts pId maybeNodeType = runOpaQuery $ selectChildren pId maybeNodeType


getChildren :: JSONB a => ParentId -> proxy a -> Maybe NodeType -> Maybe Offset -> Maybe Limit -> Cmd err [Node a]
getChildren pId _ maybeNodeType maybeOffset maybeLimit = runOpaQuery
                  $ limit' maybeLimit $ offset' maybeOffset
                  $ orderBy (asc _node_id)
                  $ selectChildren pId maybeNodeType

selectChildren :: ParentId -> Maybe NodeType -> Query NodeRead
selectChildren parentId maybeNodeType = proc () -> do
    row@(Node nId typeName _ parent_id _ _ _) <- queryNodeTable -< ()
    (NodeNode n1id n2id _ _ _) <- queryNodeNodeTable -< ()
    
    let nodeType = maybe 0 nodeTypeId maybeNodeType
    restrict -< typeName  .== pgInt4 nodeType
    
    restrict -< (.||) (parent_id .== (pgNodeId parentId))
                      ( (.&&) (n1id .== pgNodeId parentId)
                              (n2id .== nId))
    returnA -< row




