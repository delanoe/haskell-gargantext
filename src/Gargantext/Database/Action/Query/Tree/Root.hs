{-|
Module      : Gargantext.Database.Root
Description : Main requests to get root of users
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Action.Query.Tree.Root
  where

import Control.Arrow (returnA)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Admin.Config (nodeTypeId)
import Gargantext.Database.Action.Query.Node.User (HyperdataUser)
import Gargantext.Database.Schema.Node (NodeRead)
import Gargantext.Database.Schema.Node (queryNodeTable)
import Gargantext.Database.Schema.User (UserPoly(..))
import Gargantext.Database.Action.Query.User (queryUserTable)
import Gargantext.Database.Admin.Types.Node (Node, NodePoly(..), NodeType(NodeUser), pgNodeId)
import Gargantext.Database.Admin.Utils (Cmd, runOpaQuery)
import Gargantext.Prelude
import Opaleye (restrict, (.==), Query)
import Opaleye.PGTypes (pgStrictText, pgInt4)

getRoot :: User -> Cmd err [Node HyperdataUser]
getRoot = runOpaQuery . selectRoot

selectRoot :: User -> Query NodeRead
selectRoot (UserName username) = proc () -> do
    row   <- queryNodeTable -< ()
    users <- queryUserTable -< ()
    restrict -< _node_typename row   .== (pgInt4 $ nodeTypeId NodeUser)
    restrict -< user_username  users .== (pgStrictText username)
    restrict -< _node_userId   row   .== (user_id users)
    returnA  -< row

selectRoot (UserDBId uid) = proc () -> do
    row   <- queryNodeTable -< ()
    restrict -< _node_typename row   .== (pgInt4 $ nodeTypeId NodeUser)
    restrict -< _node_userId   row   .== (pgInt4 uid)
    returnA  -< row

selectRoot (RootId nid) =
 proc () -> do
    row   <- queryNodeTable -< ()
    restrict -< _node_typename row   .== (pgInt4 $ nodeTypeId NodeUser)
    restrict -< _node_id   row   .== (pgNodeId nid)
    returnA  -< row



