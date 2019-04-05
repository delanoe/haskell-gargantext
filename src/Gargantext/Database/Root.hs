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

module Gargantext.Database.Root where

import Opaleye (restrict, (.==), Query)
import Opaleye.PGTypes (pgStrictText, pgInt4)
import Control.Arrow (returnA)
import Gargantext.Prelude
import Gargantext.Database.Types.Node (Node, NodePoly(..), NodeType(NodeUser), HyperdataUser)
import Gargantext.Database.Schema.Node (NodeRead)
import Gargantext.Database.Schema.Node (queryNodeTable)
import Gargantext.Database.Schema.User (queryUserTable, UserPoly(..))
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Database.Utils (Cmd, runOpaQuery)

getRoot :: Username -> Cmd err [Node HyperdataUser]
getRoot = runOpaQuery . selectRoot

selectRoot :: Username -> Query NodeRead
selectRoot username = proc () -> do
    row   <- queryNodeTable -< ()
    users <- queryUserTable -< ()
    restrict -< _node_typename row   .== (pgInt4 $ nodeTypeId NodeUser)
    restrict -< user_username  users .== (pgStrictText username)
    restrict -< _node_userId   row   .== (user_id users)
    returnA  -< row


