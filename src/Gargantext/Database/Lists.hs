{-|
Module      : Gargantext.Database.Lists
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

module Gargantext.Database.Lists where

import Control.Arrow (returnA)
import Gargantext.Core.Types -- (NodePoly(..), NodeCorpus, ListId)
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Schema.Node -- (HasNodeError, queryNodeTable)
import Gargantext.Database.Schema.User -- (queryUserTable)
import Gargantext.Database.Utils
import Gargantext.Prelude hiding (sum, head)
import Opaleye hiding (FromField)
import Opaleye.Internal.QueryArr (Query)
import Prelude hiding (null, id, map, sum)

-- | To get all lists of a user
-- /!\ lists of different types of corpora (Annuaire or Documents)
listsWith :: HasNodeError err => Username -> Cmd err [Maybe ListId]
listsWith u = runOpaQuery (selectLists u)
  where
    selectLists u = proc () -> do
      (auth_user,nodes) <- listsWithJoin2 -< ()
      restrict -< user_username auth_user .== (pgStrictText u)
      restrict -< _node_typename nodes .== (toNullable $ pgInt4 $ nodeTypeId NodeList)
      returnA  -< _node_id nodes

listsWithJoin2 :: Query (UserRead, NodeReadNull)
listsWithJoin2 = leftJoin queryUserTable queryNodeTable cond12
  where
    cond12 (u,n)       = user_id u   .== _node_userId n

{-
listsWithJoin3 :: Query (NodeRead, (UserRead, NodeReadNull))
listsWithJoin3 = leftJoin3 queryUserTable queryNodeTable queryNodeTable cond12 cond23
  where
    cond12 :: (NodeRead
    cond12 (u,n)       = user_id u   .== _node_userId n
    cond23 :: (NodeRead, (UserRead, NodeReadNull)) -> Column PGBool
    cond23 (n1,(u,n2)) = (toNullable $ _node_id n1) .== _node_parentId n2
--}

