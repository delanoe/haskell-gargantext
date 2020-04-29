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

module Gargantext.Database.Query.Tree.Root
  where

import Data.Either (Either, fromLeft, fromRight)
import Control.Arrow (returnA)
import Gargantext.Core.Types.Main (CorpusName)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Admin.Config (nodeTypeId, userMaster)
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.User (HyperdataUser)
import Gargantext.Database.Action.Flow.Utils (getUserId)
import Gargantext.Database.Schema.Node (NodePoly(..), NodeRead)
import Gargantext.Database.Schema.Node (queryNodeTable)
import Gargantext.Database.Query
import Gargantext.Database.Query.Table.User (queryUserTable, UserPoly(..))
import Gargantext.Database.Admin.Types.Node (Node, NodeType(NodeUser), pgNodeId)
import Gargantext.Database.Prelude (Cmd, runOpaQuery)
import Gargantext.Prelude
import Opaleye (restrict, (.==), Query)
import Opaleye.PGTypes (pgStrictText, pgInt4)



getOrMkRoot :: (HasNodeError err)
            => User
            -> Cmd err (UserId, RootId)
getOrMkRoot user = do
  userId <- getUserId user

  rootId' <- map _node_id <$> getRoot user

  rootId'' <- case rootId' of
        []  -> mkRoot user
        n   -> case length n >= 2 of
            True  -> nodeError ManyNodeUsers
            False -> pure rootId'

  rootId <- maybe (nodeError NoRootFound) pure (head rootId'')
  pure (userId, rootId)


getOrMk_RootWithCorpus :: (HasNodeError err, MkCorpus a)
                      => User
                      -> Either CorpusName [CorpusId]
                      -> Maybe a
                      -> Cmd err (UserId, RootId, CorpusId)
getOrMk_RootWithCorpus user cName c = do
  (userId, rootId) <- getOrMkRoot user
  corpusId'' <- if user == UserName userMaster
                  then do
                    ns <- getCorporaWithParentId rootId
                    pure $ map _node_id ns
                  else
                    pure $ fromRight [] cName

  corpusId' <- if corpusId'' /= []
                  then pure corpusId''
                  else do
                    c' <- mk (Just $ fromLeft "Default" cName) c rootId userId
                    _tId <- case head c' of
                              Nothing -> pure [0]
                              Just c'' -> mkNode NodeTexts c'' userId
                    pure c'

  corpusId <- maybe (nodeError NoCorpusFound) pure (head corpusId')
  pure (userId, rootId, corpusId)






mkRoot :: HasNodeError err
       => User
       -> Cmd err [RootId]
mkRoot user = do

  -- TODO
  -- udb <- getUserDb user
  -- let uid = user_id udb
  uid <- getUserId user

  -- TODO ? Which name for user Node ?
  let una = "username"

  case uid > 0 of
     False -> nodeError NegativeId
     True  -> do
       rs <- mkNodeWithParent NodeUser Nothing uid una
       _ <- case rs of
         [r] -> do
           _ <- mkNodeWithParent NodeFolderPrivate (Just r) uid una
           _ <- mkNodeWithParent NodeFolderShared  (Just r) uid una
           _ <- mkNodeWithParent NodeFolderPublic  (Just r) uid una
           pure rs
         _   -> pure rs
       pure rs

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

