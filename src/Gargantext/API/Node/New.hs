{-|
Module      : Gargantext.API.Node.Post
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

New = Post maybe change the name
Async new node feature

-}

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Node.New
      where

import Control.Lens hiding (elements, Empty)
import Data.Aeson
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Admin.Orchestrator.Types (ScraperStatus(..))
import Gargantext.API.Node.Corpus.New (AsyncJobs)
import Gargantext.API.Prelude
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Node
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Table.Node.User
import Gargantext.Database.Schema.Node
import Gargantext.Prelude
import Servant
import Servant.Job.Async
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import Web.FormUrlEncoded          (FromForm)

------------------------------------------------------------------------
data PostNode = PostNode { pn_name     :: Text
                         , pn_typename :: NodeType}
  deriving (Generic)
------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  PostNode
instance ToJSON    PostNode
instance ToSchema  PostNode
instance FromForm  PostNode
instance Arbitrary PostNode where
  arbitrary = elements [PostNode "Node test" NodeCorpus]

------------------------------------------------------------------------
postNode :: HasNodeError err
         => UserId
         -> NodeId
         -> PostNode
         -> Cmd err [NodeId]
postNode uId pId (PostNode nodeName nt) = do
  nodeUser <- getNodeUser (NodeId uId)
  let uId' = nodeUser ^. node_userId
  mkNodeWithParent nt (Just pId) uId' nodeName

------------------------------------------------------------------------
type PostNodeAsync = Summary "Post Node"
   :> "async"
   :> AsyncJobs ScraperStatus '[FormUrlEncoded] PostNode ScraperStatus


postNodeAsyncAPI :: UserId -> NodeId -> GargServer PostNodeAsync
postNodeAsyncAPI uId nId =
  serveJobsAPI $ 
    JobFunction (\p logs -> postNodeAsync uId nId p (liftBase . logs))

------------------------------------------------------------------------
postNodeAsync :: FlowCmdM env err m
    => UserId
    -> NodeId
    -> PostNode
    -> (ScraperStatus -> m ())
    -> m ScraperStatus
postNodeAsync uId nId (PostNode nodeName tn) logStatus = do

  printDebug "postNodeAsync" nId
  logStatus ScraperStatus { _scst_succeeded = Just 1
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 2
                          , _scst_events    = Just []
                          }

  nodeUser <- getNodeUser (NodeId uId)

  -- _ <- threadDelay 1000
  logStatus ScraperStatus { _scst_succeeded = Just 1
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 2
                          , _scst_events    = Just []
                          }

  let uId' = nodeUser ^. node_userId
  _ <- mkNodeWithParent tn (Just nId) uId' nodeName

  pure      ScraperStatus { _scst_succeeded = Just 3
                          , _scst_failed    = Just 0
                          , _scst_remaining = Just 0
                          , _scst_events    = Just []
                          }
