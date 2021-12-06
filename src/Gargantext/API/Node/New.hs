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

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE IncoherentInstances #-}
module Gargantext.API.Node.New
      where

import Control.Lens hiding (elements, Empty)
import Data.Aeson
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.Job.Async
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import Web.FormUrlEncoded          (FromForm, ToForm)

import Gargantext.API.Admin.Orchestrator.Types (JobLog(..), AsyncJobs)
import Gargantext.API.Prelude
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Node
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Table.Node.User
import Gargantext.Database.Schema.Node
import Gargantext.Prelude

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
instance ToForm    PostNode
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
  let uId' = nodeUser ^. node_user_id
  mkNodeWithParent nt (Just pId) uId' nodeName

------------------------------------------------------------------------
type PostNodeAsync = Summary "Post Node"
   :> "async"
   :> AsyncJobs JobLog '[FormUrlEncoded] PostNode JobLog


postNodeAsyncAPI :: UserId -> NodeId -> GargServer PostNodeAsync
postNodeAsyncAPI uId nId =
  serveJobsAPI $
    JobFunction (\p logs -> postNodeAsync uId nId p (liftBase . logs))

------------------------------------------------------------------------
postNodeAsync :: FlowCmdM env err m
    => UserId
    -> NodeId
    -> PostNode
    -> (JobLog -> m ())
    -> m JobLog
postNodeAsync uId nId (PostNode nodeName tn) logStatus = do

  printDebug "postNodeAsync" nId
  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 2
                   , _scst_events    = Just []
                   }

  nodeUser <- getNodeUser (NodeId uId)

  -- _ <- threadDelay 1000
  logStatus JobLog { _scst_succeeded = Just 1
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 2
                   , _scst_events    = Just []
                   }

  let uId' = nodeUser ^. node_user_id
  _ <- mkNodeWithParent tn (Just nId) uId' nodeName

  pure      JobLog { _scst_succeeded = Just 3
                   , _scst_failed    = Just 0
                   , _scst_remaining = Just 0
                   , _scst_events    = Just []
                   }
