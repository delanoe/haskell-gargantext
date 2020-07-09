{-|
Module      : Gargantext.API.Node.Share
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Node.Share
      where

import Data.Aeson
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Database.Action.Share (ShareNodeWith(..))
import Gargantext.Database.Action.Share as DB (shareNodeWith, unPublish)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.API.Prelude
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

------------------------------------------------------------------------
data ShareNodeParams = ShareTeamParams   { username :: Text  }
                     | SharePublicParams { node_id  :: NodeId}
  deriving (Generic)
------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  ShareNodeParams where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
instance ToJSON    ShareNodeParams where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
instance ToSchema  ShareNodeParams
instance Arbitrary ShareNodeParams where
  arbitrary = elements [ ShareTeamParams "user1"
                       , SharePublicParams (NodeId 1)
                       ]
------------------------------------------------------------------------
-- TODO permission
api :: HasNodeError err
    => NodeId
    -> ShareNodeParams
    -> Cmd err Int
api nId (ShareTeamParams user) =
  fromIntegral <$> DB.shareNodeWith (ShareNodeWith_User NodeFolderShared (UserName user)) nId 
api nId2 (SharePublicParams nId1) =
  fromIntegral <$> DB.shareNodeWith (ShareNodeWith_Node NodeFolderPublic nId1) nId2

------------------------------------------------------------------------
type API = Summary " Share Node with username"
         :> ReqBody '[JSON] ShareNodeParams
         :> Post    '[JSON] Int

------------------------------------------------------------------------
type Unpublish = Summary " Unpublish Node"
               :> Capture "node_id" NodeId
               :> Put '[JSON] Int

unPublish :: NodeId -> GargServer Unpublish
unPublish n = DB.unPublish n
