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
import Gargantext.Database.Action.Share (shareNodeWith)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

------------------------------------------------------------------------
data ShareNode = ShareTeam   { username :: Text }
               | SharePublic { rights   :: Text}
  deriving (Generic)
------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  ShareNode
instance ToJSON    ShareNode
instance ToSchema  ShareNode
instance Arbitrary ShareNode where
  arbitrary = elements [ ShareTeam "user1"
                       , SharePublic "public"
                       ]
------------------------------------------------------------------------
-- TODO permission
api :: HasNodeError err
    => NodeId
    -> ShareNode
    -> Cmd err Int
api nId (ShareTeam user) =
  fromIntegral <$> shareNodeWith nId NodeFolderShared (UserName user)
api nId (SharePublic _rights) =
  fromIntegral <$> shareNodeWith nId NodeFolderPublic UserPublic

------------------------------------------------------------------------
type API = Summary " Share Node with username"
         :> ReqBody '[JSON] ShareNode
         :> Post    '[JSON] Int

