{-|
Module      : Gargantext.API.Node.Get
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Polymorphic Get Node API

-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Node.Get
      where

import Data.Aeson
import Data.Swagger
import GHC.Generics (Generic)
import Servant
import Test.QuickCheck.Arbitrary

import Gargantext.API.Prelude
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (JSONB)
import Gargantext.Prelude

------------------------------------------------------------------------
type API a = Summary "Polymorphic Get Node Endpoint"
         :> ReqBody '[JSON] GetNodeParams
         :> Get '[JSON] (Node a)

------------------------------------------------------------------------
data GetNodeParams = GetNodeParams { node_id  :: NodeId 
                                   , nodetype :: NodeType
                                   }
    deriving (Generic)

----------------------------------------------------------------------
api :: forall proxy a.
    ( JSONB a
    , FromJSON a
    , ToJSON   a
    ) => proxy a -> UserId -> NodeId -> GargServer (API a)
api _p _uId _nId (GetNodeParams _nId' _nt) = undefined

------------------------------------------------------------------------
instance FromJSON  GetNodeParams where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON    GetNodeParams where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })
  
instance ToSchema  GetNodeParams
instance Arbitrary GetNodeParams where
  arbitrary = GetNodeParams <$> arbitrary <*> arbitrary

------------------------------------------------------------------------
