{-|
Module      : Gargantext.API.Admin.Auth.Types
Description : Types for Server API Auth Module
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}

module Gargantext.API.Admin.Auth.Types
      where

import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Auth.Server
import Test.QuickCheck (elements, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Gargantext.Core.Types.Individu (Username, GargPassword(..), arbitraryUsername, arbitraryPassword)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Node (NodeId(..), ListId, DocId, UserId)
import Gargantext.Prelude hiding (reverse)

---------------------------------------------------

-- | Main types for AUTH API
data AuthRequest = AuthRequest { _authReq_username :: Username
                               , _authReq_password :: GargPassword
                               }
  deriving (Generic)

-- TODO: Use an HTTP error to wrap AuthInvalid
data AuthResponse = AuthResponse { _authRes_valid :: Maybe AuthValid
                                 , _authRes_inval :: Maybe AuthInvalid
                                 }
  deriving (Generic)

data AuthInvalid = AuthInvalid { _authInv_message :: Text }
  deriving (Generic)

data AuthValid = AuthValid { _authVal_token   :: Token
                           , _authVal_tree_id :: TreeId
                           , _authVal_user_id :: UserId
                           }
  deriving (Generic)

type Token  = Text
type TreeId = NodeId

data CheckAuth = InvalidUser | InvalidPassword | Valid Token TreeId UserId
  deriving (Eq)

newtype AuthenticatedUser = AuthenticatedUser
  { _authUser_id :: NodeId
  } deriving (Generic)

$(deriveJSON (unPrefix "_authUser_") ''AuthenticatedUser)

instance ToSchema AuthenticatedUser where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authUser_")

instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

-- TODO-SECURITY why is the CookieSettings necessary?
type AuthContext = '[JWTSettings, CookieSettings] -- , BasicAuthCfg

-- | Instances
$(deriveJSON (unPrefix "_authReq_") ''AuthRequest)
instance ToSchema AuthRequest where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authReq_")

instance Arbitrary AuthRequest where
  arbitrary = elements [ AuthRequest u p
                       | u <- arbitraryUsername
                       , p <- arbitraryPassword
                       ]

$(deriveJSON (unPrefix "_authRes_") ''AuthResponse)
instance ToSchema AuthResponse where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authRes_")
instance Arbitrary AuthResponse where
  arbitrary = oneof [ AuthResponse Nothing . Just      <$> arbitrary
                    , flip AuthResponse Nothing . Just <$> arbitrary ]

$(deriveJSON (unPrefix "_authInv_") ''AuthInvalid)
instance ToSchema AuthInvalid where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authInv_")
instance Arbitrary AuthInvalid where
  arbitrary = elements [ AuthInvalid m 
                       | m <- [ "Invalid user", "Invalid password"]
                       ]

$(deriveJSON (unPrefix "_authVal_") ''AuthValid)
instance ToSchema AuthValid where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_authVal_")
instance Arbitrary AuthValid where
  arbitrary = elements [ AuthValid to tr u
                       | to <- ["token0", "token1"]
                       , tr <- [1..3]
                       , u <-  [1..3]
                       ]

data PathId = PathNode NodeId | PathNodeNode ListId DocId


---------------------------

type Email = Text
type Password = Text

data ForgotPasswordRequest = ForgotPasswordRequest { _fpReq_email :: Email }
  deriving (Generic )
$(deriveJSON (unPrefix "_fpReq_") ''ForgotPasswordRequest)
instance ToSchema ForgotPasswordRequest where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_fpReq_")

data ForgotPasswordResponse = ForgotPasswordResponse { _fpRes_status :: Text }
  deriving (Generic )
$(deriveJSON (unPrefix "_fpRes_") ''ForgotPasswordResponse)
instance ToSchema ForgotPasswordResponse where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_fpRes_")

data ForgotPasswordGet = ForgotPasswordGet {_fpGet_password :: Password}
  deriving (Generic )
$(deriveJSON (unPrefix "_fpGet_") ''ForgotPasswordGet)
instance ToSchema ForgotPasswordGet where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_fpGet_") 