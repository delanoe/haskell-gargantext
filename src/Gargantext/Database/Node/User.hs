{-|
Module      : Gargantext.Database.Node.User
Description : User Node in Gargantext
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Node.User
  where

import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema(..), genericDeclareNamedSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Node.Contact (HyperdataContact)
import Gargantext.Database.Types.Node (Node,Hyperdata, DocumentId, NodeId(..))
import Gargantext.Database.Utils (fromField')
import Gargantext.Prelude
import Opaleye (QueryRunnerColumnDefault, queryRunnerColumnDefault, PGJsonb, fieldQueryRunnerColumn)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------

type NodeUser  = Node HyperdataUser

data HyperdataUser =
     HyperdataUser { _hu_private   :: !(Maybe HyperdataPrivate)
                   , _hu_shared    :: !(Maybe HyperdataContact)
                   , _hu_public    :: !(Maybe HyperdataPublic)
                   } deriving (Eq, Show, Generic)

data HyperdataPrivate =
     HyperdataPrivate { _hpr_password :: !Text
                      , _hpr_lang     :: !Lang
                      }
     deriving (Eq, Show, Generic)

data HyperdataPublic =
     HyperdataPublic { _hpu_pseudo :: !Text
                     , _hpu_publications :: ![DocumentId]
                     }
     deriving (Eq, Show, Generic)

-- | ToSchema instances
instance ToSchema HyperdataUser where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_hu_")

instance ToSchema HyperdataPrivate where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_hpr_")

instance ToSchema HyperdataPublic where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_hpu_")


-- | Arbitrary instances
instance Arbitrary HyperdataUser where
  arbitrary = HyperdataUser <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HyperdataPrivate where
  arbitrary = elements [HyperdataPrivate "" EN]

instance Arbitrary HyperdataPublic where
  arbitrary = elements [HyperdataPublic "pseudo" [NodeId 2]]


-- | Specific Gargantext instance
instance Hyperdata HyperdataUser
instance Hyperdata HyperdataPrivate
instance Hyperdata HyperdataPublic

-- | Database (Posgresql-simple instance)
instance FromField HyperdataUser where
  fromField = fromField'
instance FromField HyperdataPrivate where
  fromField = fromField'
instance FromField HyperdataPublic where
  fromField = fromField'

-- | Database (Opaleye instance)
instance QueryRunnerColumnDefault PGJsonb HyperdataUser   where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataPrivate   where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataPublic   where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- | All lenses
makeLenses ''HyperdataUser
makeLenses ''HyperdataPrivate
makeLenses ''HyperdataPublic

-- | All Json instances
$(deriveJSON (unPrefix "_hu_") ''HyperdataUser)
$(deriveJSON (unPrefix "_hpr_") ''HyperdataPrivate)
$(deriveJSON (unPrefix "_hpu_") ''HyperdataPublic)
