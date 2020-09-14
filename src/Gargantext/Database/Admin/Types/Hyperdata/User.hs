{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.User
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}


module Gargantext.Database.Admin.Types.Hyperdata.User
  where

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Database.Admin.Types.Hyperdata.Contact
import Gargantext.Database.Admin.Types.Node (DocumentId)

-- import Gargantext.Database.Schema.Node -- (Node(..))

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
     HyperdataPublic { _hpu_pseudo       :: !Text
                     , _hpu_publications :: ![DocumentId]
                     }
     deriving (Eq, Show, Generic)

-- | Default
defaultHyperdataUser :: HyperdataUser
defaultHyperdataUser = HyperdataUser (Just defaultHyperdataPrivate)
                                   (Just defaultHyperdataContact)
                                   (Just defaultHyperdataPublic)

defaultHyperdataPublic :: HyperdataPublic
defaultHyperdataPublic = HyperdataPublic "pseudo" [1..10]

defaultHyperdataPrivate :: HyperdataPrivate
defaultHyperdataPrivate = HyperdataPrivate "password" EN


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
-- | Specific Gargantext instance
instance Hyperdata HyperdataUser
instance Hyperdata HyperdataPrivate
instance Hyperdata HyperdataPublic

-- | All lenses
makeLenses ''HyperdataUser
makeLenses ''HyperdataPrivate
makeLenses ''HyperdataPublic

-- | All Json instances
$(deriveJSON (unPrefix "_hu_")  ''HyperdataUser)
$(deriveJSON (unPrefix "_hpr_") ''HyperdataPrivate)
$(deriveJSON (unPrefix "_hpu_") ''HyperdataPublic)

-- | Arbitrary instances
instance Arbitrary HyperdataUser where
  arbitrary = HyperdataUser <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HyperdataPrivate where
  arbitrary = pure defaultHyperdataPrivate

instance Arbitrary HyperdataPublic where
  arbitrary = pure defaultHyperdataPublic

-- | ToSchema instances
instance ToSchema HyperdataUser where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hu_") proxy
    & mapped.schema.description ?~ "User Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataUser

instance ToSchema HyperdataPrivate where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hpr_") proxy
    & mapped.schema.description ?~ "User Private Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataPrivate


instance ToSchema HyperdataPublic where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (unPrefixSwagger "_hpu_") proxy
    & mapped.schema.description ?~ "User Public Hyperdata"
    & mapped.schema.example ?~ toJSON defaultHyperdataPublic


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

