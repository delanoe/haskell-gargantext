{-|
Module      : Gargantext.Database.Action.Query.Node.User
Description : User Node in Gargantext
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.Node.User
  where

import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe (fromMaybe)
import Data.Swagger (ToSchema(..), genericDeclareNamedSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (Name)
import Gargantext.Core.Types.Individu
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Admin.Types.Node (Node,Hyperdata, DocumentId, NodeId(..))
import Gargantext.Database.Admin.Types.Node (NodeType(..))
import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Admin.Utils -- (fromField', Cmd)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Contact (HyperdataContact, fake_HyperdataContact)
import Gargantext.Database.Schema.Node -- (Node(..))
import Gargantext.Prelude
import Opaleye hiding (FromField)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------
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

-- | Fake instances

fake_HyperdataUser :: HyperdataUser
fake_HyperdataUser = HyperdataUser (Just fake_HyperdataPrivate)
                                   (Just fake_HyperdataContact)
                                   (Just fake_HyperdataPublic)

fake_HyperdataPublic :: HyperdataPublic
fake_HyperdataPublic = HyperdataPublic "pseudo" [1..10]

fake_HyperdataPrivate :: HyperdataPrivate
fake_HyperdataPrivate = HyperdataPrivate "password" EN

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


-----------------------------------------------------------------
getNodeUser :: NodeId -> Cmd err (Node HyperdataUser)
getNodeUser nId = do
    fromMaybe (panic $ "Node does not exist: " <> (cs $ show nId)) . headMay
             <$> runOpaQuery (limit 1 $ selectNode (pgNodeId nId))


nodeUserW :: Maybe Name -> Maybe HyperdataUser -> UserId -> NodeWrite
nodeUserW maybeName maybeHyperdata = node NodeUser name user Nothing
  where
    name = maybe "User" identity maybeName
    user = maybe fake_HyperdataUser identity maybeHyperdata
