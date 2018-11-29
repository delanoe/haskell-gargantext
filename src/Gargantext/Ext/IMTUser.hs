{-|
Module      : Gargantext.Ext.IMTUser
Description : Interface to get IMT users
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

We can not import the IMT Client API code since it is copyrighted.
Here is writtent a common interface.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Gargantext.Ext.IMTUser (deserialiseImtUsersFromFile)
  where


import System.IO (FilePath)
import Codec.Serialise
import Data.Maybe (Maybe, catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Prelude

import Gargantext.Database.Node.Contact -- (HyperdataContact, ContactWho, ContactWhere, ContactTouch, ContactMetaData)
import qualified Data.ByteString.Lazy as BSL

instance Serialise IMTUser

deserialiseImtUsersFromFile :: FilePath -> IO [HyperdataContact]
deserialiseImtUsersFromFile filepath = map imtUser2gargContact <$> deserialiseFromFile' filepath

deserialiseFromFile' :: FilePath -> IO [IMTUser]
deserialiseFromFile' filepath = deserialise <$> BSL.readFile filepath

serialiseToFile :: FilePath -> [IMTUser] -> IO ()
serialiseToFile f d = BSL.writeFile f (serialise d)

data IMTUser = IMTUser
  { id        :: Text
  , entite    :: Maybe Text
  , mail      :: Maybe Text
  , nom       :: Maybe Text
  , prenom    :: Maybe Text
  , fonction  :: Maybe Text
  , tel       :: Maybe Text
  , fax       :: Maybe Text
  , service   :: Maybe Text
  , groupe    :: Maybe Text
  , bureau    :: Maybe Text
  , url       :: Maybe Text
  , pservice  :: Maybe Text
  , pfonction :: Maybe Text
  , afonction :: Maybe Text
  , grprech   :: Maybe Text
  , lieu      :: Maybe Text
  , aprecision   :: Maybe Text
  , atel         :: Maybe Text
  , sexe         :: Maybe Text
  , statut       :: Maybe Text
  , idutilentite :: Maybe Text
  , entite2  :: Maybe Text
  , service2 :: Maybe Text
  , groupe2  :: Maybe Text
  , actif    :: Maybe Text
  , idutilsiecoles    :: Maybe Text
  , date_modification :: Maybe Text
  } deriving (Eq, Show, Generic)

imtUser2gargContact :: IMTUser -> HyperdataContact
imtUser2gargContact (IMTUser id' entite' mail' nom' prenom' fonction' tel' _fax'
                     service' _groupe' bureau' url' _pservice' _pfonction' _afonction'
                     _grprech' lieu' _aprecision' _atel' _sexe' _statut' _idutilentite'
                     _entite2' _service2' _group2' _actif' _idutilsiecoles' date_modification')
                  = HyperdataContact (Just qui) (Just [ou]) (Just meta) Nothing Nothing
                    where
                      qui = ContactWho (Just id') prenom' nom' (Just $ catMaybes [service']) Nothing
                      ou  = ContactWhere (toList entite') (toList service') fonction' bureau' (Just "France") lieu' contact Nothing Nothing
                      contact = Just $ ContactTouch mail' tel' url'
                      meta    = ContactMetaData (Just "IMT annuaire") date_modification'
                      toList Nothing  = Nothing
                      toList (Just x) = Just [x]
                             


