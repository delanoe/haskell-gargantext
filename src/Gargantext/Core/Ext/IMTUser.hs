{-|
Module      : Gargantext.Core.Ext.IMTUser
Description : Interface to get IMT users
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

We can not import the IMT Client API code since it is copyrighted.
Here is writtent a common interface.

-}


module Gargantext.Core.Ext.IMTUser -- (deserialiseImtUsersFromFile)
  where

import Codec.Serialise
import Data.Csv
import Data.Either
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Gargantext.Core.Text.Corpus.Parsers.CSV
import Gargantext.Database.Admin.Types.Hyperdata.Contact
import Gargantext.Prelude
import System.FilePath.Posix (takeExtension)
import System.IO (FilePath)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as Vector

------------------------------------------------------------------------
readFile_Annuaire :: FilePath -> IO [HyperdataContact]
readFile_Annuaire fp = case takeExtension fp of
    ".csv"  -> readCSVFile_Annuaire fp
    ".data" -> deserialiseImtUsersFromFile fp
    _       -> panic "[G.C.E.I.readFile_Annuaire] extension unknown"

------------------------------------------------------------------------
data IMTUser = IMTUser
  { id         :: Maybe Text
  , entite     :: Maybe Text
  , mail       :: Maybe Text
  , nom        :: Maybe Text
  , prenom     :: Maybe Text
  , fonction   :: Maybe Text
  , fonction2  :: Maybe Text
  , tel        :: Maybe Text
  , fax        :: Maybe Text
  , service    :: Maybe Text
  , groupe     :: Maybe Text
  , entite2    :: Maybe Text
  , service2   :: Maybe Text
  , groupe2    :: Maybe Text
  , bureau     :: Maybe Text
  , url        :: Maybe Text
  , pservice   :: Maybe Text
  , pfonction  :: Maybe Text
  , afonction  :: Maybe Text
  , afonction2 :: Maybe Text
  , grprech      :: Maybe Text
  , appellation  :: Maybe Text
  , lieu         :: Maybe Text
  , aprecision   :: Maybe Text
  , atel         :: Maybe Text
  , sexe         :: Maybe Text
  , statut       :: Maybe Text
  , idutilentite :: Maybe Text
  , actif             :: Maybe Text
  , idutilsiecoles    :: Maybe Text
  , date_modification :: Maybe Text
  } deriving (Eq, Show, Generic)

-- | CSV instance
instance FromNamedRecord IMTUser where
  parseNamedRecord r = IMTUser <$> r .: "id"
                               <*> r .: "entite"
                               <*> r .: "mail"
                               <*> r .: "nom"
                               <*> r .: "prenom"
                               <*> r .: "fonction"
                               <*> r .: "fonction2"
                               <*> r .: "tel"
                               <*> r .: "fax"
                               <*> r .: "service"
                               <*> r .: "groupe"
                               <*> r .: "entite2"
                               <*> r .: "service2"
                               <*> r .: "groupe2"
                               <*> r .: "bureau"
                               <*> r .: "url"
                               <*> r .: "pservice"
                               <*> r .: "pfonction"
                               <*> r .: "afonction"
                               <*> r .: "afonction2"
                               <*> r .: "grprech"
                               <*> r .: "appellation"
                               <*> r .: "lieu"
                               <*> r .: "aprecision"
                               <*> r .: "atel"
                               <*> r .: "sexe"
                               <*> r .: "statut"
                               <*> r .: "idutilentite"
                               <*> r .: "actif"
                               <*> r .: "idutilsiecoles"
                               <*> r .: "date_modification"

headerCSVannuaire :: Header
headerCSVannuaire =
  header ["id","entite","mail","nom","prenom","fonction","fonction2","tel","fax","service","groupe","entite2","service2","groupe2","bureau","url","pservice","pfonction","afonction","afonction2","grprech","appellation","lieu","aprecision","atel","sexe","statut","idutilentite","actif","idutilsiecoles","date_modification"]


readCSVFile_Annuaire :: FilePath -> IO [HyperdataContact]
readCSVFile_Annuaire fp = do
  users <- snd <$> readCSVFile_Annuaire' fp
  pure $ map imtUser2gargContact $ Vector.toList users

readCSVFile_Annuaire' :: FilePath -> IO (Header, Vector IMTUser)
readCSVFile_Annuaire' = fmap readCsvHalLazyBS' . BL.readFile
  where
    readCsvHalLazyBS' :: BL.ByteString -> (Header, Vector IMTUser)
    readCsvHalLazyBS' bs = case decodeByNameWith csvDecodeOptions bs of
          Left  e    -> panic (cs e)
          Right rows -> rows

------------------------------------------------------------------------
-- | Serialization for optimization
instance Serialise IMTUser
deserialiseImtUsersFromFile :: FilePath -> IO [HyperdataContact]
deserialiseImtUsersFromFile filepath = map imtUser2gargContact <$> deserialiseFromFile' filepath

deserialiseFromFile' :: FilePath -> IO [IMTUser]
deserialiseFromFile' filepath = deserialise <$> BL.readFile filepath

------------------------------------------------------------------------
imtUser2gargContact :: IMTUser -> HyperdataContact
imtUser2gargContact (IMTUser id' entite' mail' nom' prenom' fonction' _fonction2' tel' _fax'
                     service' _groupe' _entite2 _service2 _group2 bureau' url' _pservice' _pfonction' _afonction' _afonction2'
                     _grprech' _appellation' lieu' _aprecision' _atel' _sexe' _statut' _idutilentite'
                     _actif' _idutilsiecoles' date_modification')
                  = HyperdataContact (Just "IMT Annuaire") (Just qui) [ou] ((<>) <$> (fmap (\p -> p <> " ") prenom') <*> nom') entite' date_modification' Nothing Nothing
                    where
                      qui = ContactWho id' prenom' nom' (catMaybes [service']) []
                      ou  = ContactWhere (toList entite') (toList service') fonction' bureau' (Just "France") lieu' contact Nothing Nothing
                      contact = Just $ ContactTouch mail' tel' url'
                      -- meta    = ContactMetaData (Just "IMT annuaire") date_modification'
                      toList Nothing  = []
                      toList (Just x) = [x]
