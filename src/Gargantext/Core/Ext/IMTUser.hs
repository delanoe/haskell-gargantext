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
  parseNamedRecord r = do
    id <- r .: "id"
    entite <- r .: "entite"
    mail <- r .: "mail"
    nom <- r .: "nom"
    prenom <- r .: "prenom"
    fonction <- r .: "fonction"
    fonction2 <- r .: "fonction2"
    tel <- r .: "tel"
    fax <- r .: "fax"
    service <- r .: "service"
    groupe <- r .: "groupe"
    entite2 <- r .: "entite2"
    service2 <- r .: "service2"
    groupe2 <- r .: "groupe2"
    bureau <- r .: "bureau"
    url <- r .: "url"
    pservice <- r .: "pservice"
    pfonction <- r .: "pfonction"
    afonction <- r .: "afonction"
    afonction2 <- r .: "afonction2"
    grprech <- r .: "grprech"
    appellation <- r .: "appellation"
    lieu <- r .: "lieu"
    aprecision <- r .: "aprecision"
    atel <- r .: "atel"
    sexe <- r .: "sexe"
    statut <- r .: "statut"
    idutilentite <- r .: "idutilentite"
    actif <- r .: "actif"
    idutilsiecoles <- r .: "idutilsiecoles"
    date_modification <- r .: "date_modification"
    pure $ IMTUser {..}

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
    readCsvHalLazyBS' bs = case decodeByNameWith (csvDecodeOptions Tab) bs of
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
--imtUser2gargContact (IMTUser id' entite' mail' nom' prenom' fonction' _fonction2' tel' _fax'
--                     service' _groupe' _entite2 _service2 _group2 bureau' url' _pservice' _pfonction' _afonction' _afonction2'
--                     _grprech' _appellation' lieu' _aprecision' _atel' _sexe' _statut' _idutilentite'
--                     _actif' _idutilsiecoles' date_modification')
--                  = HyperdataContact (Just "IMT Annuaire") (Just qui) [ou] ((<>) <$> (fmap (\p -> p <> " ") prenom') <*> nom') entite' date_modification' Nothing Nothing
imtUser2gargContact (IMTUser { id
                             , entite
                             , mail
                             , nom
                             , prenom
                             , fonction
                             , tel
                             , service
                             , bureau
                             , url
                             , lieu
                             , date_modification }) =
                        HyperdataContact { _hc_bdd = Just "IMT Annuaire"
                                         , _hc_who = Just qui
                                         , _hc_where = [ou]
                                         , _hc_title = title
                                         , _hc_source = entite
                                         , _hc_lastValidation = date_modification
                                         , _hc_uniqIdBdd = Nothing
                                         , _hc_uniqId = Nothing }
  where
    title = (<>) <$> (fmap (\p -> p <> " ") prenom) <*> nom
    qui = ContactWho { _cw_id = id
                     , _cw_firstName = prenom
                     , _cw_lastName = nom
                     , _cw_keywords = catMaybes [service]
                     , _cw_freetags = []
                     , _cw_description = Nothing }
    ou  = ContactWhere { _cw_organization = toList entite
                       , _cw_labTeamDepts = toList service
                       , _cw_role = fonction
                       , _cw_office = bureau
                       , _cw_country = Just "France"
                       , _cw_city = lieu
                       , _cw_touch = contact
                       , _cw_entry = Nothing
                       , _cw_exit = Nothing }
    contact = Just $ ContactTouch { _ct_mail = mail
                                  , _ct_phone = tel
                                  , _ct_url = url }
    -- meta    = ContactMetaData (Just "IMT annuaire") date_modification'
    toList Nothing  = []
    toList (Just x) = [x]
  



