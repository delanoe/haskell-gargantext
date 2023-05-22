{-# LANGUAGE TypeOperators #-}

module Gargantext.Core.Text.Upload
  ( Host(..)
  , DocId(..)
  , Data(..)
  , ContentType (..)
  , ethercalc
  , codimd
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Gargantext.Utils.Servant (CSV, Markdown)
import Network.HTTP.Client (newManager, Request(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Protolude
import Servant.API
import Servant.Client


newtype Host = Host { fromHost :: Text }
newtype DocId = DocId { fromDocId :: Text }
newtype Data = Data { fromData :: Text }
data ContentType a =
    CTPlain a
  | CTCSV a
  -- TODO SocialCalc, Excel XML ?

instance MimeRender CSV Data where
  mimeRender p (Data d) = mimeRender p d
instance MimeRender PlainText Data where
  mimeRender p (Data d) = mimeRender p d

instance ToHttpApiData DocId where
  toUrlPiece (DocId docId) = docId


-- https://github.com/audreyt/ethercalc/blob/master/API.md
type EthercalcAPI =
  "_" :> (
       -- plain text
           ReqBody '[PlainText] Data
        :> Post '[PlainText] Text
      :<|>
           Capture "docId" DocId
        :> ReqBody '[PlainText] Data
        :> Put '[PlainText] Text

      -- csv
      :<|>
           ReqBody '[CSV] Data
        :> Post '[PlainText, CSV] Text
      :<|>
           Capture "docId" DocId
        :> ReqBody '[CSV] Data
        :> Put '[PlainText, CSV] Text
  )

ethercalcAPI :: Proxy EthercalcAPI
ethercalcAPI = Proxy

ethercalcNewPlain :: Data -> ClientM Text
ethercalcUpdatePlain :: DocId -> Data -> ClientM Text
ethercalcNewCSV :: Data -> ClientM Text
ethercalcUpdateCSV :: DocId -> Data -> ClientM Text
ethercalcNewPlain :<|> ethercalcUpdatePlain
  :<|> ethercalcNewCSV :<|> ethercalcUpdateCSV = client ethercalcAPI


------------------------------

-- | Create new or update existing Ethercalc document (depending on
-- `Maybe DocId` constructor). `Data` can be in various formats (CSV,
-- etc).
ethercalc :: Host -> Maybe DocId -> ContentType Data -> IO (Either ClientError Text)
ethercalc (Host host) mDocId ctD = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https (T.unpack host) 443 "")
  case (mDocId, ctD) of
    (Nothing, CTPlain d)    -> runClientM (ethercalcNewPlain d) env
    (Nothing, CTCSV d)      -> runClientM (ethercalcNewCSV d) env
    (Just docId, CTPlain d) -> runClientM (ethercalcUpdatePlain docId d) env
    (Just docId, CTCSV d)   -> runClientM (ethercalcUpdateCSV docId d) env

-----------------------------------

type CodiMDAPI =
  "new" :> (
      ReqBody '[Markdown] Data
   :> Post '[Markdown] Text
  )

instance MimeRender Markdown Data where
  mimeRender p (Data d) = mimeRender p d

codimdAPI :: Proxy CodiMDAPI
codimdAPI = Proxy

codimdAPINew :: Data -> ClientM Text
codimdAPINew = client codimdAPI


-- | Create a new CodiMD document (with Markdown contents). Please
-- note that AFAIK CodiMD update is not supported, see
-- https://github.com/hackmdio/codimd/issues/1013
codimd :: Host -> Data -> IO (Either Text Text)
codimd (Host host) d = do
  manager' <- newManager tlsManagerSettings
  let env' = mkClientEnv manager' (BaseUrl Https (T.unpack host) 443 "")
  let env = env' { makeClientRequest = \burl req -> (defaultMakeClientRequest burl req) { redirectCount = 0 } }
  eRes <- runClientM (codimdAPINew d) env
  pure $ case eRes of
    -- NOTE We actually expect a redirect here (a 302 with the new
    -- page's URL). Hence we expect a `Left FailureResponse` because
    -- we have set `redirectCount = 0` above.
    Left (FailureResponse _req (Response { responseHeaders })) ->
      case Map.lookup "location" (Map.fromList $ toList responseHeaders) of
        Nothing -> Left "Cannot find 'Location' header in response"
        Just loc -> Right $ TE.decodeUtf8 loc
    err -> Left $ "Error creating codimd document: " <> show err
