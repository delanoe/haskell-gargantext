{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Gargantext.Text.Parsers.Isidore where

import Data.Text (Text)
import Data.Either
import Gargantext.Prelude
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client


type IsidoreAPI = "sparql" :> Capture "query" Text :> Get '[JSON] [IsidoreDoc]


data IsidoreDoc =
     IsidoreDoc {title :: Maybe Text}
     deriving (Show, Generic)


instance FromJSON IsidoreDoc
instance ToJSON   IsidoreDoc

isidoreDocsApi :: Proxy IsidoreAPI
isidoreDocsApi = Proxy

isidoreDocs :: ClientM [IsidoreDoc]
isidoreDocs = client isidoreDocsApi

getIsidoreDocs :: IO [IsidoreDoc]
getIsidoreDocs = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM isidoreDocs $ mkClientEnv manager' $ BaseUrl Https "https://www.rechercheisidore.fr" 8080 ""
  case res of
    Left  _ ->  panic "err"
    Right res' -> pure res'
