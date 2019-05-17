{-|
Module      : Gargantext.API.Upload
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Gargantext.API.Upload
  where

import qualified Data.Text as Text
import GHC.Generics (Generic)
import Gargantext.Prelude
import Data.Text (Text)
import Data.Aeson
import Servant
import Servant.Multipart
--import Servant.Mock (HasMock(mock))
import Servant.Swagger (HasSwagger(toSwagger))
-- import qualified Data.ByteString.Lazy as LBS
import Control.Monad
import Control.Monad.IO.Class
import Gargantext.API.Types
--import Servant.CSV.Cassava (CSV'(..))
--import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
--import Data.Swagger
--import Gargantext.API.Ngrams (TODO)

-- | Upload files
-- TODO Is it possible to adapt the function according to iValue input ?
--type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

-- instance Generic Mem

--instance ToSchema Mem
--instance Arbitrary Mem

--instance ToSchema (MultipartData Mem)
--instance Arbitrary ( MultipartData Mem)

instance HasSwagger (MultipartForm tag a :> sub) where
  -- TODO
  toSwagger _ = undefined -- toSwagger (Proxy :: Proxy (TODO :> Post '[JSON] ()))
--declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy TODO)
--instance Arbitrary (MultipartForm Mem (MultipartData Mem))

{-
instance (FromMultipart tag a, MultipartBackend tag, Servant.Multipart.LookupContext context (MultipartOptions tag))
      => HasMock (MultipartForm tag a :> sub) context where
  mock _ _ = undefined

instance HasMock (MultipartForm Mem (MultipartData Mem) :> sub) context where
  mock _ _ = undefined
-}

data Upload = Upload { up :: [Text] }
  deriving (Generic)

instance ToJSON Upload

type ApiUpload = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Text
-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
upload :: GargServer ApiUpload
upload multipartData = do

--{-
  is <- liftIO $ do
    putStrLn ("Inputs:" :: Text)
    forM (inputs multipartData) $ \input -> do
      putStrLn $ ("iName  " :: Text) <> (iName input)
            <> ("iValue " :: Text) <> (iValue input)
      pure $ iName input

--{-
  _ <- forM (files multipartData) $ \file -> do
    let content = fdPayload file
    putStrLn $ ("XXX " :: Text) <> (fdFileName file)
    putStrLn $ ("YYY " :: Text) <>  cs content
    --pure $ cs content
  -- is <- inputs multipartData
--}

  pure $ Text.concat $ map cs is
-------------------------------------------------------------------------------


