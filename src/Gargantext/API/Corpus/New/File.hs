{-|
Module      : Gargantext.API.Corpus.New.File
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Gargantext.API.Corpus.New.File
  where

import Control.Lens ((.~), (?~))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Aeson
import Data.Monoid (mempty)
import Data.Swagger
import Data.Text (Text())
import GHC.Generics (Generic)
import Gargantext.API.Ngrams (TODO)
import Gargantext.Database.Types.Node
import Gargantext.Database.Utils -- (Cmd, CmdM)
import Gargantext.Prelude
import Gargantext.Prelude.Utils (sha)
import Servant
import Servant.Multipart
import Servant.Swagger (HasSwagger(toSwagger))
import Servant.Swagger.Internal
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

-------------------------------------------------------------
type Hash = Text
data FileType = CSV | PresseRIS
  deriving (Eq, Show, Generic)

instance ToSchema FileType
instance Arbitrary FileType
  where
    arbitrary = elements [CSV, PresseRIS]
instance ToParamSchema FileType

instance FromJSON FileType

instance ToParamSchema (MultipartData Mem) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance FromHttpApiData FileType
  where
    parseUrlPiece "CSV"       = pure CSV
    parseUrlPiece "PresseRis" = pure PresseRIS
    parseUrlPiece _           = pure CSV -- TODO error here


instance (ToParamSchema a, HasSwagger sub) =>
         HasSwagger (MultipartForm tag a :> sub) where
  -- TODO
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
              & addParam param
    where
      param = mempty
            & required ?~ True
            & schema   .~ ParamOther sch
      sch = mempty
          & in_         .~ ParamFormData
          & paramSchema .~ toParamSchema (Proxy :: Proxy a)


type WithUpload' = Summary "Upload file(s) to a corpus"
                :> QueryParam "fileType"  FileType
                :> MultipartForm Mem (MultipartData Mem)
                :> Post '[JSON] [Hash]

--postUpload :: NodeId -> Maybe FileType ->  GargServer UploadAPI
--postUpload :: NodeId -> GargServer UploadAPI
postUpload :: NodeId
           -> Maybe FileType
           -> MultipartData Mem
           -> Cmd err [Hash]
postUpload _ Nothing _ = panic "fileType is a required parameter"
postUpload _ (Just fileType) multipartData = do
  putStrLn $ "File Type: " <> (show fileType)
  is <- liftIO $ do
    putStrLn ("Inputs:" :: Text)
    forM (inputs multipartData) $ \input -> do
      putStrLn $ ("iName  " :: Text) <> (iName input)
               <> ("iValue " :: Text) <> (iValue input)
      pure $ iName input

  _ <- forM (files multipartData) $ \file -> do
    let content = fdPayload file
    putStrLn $ ("XXX " :: Text) <> (fdFileName file)
    putStrLn $ ("YYY " :: Text) <>  cs content
    --pure $ cs content
  -- is <- inputs multipartData

  pure $ map (sha . cs) is

-------------------------------------------------------------------
