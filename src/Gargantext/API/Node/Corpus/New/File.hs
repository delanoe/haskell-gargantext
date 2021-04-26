{-|
Module      : Gargantext.API.Node.Corpus.New.File
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Gargantext.API.Node.Corpus.New.File
  where

import Control.Lens ((.~), (?~))
import Control.Monad (forM)
import Data.Aeson
import Data.Maybe
import Data.Monoid (mempty)
import Data.Swagger
import Data.Text (Text())
import GHC.Generics (Generic)

import Servant
import Servant.Multipart
import Servant.Swagger.Internal
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Gargantext.Core.Types (TODO)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude -- (Cmd, CmdM)
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (hash)

-------------------------------------------------------------
type Hash = Text
data FileType = CSV
              | CSV_HAL
              | PresseRIS
              | WOS
  deriving (Eq, Show, Generic)

instance ToSchema FileType
instance Arbitrary FileType
  where
    arbitrary = elements [CSV, PresseRIS]
instance ToParamSchema FileType

instance FromJSON FileType
instance ToJSON FileType

instance ToParamSchema (MultipartData Mem) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

instance FromHttpApiData FileType
  where
    parseUrlPiece "CSV"       = pure CSV
    parseUrlPiece "CSV_HAL"   = pure CSV_HAL
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
  printDebug "File Type: " fileType
  is <- liftBase $ do
    printDebug "Inputs:" ()
    forM (inputs multipartData) $ \input -> do
      printDebug "iName  " (iName input)
      printDebug "iValue " (iValue input)
      pure $ iName input

  _ <- forM (files multipartData) $ \file -> do
    let content = fdPayload file
    printDebug "XXX " (fdFileName file)
    printDebug "YYY " content
    --pure $ cs content
  -- is <- inputs multipartData

  pure $ map hash is

-------------------------------------------------------------------
