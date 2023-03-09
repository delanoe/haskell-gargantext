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
import Data.Maybe
import Data.Monoid (mempty)
import Data.Swagger
import Data.Text (Text())

import Servant
import Servant.Multipart
import Servant.Swagger.Internal

import Gargantext.API.Node.Corpus.New.Types
import Gargantext.Core.Types (TODO)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude -- (Cmd, CmdM)
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (hash)

-------------------------------------------------------------
type Hash = Text

instance ToParamSchema (MultipartData Mem) where toParamSchema _ = toParamSchema (Proxy :: Proxy TODO)

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
                :> QueryParam "fileFormat" FileFormat
                :> MultipartForm Mem (MultipartData Mem)
                :> Post '[JSON] [Hash]

--postUpload :: NodeId -> Maybe FileType ->  GargServer UploadAPI
--postUpload :: NodeId -> GargServer UploadAPI
postUpload :: NodeId
           -> Maybe FileType
           -> Maybe FileFormat
           -> MultipartData Mem
           -> Cmd err [Hash]
postUpload _ Nothing _ _ = panic "fileType is a required parameter"
postUpload _ _ Nothing _ = panic "fileFormat is a required parameter"
postUpload _ (Just _fileType) (Just _fileFormat) multipartData = do
  -- printDebug "File Type: " fileType
  -- printDebug "File format: " fileFormat
  is <- liftBase $ do
    -- printDebug "Inputs:" ()
    forM (inputs multipartData) $ \input -> do
      -- printDebug "iName  " (iName input)
      -- printDebug "iValue " (iValue input)
      pure $ iName input

{-
  _ <- forM (files multipartData) $ \file -> do
    -- let content = fdPayload file
    -- printDebug "XXX " (fdFileName file)
    -- printDebug "YYY " content
    pure () -- $ cs content
  -- is <- inputs multipartData
-}
  pure $ map hash is

-------------------------------------------------------------------
