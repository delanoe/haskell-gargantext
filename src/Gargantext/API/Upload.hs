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

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Upload
  where


import Gargantext.Prelude
import Data.Text (Text)
import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy as LBS
import Control.Monad
import Control.Monad.IO.Class
import Gargantext.API.Types
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Data.Swagger

-- | Upload files
-- TODO Is it possible to adapt the function according to iValue input ?
--type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

instance Generic Mem

instance ToSchema Mem
instance Arbitrary Mem

instance ToSchema (MultipartData Mem)
instance Arbitrary ( MultipartData Mem)

instance ToSchema (MultipartForm Mem (MultipartData Mem))
instance Arbitrary (MultipartForm Mem (MultipartData Mem))

type ApiUpload = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer
-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
upload :: GargServer ApiUpload
upload multipartData = do
  liftIO $ do
    putStrLn ("Inputs:" :: Text)
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ ("  " :: Text) <> (iName input)
            <> (" -> " :: Text) <> (iValue input)

    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ ("Content of " :: Text) <> (fdFileName file)
      LBS.putStr content
  return 0
-------------------------------------------------------------------------------

