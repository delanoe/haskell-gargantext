{-|
Module      : Gargantext.API.Ngrams.List
Description : Get Ngrams (lists)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Ngrams.List
  where

import Data.Text (Text)
import Data.Aeson
import Data.List (zip)
import Data.Map (Map, toList, fromList)
import Network.HTTP.Media ((//), (/:))
import Servant

import Gargantext.Prelude
import Gargantext.API.Ngrams
import Gargantext.API.Types (GargServer)
import Gargantext.Database.Flow (FlowCmdM)
import Gargantext.Database.Schema.Ngrams (NgramsType(..), ngramsTypes)
import Gargantext.Database.Types.Node

type NgramsList = (Map NgramsType (Versioned NgramsTableMap))

data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToJSON a => MimeRender HTML a where
  mimeRender _ = encode

type API = Get '[JSON] NgramsList
      :<|> ReqBody '[JSON] NgramsList :> Put '[JSON] Bool
      :<|> Get '[HTML] (Headers '[Header "Content-Disposition" Text] NgramsList)

api :: ListId -> GargServer API
api l = get l :<|> put l :<|> getHtml l

get :: RepoCmdM env err m
    => ListId -> m NgramsList
get lId = fromList
       <$> zip ngramsTypes
       <$> mapM (getNgramsTableMap lId) ngramsTypes

getHtml :: RepoCmdM env err m
        => ListId -> m (Headers '[Header "Content-Disposition" Text] NgramsList)
getHtml lId = do
  lst <- get lId
  return $ addHeader "attachment" lst


-- TODO : purge list
put :: FlowCmdM env err m
    => ListId
    -> NgramsList
    -> m Bool
put l m = do
  -- TODO check with Version for optim
  _ <- mapM (\(nt, Versioned _v ns) -> putListNgrams' l nt ns) $ toList m
  -- TODO reindex
  pure True


