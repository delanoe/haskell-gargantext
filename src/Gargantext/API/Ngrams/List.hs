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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Gargantext.API.Ngrams.List
  where

import Gargantext.Prelude
import Gargantext.API.Ngrams
import Servant
import Data.List (zip)
import Data.Map (Map, toList, fromList)
import Gargantext.Database.Types.Node
import Gargantext.Database.Schema.Ngrams (NgramsType(..), ngramsTypes)
import Gargantext.Database.Flow (FlowCmdM)
import Gargantext.API.Types (GargServer)
import Gargantext.API.Ngrams (putListNgrams')

type NgramsList = (Map NgramsType (Versioned NgramsTableMap))

type API = Get '[JSON] NgramsList
      :<|> ReqBody '[JSON] NgramsList :> Put '[JSON] Bool

api :: ListId -> GargServer API
api l = get l :<|> put l

get :: RepoCmdM env err m 
    => ListId -> m NgramsList
get lId = fromList
       <$> zip ngramsTypes
       <$> mapM (getNgramsTableMap lId) ngramsTypes

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


