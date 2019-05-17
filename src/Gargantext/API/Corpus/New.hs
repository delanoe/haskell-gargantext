{-|
Module      : Gargantext.API.Corpus.New
Description : New corpus API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

New corpus means either:
- new corpus
- new data in existing corpus
-}

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Gargantext.API.Corpus.New
      where

import Servant
import Gargantext.Prelude
import Gargantext.API.Count (Query(..))
import Gargantext.Database.Types.Node (CorpusId)
--import Gargantext.Database.Flow (flowCorpusSearchInDatabase)

type Api = Summary "New Corpus endpoint"
         :> ReqBody '[JSON] Query
         :> Post '[JSON] CorpusId


api :: Monad m => Query -> m CorpusId
api _ = pure 1
