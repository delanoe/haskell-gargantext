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
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes          #-}

module Gargantext.API.Corpus.New
      where

import Data.Aeson.TH (deriveJSON)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Flow (flowCorpusSearchInDatabase)
import Gargantext.Database.Types.Node (CorpusId)
import Gargantext.Prelude
import Gargantext.Prelude.Utils (hash)
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import Gargantext.Database.Flow (FlowCmdM)

data Query = Query { query_query      :: Text
                   , query_corpus_id  :: Int
                   , query_files_id  :: [Text]
                   }
                   deriving (Eq, Show, Generic)

deriveJSON (unPrefix "query_") ''Query


instance Arbitrary Query where
    arbitrary = elements [ Query q n fs
                         | q <- ["a","b"]
                         , n <- [0..10]
                         , fs <- map (map hash) [["a","b"], ["c","d"]]
                         ]

instance ToSchema Query where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 6 fieldLabel}



type Api = Summary "New Corpus endpoint"
         :> ReqBody '[JSON] Query
         :> Post '[JSON] CorpusId


api :: FlowCmdM env err m => Query -> m CorpusId
api (Query q _ _) = do
  cId <- flowCorpusSearchInDatabase "user1" EN q
  pure cId
