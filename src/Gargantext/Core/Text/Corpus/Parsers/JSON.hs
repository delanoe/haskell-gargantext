{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.JSON
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

JSON parser for Gargantext corpus files.

-}

{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.Core.Text.Corpus.Parsers.JSON where

import Conduit
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Either (Either(..))
import Data.Text
import GHC.Generics

import qualified Prelude

import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
-- import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Prelude hiding (length)


data JSONStruct =
  JSONStruct { documents    :: [ JSONStructDocument ]
             , garg_version :: Text }
  deriving (Generic)
instance FromJSON JSONStruct

data JSONStructDocument =
  JSONStructDocument { document :: JSONDocument
                     , ngrams   :: JSONNgrams
                     , hash     :: Text }
  deriving (Generic)
instance FromJSON JSONStructDocument

data JSONDocument =
  JSONDocument { id        :: Int
               , hash_id   :: Maybe Text
               , typename  :: Int
               , user_id   :: Int
               , parent_id :: Maybe Int
               , name      :: Text
               , date      :: Text
               , hyperdata :: HyperdataDocument }
  deriving (Generic)
instance FromJSON JSONDocument

data JSONNgrams =
  JSONNgrams { ngrams :: [Text]
             , hash   :: Text }
  deriving (Generic)
instance FromJSON JSONNgrams

------------------------------------------------------------------------
-- | TODO: documents -> document -> hyperdata + title etc
readJSONLazyBS :: BL.ByteString -> Either Prelude.String JSONStruct
readJSONLazyBS bs = eitherDecode bs


parseJSONC :: BL.ByteString
           -> Either Prelude.String (Maybe Integer, ConduitT () HyperdataDocument Identity ())
parseJSONC bs = do
  case readJSONLazyBS bs of
    Left err -> Left err
    Right (JSONStruct { documents }) ->
      Right ( Just $ Prelude.fromIntegral $ Prelude.length documents
            , yieldMany documents .| mapC doc2hyperdoc )

doc2hyperdoc :: JSONStructDocument -> HyperdataDocument
doc2hyperdoc (JSONStructDocument { document = JSONDocument { hyperdata } }) = hyperdata
