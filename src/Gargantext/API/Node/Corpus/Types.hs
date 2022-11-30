{-# LANGUAGE TemplateHaskell    #-}

module Gargantext.API.Node.Corpus.Types where

import Control.Lens hiding (elements, Empty)
import Control.Monad.Fail (fail)
import Control.Monad.Reader (MonadReader)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Monoid (mempty)
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

import Protolude ((++))
import Gargantext.Prelude

import qualified Gargantext.API.Admin.Orchestrator.Types as T
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Action.Flow (DataOrigin(..))
import Gargantext.Database.Prelude (HasConfig(..))
import Gargantext.Prelude.Config (gc_pubmed_api_key)

data Database = Empty
              | PubMed
              | Arxiv
              | HAL
              | IsTex
              | Isidore
  deriving (Eq, Show, Generic)

deriveJSON (unPrefix "") ''Database
instance ToSchema Database

database2origin :: ( MonadReader env m
                   , HasConfig env ) => Database -> m DataOrigin
database2origin Empty   = pure $ InternalOrigin T.IsTex
database2origin PubMed  = do
  pubmed_api_key <- view $ hasConfig . gc_pubmed_api_key

  pure $ ExternalOrigin $ T.PubMed { mAPIKey = Just pubmed_api_key }
database2origin Arxiv   = pure $ ExternalOrigin T.Arxiv
database2origin HAL     = pure $ ExternalOrigin T.HAL
database2origin IsTex   = pure $ ExternalOrigin T.IsTex
database2origin Isidore = pure $ ExternalOrigin T.Isidore

------------------------------------------------------------------------
data Datafield = Gargantext
               | External (Maybe Database)
               | Web
               | Files
  deriving (Eq, Show, Generic)

instance FromJSON Datafield where
  parseJSON = withText "Datafield" $ \text ->
    case text of
      "Gargantext" -> pure Gargantext
      "Web" -> pure Web
      "Files" -> pure Files
      v ->
        let (preExternal, _, postExternal) = v =~ ("External " :: Text) :: (Text, Text, Text)
        in
        if preExternal == "" then do
          db <- parseJSON $ String postExternal
          pure $ External db
        else fail $ "Cannot match patterh 'External <db>' for string " ++ (T.unpack v)
instance ToJSON Datafield where
  toJSON (External db) = toJSON $ "External " ++ (show db)
  toJSON s = toJSON $ show s
instance ToSchema Datafield where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Datafield") $ mempty
      & type_ ?~ SwaggerObject
