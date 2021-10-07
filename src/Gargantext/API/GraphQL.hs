{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}  -- permit duplications for field names in multiple constructors
{-# LANGUAGE KindSignatures #-}  -- for use of Endpoint (name :: Symbol)
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Gargantext.API.GraphQL where

import Data.Morpheus
  ( App
  , deriveApp )
import Data.Morpheus.Subscriptions
  ( Event (..)
  , Hashable
  , webSocketsApp
  )
import Data.Morpheus.Types
  ( GQLType
  -- , ResolverM
  , ResolverQ
  , RootResolver (..)
  , publish
  -- , subscribe
  )
import Data.Text (Text)
import GHC.Generics (Generic)

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Morpheus.Server
  ( httpPlayground
  )
import Data.Morpheus.Subscriptions
  ( PubApp
  , SubApp
  , httpPubApp
  )
import Data.Morpheus.Types
  ( -- App
    GQLRequest
  , GQLResponse
  , Undefined (..)
  , liftEither
  , render
  )
-- import Data.Proxy (Proxy)
-- import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
-- import Network.Wai.Handler.Warp
--   ( defaultSettings,
--     runSettings,
--     setPort,
--   )
-- import Network.Wai.Handler.WebSockets
--   ( websocketsOr,
--   )
import Network.WebSockets
  ( ServerApp,
--    defaultConnectionOptions,
  )
import Servant
  ( (:<|>) (..),
    (:>),
    Accept (..),
    Get,
--    HasServer,
    JSON,
    MimeRender (..),
    PlainText,
    Post,
    ReqBody,
    Server,
--    serve,
  )
import Prelude

import qualified Data.Swagger as Swagger
import Gargantext.Database.Prelude (Cmd)

type EVENT = Event Channel Contet

data Query m
  = Query
    { user :: UserArgs -> m User
    } deriving (Generic, GQLType)

data Channel
  = Update
  | New
  deriving (Eq, Show, Generic, Hashable)

data Contet
  = UserContet User
data User
  = User
    { name    :: Text
    , user_id :: Int
    } deriving (Generic, GQLType)

data UserArgs
  = UserArgs
    { name :: Text
    } deriving (Generic, GQLType)

resolveUser :: UserArgs -> ResolverQ e IO User
resolveUser UserArgs { name } = liftEither $ dbUser name

dbUser :: Text -> IO (Either String User)
dbUser name = pure $ Right $ User { name, user_id = 1 }

rootResolver :: RootResolver IO EVENT Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query { user = resolveUser }
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined }

app :: App EVENT IO
app = deriveApp rootResolver

----------------------------------------------

data HTML deriving (Typeable)
instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]
instance MimeRender HTML ByteString where
  mimeRender _ = id

type GQAPI = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
type Schema = "schema" :> Get '[PlainText] Text
type Playground = Get '[HTML] ByteString
type API' (name :: Symbol) = name :> (GQAPI :<|> Schema :<|> Playground)
type API = "gql" :> (GQAPI :<|> Playground)

-- serveEndpoint ::
--   ( SubApp ServerApp e
--   , PubApp e
--   ) =>
--   [e -> IO ()] ->
--   App e IO ->
--   Server (API name)
-- serveEndpoint publish app' = (liftIO . httpPubApp publish app') :<|> withSchema app' :<|> pure httpPlayground
-- 
-- withSchema :: (Applicative f) => App e m -> f Text
-- withSchema = pure . LT.toStrict . decodeUtf8 . render

api :: Server API
api = do
  --(wsApp, publish') <- liftIO $ webSocketsApp app
  (liftIO . httpPubApp [] app) :<|> pure httpPlayground
