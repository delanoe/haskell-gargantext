{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}  -- permit duplications for field names in multiple constructors
{-# LANGUAGE KindSignatures #-}  -- for use of Endpoint (name :: Symbol)
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Gargantext.API.GraphQL where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Morpheus
  ( App
  , deriveApp )
import Data.Morpheus.Server
  ( httpPlayground
  )
import Data.Morpheus.Subscriptions
  ( Event (..)
  , Hashable
  , PubApp
  , SubApp
  , httpPubApp
  , webSocketsApp
  )
import Data.Morpheus.Types
  ( GQLRequest
  , GQLResponse
  , GQLType
  , ResolverQ
  , RootResolver(..)
  , Undefined
  , liftEither
  , publish
  , render
  )
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
import Network.WebSockets
  ( ServerApp,
  )
import Servant
  ( (:<|>) (..),
    (:>),
    Accept (..),
    Get,
    JSON,
    MimeRender (..),
    PlainText,
    Post,
    ReqBody,
    Server,
  )
import Prelude
import Gargantext.Database.Prelude (Cmd)

-- | Our simple datatype.
data User
  = User
    { name    :: Text
    , user_id :: Int
    } deriving (Generic, GQLType)

-- | Represents possible GraphQL queries.
data Query m
  = Query
    { user :: UserArgs -> m User
    } deriving (Generic, GQLType)

-- | Arguments to the "user" query.
data UserArgs
  = UserArgs
    { name :: Text
    } deriving (Generic, GQLType)

-- | Possible GraphQL Events, i.e. here we describe how we will
-- manipulate the data.
type EVENT = Event Channel Contet

-- | Channels are possible actions to call when manipulating the data.
data Channel
  = Update
  | New
  deriving (Eq, Show, Generic, Hashable)

-- | This type describes what data we will operate on.
data Contet
  = UserContet User


-- | The main GraphQL resolver: how queries, mutations and
-- subscriptions are handled.
rootResolver :: RootResolver IO EVENT Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query { user = resolveUser }
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined }

-- | Function to resolve user from a query.
resolveUser :: UserArgs -> ResolverQ e IO User
resolveUser UserArgs { name } = liftEither $ dbUser name

-- | Inner function to fetch the user from DB.
dbUser :: Text -> IO (Either String User)
dbUser name = pure $ Right $ User { name, user_id = 1 }

-- | Main GraphQL "app".
app :: App EVENT IO
app = deriveApp rootResolver

----------------------------------------------

-- Now for some boilerplate to integrate the above GraphQL app with
-- servant.

-- | HTML type is needed for the GraphQL Playground.
data HTML deriving (Typeable)
instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]
instance MimeRender HTML ByteString where
  mimeRender _ = id

-- | Servant route for the app we defined above.
type GQAPI = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
-- type Schema = "schema" :> Get '[PlainText] Text
-- | Servant route for the playground.
type Playground = Get '[HTML] ByteString
-- type API' (name :: Symbol) = name :> (GQAPI :<|> Schema :<|> Playground)
-- | Our API consists of `GQAPI` and `Playground`.
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

-- | Implementation of our API.
api :: Server API
api = do
  --(wsApp, publish') <- liftIO $ webSocketsApp app
  (liftIO . httpPubApp [] app) :<|> pure httpPlayground
