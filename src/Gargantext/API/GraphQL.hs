{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}  -- permit duplications for field names in multiple constructors
{-# LANGUAGE KindSignatures #-}  -- for use of Endpoint (name :: Symbol)
{-# LANGUAGE PartialTypeSignatures #-}  -- to automatically use suggested type hole signatures during compilation
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Gargantext.API.GraphQL where

import Control.Monad.Base (liftBase)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
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
  , Undefined(..)
  , lift
  , liftEither
  , publish
  , render
  )
import Data.Morpheus.Types.Internal.AST
  ( msg )
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import qualified Gargantext.API.GraphQL.User as GQLUser
import qualified Gargantext.API.GraphQL.UserInfo as GQLUserInfo
import Gargantext.API.Prelude (GargServerT, GargM, GargError)
import Gargantext.Database.Prelude (Cmd, HasConnectionPool, HasConfig)
import Gargantext.Database.Schema.User (UserPoly(..))
import Gargantext.Prelude
import GHC.Generics (Generic)
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
import Network.WebSockets
  ( ServerApp,
  )
import qualified Prelude as Prelude
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
    ServerT,
  )

-- | Represents possible GraphQL queries.
data Query m
  = Query
    { user_infos :: GQLUserInfo.UserInfoArgs -> m [GQLUserInfo.UserInfo]
    , users :: GQLUser.UserArgs -> m [GQLUser.User m]
    } deriving (Generic, GQLType)

data Mutation m
  = Mutation
    { update_user_info :: GQLUserInfo.UserInfoMArgs -> m Int }
    deriving (Generic, GQLType)

-- | Possible GraphQL Events, i.e. here we describe how we will
-- manipulate the data.
type EVENT m = Event Channel (Contet m)

-- | Channels are possible actions to call when manipulating the data.
data Channel
  = Update
  | New
  deriving (Eq, Show, Generic, Hashable)

-- | This type describes what data we will operate on.
data Contet m
  = UserContet [GQLUser.User m]
  | UserInfoContet [GQLUserInfo.UserInfo]

-- | The main GraphQL resolver: how queries, mutations and
-- subscriptions are handled.
rootResolver
  :: (HasConnectionPool env, HasConfig env)
  => RootResolver (GargM env GargError) e Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query { user_infos = GQLUserInfo.resolveUserInfos
                            , users = GQLUser.resolveUsers }
    , mutationResolver = Mutation { update_user_info = GQLUserInfo.updateUserInfo }
    , subscriptionResolver = Undefined }

-- | Main GraphQL "app".
app
  :: (Typeable env, HasConnectionPool env, HasConfig env)
  => App (EVENT (GargM env GargError)) (GargM env GargError)
app = deriveApp rootResolver

----------------------------------------------

-- Now for some boilerplate to integrate the above GraphQL app with
-- servant.

-- | HTML type is needed for the GraphQL Playground.
data HTML deriving (Typeable)
instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]
instance MimeRender HTML ByteString where
  mimeRender _ = Prelude.id

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
--api :: Server API
api
  :: (Typeable env, HasConnectionPool env, HasConfig env)
  => ServerT API (GargM env GargError)
api = httpPubApp [] app :<|> pure httpPlayground
