{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}  -- permit duplications for field names in multiple constructors
{-# LANGUAGE KindSignatures #-}  -- for use of Endpoint (name :: Symbol)
{-# LANGUAGE PartialTypeSignatures #-}  -- to automatically use suggested type hole signatures during compilation
{-# LANGUAGE TypeOperators #-}

module Gargantext.API.GraphQL where

import Data.ByteString.Lazy.Char8
  ( ByteString
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Morpheus
  ( App
  , deriveApp )
import Data.Morpheus.Server
  ( httpPlayground
  )
import Data.Morpheus.Subscriptions
  ( Event (..)
  , Hashable
  , httpPubApp
  )
import Data.Morpheus.Types
  ( GQLRequest
  , GQLResponse
  , GQLType
  , RootResolver(..)
  , Undefined(..)
  )
import Data.Proxy
import Gargantext.API.Admin.Auth.Types (AuthenticatedUser)
import Gargantext.API.Admin.Orchestrator.Types (JobLog)
import Gargantext.API.Prelude (HasJobEnv')
import qualified Gargantext.API.GraphQL.AsyncTask as GQLAT
import qualified Gargantext.API.GraphQL.IMT as GQLIMT
import qualified Gargantext.API.GraphQL.Node as GQLNode
import qualified Gargantext.API.GraphQL.User as GQLUser
import qualified Gargantext.API.GraphQL.UserInfo as GQLUserInfo
import qualified Gargantext.API.GraphQL.TreeFirstLevel as GQLTree
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Mail.Types (HasMail)
import Gargantext.Database.Prelude (HasConnectionPool, HasConfig)
import Gargantext.Prelude
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import qualified Prelude
import Servant
  ( (:<|>) (..)
  , (:>)
  , Accept (..)
  , Get
  , JSON
  , MimeRender (..)
  , Post
  , ReqBody
  ,  ServerT
  )
import qualified Servant.Auth as SA
import qualified Servant.Auth.Server as SAS
import Gargantext.API.Admin.Types (HasSettings)

-- | Represents possible GraphQL queries.
data Query m
  = Query
    { imt_schools :: GQLIMT.SchoolsArgs -> m [GQLIMT.School]
    , job_logs    :: GQLAT.JobLogArgs -> m (Map Int JobLog)
    , nodes       :: GQLNode.NodeArgs -> m [GQLNode.Node]
    , node_parent :: GQLNode.NodeParentArgs -> m [GQLNode.Node]
    , user_infos  :: GQLUserInfo.UserInfoArgs -> m [GQLUserInfo.UserInfo]
    , users       :: GQLUser.UserArgs -> m [GQLUser.User m]
    , tree        :: GQLTree.TreeArgs -> m GQLTree.TreeFirstLevel 
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
  :: (HasConnectionPool env, HasConfig env, HasMail env, HasJobEnv' env, HasSettings env)
  => RootResolver (GargM env GargError) e Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query { imt_schools = GQLIMT.resolveSchools
                            , job_logs    = GQLAT.resolveJobLogs
                            , nodes       = GQLNode.resolveNodes
                            , node_parent = GQLNode.resolveNodeParent
                            , user_infos  = GQLUserInfo.resolveUserInfos
                            , users       = GQLUser.resolveUsers
                            , tree        = GQLTree.resolveTree }
    , mutationResolver = Mutation { update_user_info = GQLUserInfo.updateUserInfo }
    , subscriptionResolver = Undefined }

-- | Main GraphQL "app".
app
  :: (Typeable env, HasConnectionPool env, HasConfig env, HasMail env, HasJobEnv' env, HasSettings env)
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
type API = SA.Auth '[SA.JWT, SA.Cookie] AuthenticatedUser
            :> "gql" :> (GQAPI :<|> Playground)

gqapi :: Proxy API
gqapi = Proxy

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
  :: (Typeable env, HasConnectionPool env, HasConfig env, HasMail env, HasJobEnv' env, HasSettings env)
  => ServerT API (GargM env GargError)
api (SAS.Authenticated _auser) = httpPubApp [] app :<|> pure httpPlayground
api _                          = panic "401 in graphql" -- SAS.throwAll (_ServerError # err401)
