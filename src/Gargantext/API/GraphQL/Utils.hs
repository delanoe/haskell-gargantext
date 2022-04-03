module Gargantext.API.GraphQL.Utils where

import Data.Morpheus.Types (GQLTypeOptions, fieldLabelModifier)
import qualified Data.Text as T
import Gargantext.Core.Utils.Prefix (unCapitalize, dropPrefix)
import Gargantext.Prelude
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Gargantext.API.Admin.Types (jwtSettings, HasSettings (settings))
import Servant.Auth.Server (verifyJWT, JWTSettings)
import Control.Lens.Getter (view)
import Gargantext.Database.Prelude (Cmd')
import Gargantext.API.Admin.Auth.Types (AuthenticatedUser (AuthenticatedUser, _authUser_id))
import Data.ByteString (ByteString)
import Gargantext.Database.Admin.Types.Node (unNodeId)

unPrefix :: T.Text -> GQLTypeOptions -> GQLTypeOptions
unPrefix prefix options = options { fieldLabelModifier = nflm }
  where
    nflm label = unCapitalize $ dropPrefix (T.unpack prefix) $ ( fieldLabelModifier options ) label

data AuthStatus = Valid | Invalid

authUser :: (HasSettings env) => Int -> Text -> Cmd' env err AuthStatus
authUser ui_id token = do
  let token' = encodeUtf8 token
  jwtS <- view $ settings . jwtSettings
  u <- getUserFromToken jwtS token'
  case u of
    Nothing -> pure Invalid
    Just au -> 
      if nId au == ui_id
        then pure Valid
        else pure Invalid
        where
          nId AuthenticatedUser {_authUser_id} = unNodeId _authUser_id

getUserFromToken :: JWTSettings -> ByteString -> IO (Maybe AuthenticatedUser)
getUserFromToken = verifyJWT

