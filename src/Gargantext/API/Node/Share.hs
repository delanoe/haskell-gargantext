{-|
Module      : Gargantext.API.Node.Share
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Node.Share
      where

import Data.Aeson
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Gargantext.API.Prelude
import Gargantext.Core.NLP (HasNLPServer)
import Gargantext.Core.Types.Individu (User(..), arbitraryUsername)
import Gargantext.Database.Action.Share (ShareNodeWith(..))
import Gargantext.Database.Action.Share as DB (shareNodeWith, unPublish)
import Gargantext.Database.Action.User
import Gargantext.Database.Action.User.New
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Tree (findNodesWithType)
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Gargantext.Utils.Aeson as GUA

------------------------------------------------------------------------
data ShareNodeParams = ShareTeamParams   { username :: Text  }
                     | SharePublicParams { node_id  :: NodeId}
  deriving (Generic)
------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  ShareNodeParams where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = GUA.defaultTaggedObject })
instance ToJSON    ShareNodeParams where
  toJSON = genericToJSON (defaultOptions { sumEncoding = GUA.defaultTaggedObject })
instance ToSchema  ShareNodeParams
instance Arbitrary ShareNodeParams where
  arbitrary = elements [ ShareTeamParams "user1"
                       , SharePublicParams (NodeId 1)
                       ]
------------------------------------------------------------------------
-- TODO permission
-- TODO refactor userId which is used twice
-- TODO change return type for better warning/info/success/error handling on the front
api :: (HasNodeError err, HasNLPServer env, CmdRandom env err m)
    => User
    -> NodeId
    -> ShareNodeParams
    -> m Int
api userInviting nId (ShareTeamParams user') = do
  let user'' = Text.toLower user'
  user <- case guessUserName user'' of
    Nothing    -> pure user''
    Just (u,_) -> do
      isRegistered <- getUserId' (UserName u)
      case isRegistered of
        Just _  -> do
          -- printDebug "[G.A.N.Share.api]" ("Team shared with " <> u)
          pure u
        Nothing -> do
          username' <- getUsername userInviting
          _ <- case List.elem username' arbitraryUsername of
            True  -> do
              -- printDebug "[G.A.N.Share.api]" ("Demo users are not allowed to invite" :: Text)
              pure ()
            False -> do
              -- TODO better analysis of the composition of what is shared
              children <- findNodesWithType nId [NodeList] [ NodeFolderShared
                                                           , NodeTeam
                                                           , NodeFolder
                                                           , NodeCorpus
                                                           ]
              _ <- case List.null children of
                True -> do
                  -- printDebug "[G.A.N.Share.api]" ("Invitation is enabled if you share a corpus at least" :: Text)
                  pure 0
                False -> do
                  -- printDebug "[G.A.N.Share.api]" ("Your invitation is sent to: " <> user'')
                  newUsers [user'']
              pure ()
          pure u

  fromIntegral <$> DB.shareNodeWith (ShareNodeWith_User NodeFolderShared (UserName user)) nId
api _uId nId2 (SharePublicParams nId1) =

  fromIntegral <$> DB.shareNodeWith (ShareNodeWith_Node NodeFolderPublic nId1) nId2

------------------------------------------------------------------------
type API = Summary " Share Node with username"
         :> ReqBody '[JSON] ShareNodeParams
         :> Post    '[JSON] Int

------------------------------------------------------------------------
type Unpublish = Summary " Unpublish Node"
               :> Capture "node_id" NodeId
               :> Put '[JSON] Int

unPublish :: NodeId -> GargServer Unpublish
unPublish n = DB.unPublish n
