{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.NLP
  ( Lang(..)
  , LanguagesArgs(..)
  , LanguagesMap
  , resolveLanguages
  )
  where

import Control.Lens (view)
import qualified Data.Map.Strict as Map
import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , QUERY
  )
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core (Lang(..), NLPServerConfig(..), PosTagAlgo)  -- , allLangs)
import Gargantext.Core.NLP (HasNLPServer(..))
import Gargantext.Prelude
import Protolude

data LanguagesArgs
  = LanguagesArgs
    { } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

type LanguagesMap = Map.Map Lang NLPServer

data NLPServer = NLPServer
  {
    server :: !PosTagAlgo
  , url    :: !Text
  }
  deriving (Show, Eq, Generic, GQLType)

resolveLanguages
  :: HasNLPServer env => LanguagesArgs -> GqlM e env LanguagesMap
resolveLanguages LanguagesArgs { } = do
  -- pure $ allLangs
  lift $ do
    ns <- view nlpServer
    printDebug "[resolveLanguages] nlpServer" ns
    pure $ Map.map (\(NLPServerConfig { .. }) -> NLPServer { server
                                                           , url = Protolude.show url }) ns
