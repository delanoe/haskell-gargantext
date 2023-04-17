{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.NLP
  ( Lang(..)
  , LanguagesArgs(..)
  , resolveLanguages
  )
  where

import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , QUERY
  )
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core (Lang(..), allLangs)
import Gargantext.Prelude
import GHC.Generics (Generic)

data LanguagesArgs
  = LanguagesArgs
    { } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

resolveLanguages
  :: LanguagesArgs -> GqlM e env [Lang]
resolveLanguages LanguagesArgs { } = pure $ allLangs
