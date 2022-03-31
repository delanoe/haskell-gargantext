{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Gargantext.API.GraphQL.IMT
  ( School(..)
  , SchoolsArgs(..)
  , resolveSchools
  )
  where

import Data.Morpheus.Types
  ( GQLType
  , Resolver
  , QUERY
  )
import Gargantext.API.Prelude (GargM, GargError)
import Gargantext.Core.Ext.IMT (School(..), schools)
import Gargantext.Prelude
import GHC.Generics (Generic)

data SchoolsArgs
  = SchoolsArgs
    { } deriving (Generic, GQLType)

type GqlM e env = Resolver QUERY e (GargM env GargError)

resolveSchools
  :: SchoolsArgs -> GqlM e env [School]
resolveSchools SchoolsArgs { } = pure $ schools
