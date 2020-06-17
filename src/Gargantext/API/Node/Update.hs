{-|
Module      : Gargantext.API.Node.Update
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gargantext.API.Node.Update
      where

import Prelude (Enum, Bounded, minBound, maxBound)
import Data.Aeson
import Data.Swagger
import GHC.Generics (Generic)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

------------------------------------------------------------------------
data UpdateNodeParams = UpdateNodeParamsList  { methodList  :: Method      }
                      | UpdateNodeParamsGraph { methodGraph :: Metric      }
                      | UpdateNodeParamsTexts { methodTexts :: Granularity }
                      | UpdateNodeParamsBoard { methodBoard :: Charts      }
    deriving (Generic)

----------------------------------------------------------------------
data Method = Basic | Advanced | WithModel
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Metric = Order1 | Order2
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Granularity = NewNgrams | NewTexts | Both
    deriving (Generic, Eq, Ord, Enum, Bounded)

----------------------------------------------------------------------
data Charts = Sources | Authors | Institutes | Ngrams | All
    deriving (Generic, Eq, Ord, Enum, Bounded)

------------------------------------------------------------------------
-- TODO unPrefix "pn_" FromJSON, ToJSON, ToSchema, adapt frontend.
instance FromJSON  UpdateNodeParams
instance ToJSON    UpdateNodeParams
instance ToSchema  UpdateNodeParams
instance Arbitrary UpdateNodeParams where
  arbitrary = do
    l <- UpdateNodeParamsList  <$> arbitrary
    g <- UpdateNodeParamsGraph <$> arbitrary
    t <- UpdateNodeParamsTexts <$> arbitrary
    b <- UpdateNodeParamsBoard <$> arbitrary
    elements [l,g,t,b]

instance FromJSON  Method
instance ToJSON    Method
instance ToSchema  Method
instance Arbitrary Method where
  arbitrary = elements [ minBound .. maxBound ]

instance FromJSON  Metric
instance ToJSON    Metric
instance ToSchema  Metric
instance Arbitrary Metric where
  arbitrary = elements [ minBound .. maxBound ]

instance FromJSON  Granularity
instance ToJSON    Granularity
instance ToSchema  Granularity
instance Arbitrary Granularity where
  arbitrary = elements [ minBound .. maxBound ]

instance FromJSON  Charts
instance ToJSON    Charts
instance ToSchema  Charts
instance Arbitrary Charts where
  arbitrary = elements [ minBound .. maxBound ]

------------------------------------------------------------------------
api :: HasNodeError err
    => NodeId
    -> UpdateNodeParams
    -> Cmd err Int
api _nId (UpdateNodeParamsList  _meth) = pure 1
api _nId (UpdateNodeParamsGraph _meth) = pure 1
api _nId (UpdateNodeParamsTexts _meth) = pure 1
api _nId (UpdateNodeParamsBoard _meth) = pure 1

------------------------------------------------------------------------
type API = Summary " Share Node with username"
         :> ReqBody '[JSON] UpdateNodeParams
         :> Post    '[JSON] Int
