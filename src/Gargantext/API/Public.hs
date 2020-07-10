{-|
Module      : Gargantext.API.Public
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Gargantext.API.Public
      where

import Data.Text (Text)
import Data.List (replicate)
import Data.Aeson
import Data.Swagger
import GHC.Generics (Generic)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Prelude
-- import Gargantext.Database.Admin.Types.Node
-- import Gargantext.Database.Query.Table.NodeNode (selectPublicNodes)
-- import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

------------------------------------------------------------------------
type API = Summary " Public API"
         :> Get '[JSON] [PublicData]

api :: HasNodeError err
    => Cmd err [PublicData]
api = do
  pure $ replicate 6 defaultPublicData

{-
toPublicData :: (Node HyperdataFolder, Maybe Int) -> Maybe PublicData
toPublicData (n, mn) = Just $ PublicData t a i u d db au
  where
    d = _node_date n
    t = _node_name n
-}


data PublicData = PublicData
  { title    :: Text
  , abstract :: Text
  , img      :: Text
  , url      :: Text
  , date     :: Text
  , database :: Text
  , author   :: Text
  } | NoData { nodata:: Text}
  deriving (Generic)


instance FromJSON  PublicData where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToJSON    PublicData where
  toJSON = genericToJSON (defaultOptions { sumEncoding = ObjectWithSingleField })

instance ToSchema  PublicData
instance Arbitrary PublicData where
  arbitrary = elements
            $ replicate 6 defaultPublicData 

defaultPublicData :: PublicData
defaultPublicData =
  PublicData "Title"
             (foldl (<>) "" $ replicate 100 "abstract ")
             "images/Gargantextuel-212x300.jpg"
             "https://.."
             "YY/MM/DD"
             "database"
             "Author"




