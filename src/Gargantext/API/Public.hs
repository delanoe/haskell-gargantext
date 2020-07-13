{-|
Module      : Gargantext.API.Public
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.API.Public
      where

import Control.Lens ((^?), (^.), _Just)
import Data.Maybe (maybe, catMaybes)
import Data.Tuple (snd)
import Data.Text (Text)
import Data.List (replicate, null)
import Data.Aeson
import Data.Swagger
import GHC.Generics (Generic)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Prelude
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Query.Table.NodeNode (selectPublicNodes)
import Gargantext.Core.Utils.DateUtils (utc2year)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Schema.Node -- (NodePoly(..))
import Gargantext.Prelude
import Servant
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import qualified Data.Map as Map

------------------------------------------------------------------------
type API = Summary " Public API"
         :> Get '[JSON] [PublicData]

api :: HasNodeError err
    => Cmd err [PublicData]
api = catMaybes <$> map toPublicData <$> filterPublicDatas <$> selectPublic


selectPublic :: HasNodeError err
             => Cmd err [( Node HyperData, Maybe Int)] 
selectPublic = selectPublicNodes

  -- | For tests only
  -- pure $ replicate 6 defaultPublicData

filterPublicDatas :: [( Node HyperData, Maybe Int)] -> [(Node HyperData, [NodeId])]
filterPublicDatas datas = map (\(n,mi) -> let mi' = NodeId <$> mi in
                                              ( _node_id n, (n, maybe [] (:[]) mi' ))
                              ) datas
                        & Map.fromListWith (\(n1,i1) (_n2,i2) -> (n1, i1 <> i2))
                        & Map.filter (not . null . snd)
                        & Map.elems


toPublicData :: (Node HyperData, [NodeId]) -> Maybe PublicData
toPublicData (n , _mn) = PublicData <$> (hd ^? (_Just . hf_data . cf_title))
                                   <*> (hd ^? (_Just . hf_data . cf_desc))
                                   <*> Just "images/Gargantextuel-212x300.jpg"
                                   <*> Just "https://.."
                                   <*> Just (cs $ show $ utc2year (n^.node_date))
                                   <*> (hd ^? (_Just . hf_data . cf_query))
                                   <*> (hd ^? (_Just . hf_data . cf_authors))
  where
    hd = head
       $ filter (\(HyperdataField cd _ _) -> cd == JSON)
       $ n^. (node_hyperdata . hd_fields)


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




