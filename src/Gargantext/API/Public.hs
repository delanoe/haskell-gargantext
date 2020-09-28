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
import Data.Maybe (catMaybes)
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
    => Text -> Cmd err [PublicData]
api base = catMaybes
   <$> map (toPublicData base)
   <$> filterPublicDatas
   <$> selectPublic


selectPublic :: HasNodeError err
             => Cmd err [( Node HyperdataFolder, Maybe Int)] 
selectPublic = selectPublicNodes

  -- For tests only
  -- pure $ replicate 6 defaultPublicData

filterPublicDatas :: [(Node HyperdataFolder, Maybe Int)]
                  -> [(Node HyperdataFolder, [NodeId])]
filterPublicDatas datas =
  map (\(n,mi) ->
          let mi' = NodeId <$> mi in
                  ( _node_id n, (n, maybe [] (:[]) mi' ))
      ) datas
      & Map.fromListWith (\(n1,i1) (_n2,i2) -> (n1, i1 <> i2))
      & Map.filter (not . null . snd)
      & Map.elems

-- http://localhost:8008/api/v1.0/node/23543/file/download<Paste>
-- http://localhost:8000/images/Gargantextuel-212x300.jpg
toPublicData :: Text -> (Node HyperdataFolder, [NodeId]) -> Maybe PublicData
toPublicData base (n , mn) = PublicData <$> (hd ^? (_Just . hf_data . cf_title))
                                   <*> (hd ^? (_Just . hf_data . cf_desc ))
                                   <*> (Just $ url' mn) -- "images/Gargantextuel-212x300.jpg"
                                   <*> (Just $ url' mn)
                                   <*> Just (cs $ show $ utc2year (n^.node_date))
                                   <*> (hd ^? (_Just . hf_data . cf_query))
                                   <*> (hd ^? (_Just . hf_data . cf_authors))
  where
    hd = head
       $ filter (\(HyperdataField cd _ _) -> cd == JSON)
       $ n^. (node_hyperdata . hc_fields)
    url' :: [NodeId] -> Text 
    url' mn' = base 
           <>   "/node/" 
           <> (cs $ show $ (maybe 0 unNodeId $ head mn'))
           <> "/file/download"


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




