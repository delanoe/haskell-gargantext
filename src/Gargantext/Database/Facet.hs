{-|
Module      : Gargantext.Database.Facet
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
------------------------------------------------------------------------
module Gargantext.Database.Facet
  where
------------------------------------------------------------------------

import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Either(Either(Left))
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Swagger
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Segment (jour)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Gargantext.Core.Types
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.NodeNgram
import Gargantext.Database.Schema.NodeNode
import Gargantext.Database.Utils
import Gargantext.Database.Queries.Join
import Gargantext.Database.Queries.Filter
import Opaleye
import Prelude hiding (null, id, map, sum, not, read)
import Servant.API
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary
import qualified Opaleye.Internal.Unpackspec()

------------------------------------------------------------------------
-- | DocFacet

-- data Facet = FacetDoc | FacetSources | FacetAuthors | FacetTerms
--    deriving (Show, Generic)
--instance FromJSON Facet
--instance ToJSON   Facet

type Favorite = Bool
type Title    = Text

type FacetDoc = Facet NodeId UTCTime Title HyperdataDocument Favorite Int
type FacetSources = FacetDoc
type FacetAuthors = FacetDoc
type FacetTerms   = FacetDoc



data Facet id created title hyperdata favorite ngramCount = 
     FacetDoc { facetDoc_id         :: id
              , facetDoc_created    :: created
              , facetDoc_title      :: title
              , facetDoc_hyperdata  :: hyperdata
              , facetDoc_favorite   :: favorite
              , facetDoc_ngramCount :: ngramCount
              } deriving (Show, Generic)

-- | JSON instance

$(deriveJSON (unPrefix "facetDoc_") ''Facet)

-- | Documentation instance
instance ToSchema FacetDoc

-- | Mock and Quickcheck instances

instance Arbitrary FacetDoc where
    arbitrary = elements [ FacetDoc id' (jour year 01 01) t hp fav ngramCount
                         | id'  <- [1..10]
                         , year <- [1990..2000]
                         , t    <- ["title", "another title"]
                         , hp   <- hyperdataDocuments
                         , fav  <- [True, False]
                         , ngramCount <- [3..100]
                         ]

-- Facets / Views for the Front End
-- | Database instances
$(makeAdaptorAndInstance "pFacetDoc" ''Facet)
$(makeLensesWith abbreviatedFields   ''Facet)

type FacetDocRead = Facet (Column PGInt4       )
                          (Column PGTimestamptz)
                          (Column PGText       )
                          (Column PGJsonb      )
                          (Column PGBool)
                          (Column PGInt4       )

-----------------------------------------------------------------------

data FacetChart = FacetChart { facetChart_time  :: UTCTime'
                             , facetChart_count :: Double
                        }
        deriving (Show, Generic)
$(deriveJSON (unPrefix "facetChart_") ''FacetChart)
instance ToSchema FacetChart

instance Arbitrary FacetChart where
    arbitrary = FacetChart <$> arbitrary <*> arbitrary

-----------------------------------------------------------------------
type Trash   = Bool
data OrderBy =  DateAsc | DateDesc
             | TitleAsc | TitleDesc
             | FavDesc  | FavAsc
             deriving (Generic, Enum, Bounded, Read, Show)
             -- | NgramCoun

instance FromHttpApiData OrderBy
  where
    parseUrlPiece "DateAsc"  = pure DateAsc
    parseUrlPiece "DateDesc" = pure DateDesc
    parseUrlPiece "TitleAsc" = pure TitleAsc
    parseUrlPiece "TitleDesc" = pure TitleDesc
    parseUrlPiece "FavAsc"   = pure FavAsc
    parseUrlPiece "FavDesc"   = pure FavDesc
    parseUrlPiece _           = Left "Unexpected value of OrderBy"

instance ToParamSchema OrderBy
instance FromJSON  OrderBy
instance ToJSON    OrderBy
instance ToSchema  OrderBy
instance Arbitrary OrderBy
  where
    arbitrary = elements [minBound..maxBound]


runViewAuthorsDoc :: Connection -> ContactId -> Trash -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> IO [FacetDoc]
runViewAuthorsDoc c cId t o l order = runQuery c (filterDocuments o l order $ viewAuthorsDoc cId t ntId)
  where
    ntId = NodeDocument

-- TODO add delete ?
viewAuthorsDoc :: ContactId -> Trash -> NodeType -> Query FacetDocRead
viewAuthorsDoc cId _ nt = proc () -> do
  (doc,(_,(_,(_,contact)))) <- queryAuthorsDoc      -< ()

  {-nn         <- queryNodeNodeTable -< ()
  restrict -< nodeNode_node1_id nn .== _node_id doc
  -- restrict -< nodeNode_delete   nn .== (pgBool t)
  -}

  restrict -< _node_id   contact   .== (toNullable $ pgInt4 cId)
  restrict -< _node_typename doc   .== (pgInt4 $ nodeTypeId nt)

  returnA  -< FacetDoc (_node_id doc) (_node_date doc) (_node_name doc) (_node_hyperdata doc) (pgBool True) (pgInt4 1)

queryAuthorsDoc :: Query (NodeRead, (NodeNgramReadNull, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull))))
queryAuthorsDoc = leftJoin5 queryNodeTable queryNodeNgramTable queryNgramsTable queryNodeNgramTable queryNodeTable cond12 cond23 cond34 cond45
    where
         cond12 :: (NodeNgramRead, NodeRead) -> Column PGBool
         cond12 (nodeNgram, doc) =  _node_id                  doc
                                .== nodeNgram_NodeNgramNodeId nodeNgram

         cond23 :: (NgramsRead, (NodeNgramRead, NodeReadNull)) -> Column PGBool
         cond23 (ngrams, (nodeNgram, _)) =  ngrams_id                  ngrams
                                        .== nodeNgram_NodeNgramNgramId nodeNgram
         
         cond34 :: (NodeNgramRead, (NgramsRead, (NodeNgramReadNull, NodeReadNull))) -> Column PGBool
         cond34 (nodeNgram2, (ngrams, (_,_)))= ngrams_id ngrams     .== nodeNgram_NodeNgramNgramId        nodeNgram2
         
         cond45 :: (NodeRead, (NodeNgramRead, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull)))) -> Column PGBool
         cond45 (contact, (nodeNgram2, (_, (_,_)))) = _node_id  contact    .== nodeNgram_NodeNgramNodeId         nodeNgram2


------------------------------------------------------------------------

runViewDocuments :: CorpusId -> Trash -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd [FacetDoc]
runViewDocuments cId t o l order = mkCmd $ \c -> runViewDocuments' c cId t o l order

-- | TODO use only Cmd with Reader and delete function below
runViewDocuments' :: Connection -> CorpusId -> Trash -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> IO [FacetDoc]
runViewDocuments' c cId t o l order = runQuery c ( filterDocuments o l order
                                                $ viewDocuments cId t ntId)
  where
    ntId = nodeTypeId NodeDocument

viewDocuments :: CorpusId -> Trash -> NodeTypeId -> Query FacetDocRead
viewDocuments cId t ntId = proc () -> do
  n  <- queryNodeTable     -< ()
  nn <- queryNodeNodeTable -< ()
  restrict -< _node_id          n  .== nodeNode_node2_id nn
  restrict -< nodeNode_node1_id nn .== (pgInt4 cId)
  restrict -< _node_typename    n  .== (pgInt4 ntId)
  restrict -< nodeNode_delete   nn .== (pgBool t)
  returnA  -< FacetDoc (_node_id n) (_node_date n) (_node_name n) (_node_hyperdata n) (nodeNode_favorite nn) (pgInt4 1)


------------------------------------------------------------------------

filterDocuments :: (PGOrd date, PGOrd title, PGOrd favorite) =>
     Maybe Gargantext.Core.Types.Offset
     -> Maybe Gargantext.Core.Types.Limit
     -> Maybe OrderBy
     -> Select (Facet id (Column date) (Column title) hyperdata (Column favorite) ngramCount)
     -> Query  (Facet id (Column date) (Column title) hyperdata (Column favorite) ngramCount)
filterDocuments o l order q = limit' l $ offset' o $ orderBy ordering q
  where
    ordering = case order of
      (Just DateAsc)   -> asc  facetDoc_created
      
      (Just TitleAsc)  -> asc  facetDoc_title
      (Just TitleDesc) -> desc facetDoc_title
      
      (Just FavAsc)    -> asc  facetDoc_favorite
      (Just FavDesc)   -> desc facetDoc_favorite
      _                -> desc facetDoc_created

