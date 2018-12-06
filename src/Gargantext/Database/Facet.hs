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
import Data.Profunctor.Product.Default
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
import Gargantext.Database.Ngrams
import Gargantext.Database.Node
import Gargantext.Database.NodeNgram
import Gargantext.Database.NodeNode
import Gargantext.Database.Queries
import Opaleye
import Opaleye.Internal.Join (NullMaker)
import Prelude (Enum, Bounded, minBound, maxBound)
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




------------------------------------------------------------------------
-- | TODO move this queries utilties elsewhere

leftJoin3' :: Query (NodeRead, (NodeNodeReadNull, NodeReadNull))
leftJoin3' = leftJoin3 queryNodeNodeTable queryNodeTable queryNodeTable cond12 cond23
    where
         cond12 = undefined
         cond23 :: (NodeRead, (NodeNodeRead, NodeReadNull)) -> Column PGBool
         cond23 = undefined


leftJoin3 :: ( Default Unpackspec columnsL1 columnsL1
             , Default Unpackspec columnsL2 columnsL2
             , Default Unpackspec columnsL3 columnsL3
             
             , Default Unpackspec nullableColumnsL2 nullableColumnsL2
             
             , Default NullMaker  columnsL2  nullableColumnsL2
             , Default NullMaker (columnsL1, nullableColumnsL2) nullableColumnsL3
             )
             =>
              Query columnsL1 -> Query columnsL2 -> Query columnsL3
                -> ((columnsL1, columnsL2) -> Column PGBool)
                -> ((columnsL3, (columnsL1, nullableColumnsL2)) -> Column PGBool)
                -> Query (columnsL3, nullableColumnsL3)
leftJoin3 q1 q2 q3 cond12 cond23 = leftJoin q3 (leftJoin q1 q2 cond12) cond23

--{-

leftJoin4' :: Query (NodeRead, (NodeReadNull, (NodeReadNull, NodeReadNull)))
leftJoin4' = leftJoin4 queryNodeTable queryNodeTable queryNodeTable queryNodeTable cond12 cond23 cond34
    where
         cond12 = undefined
         
         cond23 :: (NodeRead, (NodeRead, NodeReadNull)) -> Column PGBool
         cond23 = undefined
         
         cond34 :: (NodeRead, (NodeRead, (NodeReadNull, NodeReadNull))) -> Column PGBool
         cond34 = undefined


leftJoin4 :: ( Default Unpackspec fieldsL1 fieldsL1,
               Default Unpackspec fieldsL2 fieldsL2,
               Default Unpackspec fieldsL3 fieldsL3,
               Default Unpackspec fieldsR fieldsR,
               
               Default Unpackspec nullableFieldsL1 nullableFieldsL1,
               Default Unpackspec nullableFieldsL2 nullableFieldsL2,
               Default NullMaker fieldsR nullableFieldsL2,
               Default NullMaker (fieldsL2, nullableFieldsL1) nullableFieldsL3,
               Default NullMaker (fieldsL3, nullableFieldsL2) nullableFieldsL1) =>
     Query fieldsL3
     -> Query fieldsR
     -> Query fieldsL2
     -> Query fieldsL1
     -> ((fieldsL3, fieldsR)
         -> Column PGBool)
     -> ((fieldsL2, (fieldsL3, nullableFieldsL2))
         -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsL1))
         -> Column PGBool)
     -> Query (fieldsL1, nullableFieldsL3)
leftJoin4 q1 q2 q3 q4 cond12 cond23 cond34 = leftJoin q4 (leftJoin q3 (leftJoin q1 q2 cond12) cond23) cond34
--}

{-
-}
leftJoin5' :: Query (NodeRead, (NodeReadNull, (NodeReadNull, (NodeReadNull, NodeReadNull))))
leftJoin5' = leftJoin5 queryNodeTable queryNodeTable queryNodeTable queryNodeTable queryNodeTable cond12 cond23 cond34 cond45
    where
         cond12 :: (NodeRead, NodeRead) -> Column PGBool
         cond12 = undefined
         
         cond23 :: (NodeRead, (NodeRead, NodeReadNull)) -> Column PGBool
         cond23 = undefined
         
         cond34 :: (NodeRead, (NodeRead, (NodeReadNull, NodeReadNull))) -> Column PGBool
         cond34 = undefined
         
         cond45 :: (NodeRead, (NodeRead, (NodeReadNull, (NodeReadNull, NodeReadNull)))) -> Column PGBool
         cond45 = undefined


leftJoin5 :: ( Default Unpackspec fieldsL1 fieldsL1,
               Default Unpackspec fieldsL2 fieldsL2,
               Default Unpackspec nullableFieldsR1 nullableFieldsR1,
               Default Unpackspec fieldsL3 fieldsL3,
               Default Unpackspec nullableFieldsR2 nullableFieldsR2,
               Default Unpackspec fieldsL4 fieldsL4,
               Default Unpackspec nullableFieldsR3 nullableFieldsR3,
               Default Unpackspec fieldsR fieldsR,
               Default NullMaker fieldsR nullableFieldsR3,
               Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR4,
               Default NullMaker (fieldsL3, nullableFieldsR2) nullableFieldsR1,
               Default NullMaker (fieldsL4, nullableFieldsR3) nullableFieldsR2) =>
               Query fieldsR
               -> Query fieldsL4
               -> Query fieldsL3
               -> Query fieldsL2
               -> Query fieldsL1
               -> ((fieldsL4, fieldsR) -> Column PGBool)
               -> ((fieldsL3, (fieldsL4, nullableFieldsR3)) -> Column PGBool)
               -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
               -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
               -> Query (fieldsL1, nullableFieldsR4)
leftJoin5 q1 q2 q3 q4 q5 cond12 cond23 cond34 cond45 = leftJoin q5 (leftJoin q4 (leftJoin q3 (leftJoin q2 q1 cond12) cond23) cond34) cond45


leftJoin6 :: ( Default Unpackspec fieldsL1 fieldsL1,
               Default Unpackspec fieldsL2 fieldsL2,
               Default Unpackspec nullableFieldsR1 nullableFieldsR1,
               Default Unpackspec fieldsL3 fieldsL3,
               Default Unpackspec nullableFieldsR2 nullableFieldsR2,
               Default Unpackspec fieldsL4 fieldsL4,
               Default Unpackspec nullableFieldsR3 nullableFieldsR3,
               Default Unpackspec fieldsL5 fieldsL5,
               Default Unpackspec nullableFieldsR4 nullableFieldsR4,
               Default Unpackspec fieldsR fieldsR,
               Default NullMaker fieldsR nullableFieldsR4,
               Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR5,
               Default NullMaker (fieldsL3, nullableFieldsR2) nullableFieldsR1,
               Default NullMaker (fieldsL4, nullableFieldsR3) nullableFieldsR2,
               Default NullMaker (fieldsL5, nullableFieldsR4) nullableFieldsR3) =>
     Query fieldsR
     -> Query fieldsL5
     -> Query fieldsL4
     -> Query fieldsL3
     -> Query fieldsL2
     -> Query fieldsL1 -> ((fieldsL5, fieldsR) -> Column PGBool)
     -> ((fieldsL4, (fieldsL5, nullableFieldsR4)) -> Column PGBool)
     -> ((fieldsL3, (fieldsL4, nullableFieldsR3)) -> Column PGBool)
     -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
     -> Query (fieldsL1, nullableFieldsR5)
leftJoin6 q1 q2 q3 q4 q5 q6 cond12 cond23 cond34 cond45 cond56 =
  leftJoin q6 (leftJoin q5 (leftJoin q4 (leftJoin q3 (leftJoin q2 q1 cond12) cond23) cond34) cond45) cond56


