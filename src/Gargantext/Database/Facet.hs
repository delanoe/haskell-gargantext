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
{-# LANGUAGE RankNTypes                #-}
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
{- | TODO after demo
data Facet id date hyperdata score = 
     FacetDoc { facetDoc_id        :: id
              , facetDoc_date      :: date
              , facetDoc_hyperdata :: hyperdata
              , facetDoc_score     :: score
              } deriving (Show, Generic)
-}

data Pair i l = Pair {_p_id    :: i
                     ,_p_label :: l
  } deriving (Show, Generic)
$(deriveJSON (unPrefix "_p_") ''Pair)
$(makeAdaptorAndInstance "pPair" ''Pair)

instance (ToSchema i, ToSchema l) => ToSchema (Pair i l) where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 3 fieldLabel}
instance (Arbitrary i, Arbitrary l) => Arbitrary (Pair i l) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

data FacetPaired id date hyperdata score pairs =
  FacetPaired {_fp_id        :: id
              ,_fp_date      :: date
              ,_fp_hyperdata :: hyperdata
              ,_fp_score     :: score
              ,_fp_pairs     :: pairs
  } deriving (Show, Generic)
$(deriveJSON (unPrefix "_fp_") ''FacetPaired)
$(makeAdaptorAndInstance "pFacetPaired" ''FacetPaired)

instance (ToSchema id, ToSchema date, ToSchema hyperdata, ToSchema pairs, ToSchema score) => ToSchema (FacetPaired id date hyperdata score pairs) where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions {fieldLabelModifier = \fieldLabel -> drop 4 fieldLabel}

instance ( Arbitrary id
         , Arbitrary date
         , Arbitrary hyperdata
         , Arbitrary score
         , Arbitrary pairs
         ) => Arbitrary (FacetPaired id date hyperdata score pairs) where
  arbitrary = FacetPaired <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

--{-
type FacetPairedRead = FacetPaired (Column PGInt4       )
                                   (Column PGTimestamptz)
                                   (Column PGJsonb      )
                                   (Column PGInt4       )
                                   (Pair (Column (Nullable PGInt4)) (Column (Nullable PGText)))
--}



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
                         , hp   <- arbitraryHyperdataDocuments
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
data OrderBy =  DateAsc   | DateDesc
             | TitleAsc   | TitleDesc
             | ScoreDesc  | ScoreAsc
             deriving (Generic, Enum, Bounded, Read, Show)
             -- | NgramCoun

instance FromHttpApiData OrderBy
  where
    parseUrlPiece "DateAsc"  = pure DateAsc
    parseUrlPiece "DateDesc" = pure DateDesc
    parseUrlPiece "TitleAsc" = pure TitleAsc
    parseUrlPiece "TitleDesc" = pure TitleDesc
    parseUrlPiece "ScoreAsc"   = pure ScoreAsc
    parseUrlPiece "ScoreDesc"  = pure ScoreDesc
    parseUrlPiece _           = Left "Unexpected value of OrderBy"

instance ToParamSchema OrderBy
instance FromJSON  OrderBy
instance ToJSON    OrderBy
instance ToSchema  OrderBy
instance Arbitrary OrderBy
  where
    arbitrary = elements [minBound..maxBound]


runViewAuthorsDoc :: ContactId -> Trash -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd err [FacetDoc]
runViewAuthorsDoc cId t o l order = runOpaQuery $ filterWith o l order $ viewAuthorsDoc cId t ntId
  where
    ntId = NodeDocument

-- TODO add delete ?
viewAuthorsDoc :: ContactId -> Trash -> NodeType -> Query FacetDocRead
viewAuthorsDoc cId _ nt = proc () -> do
  (doc,(_,(_,(_,contact)))) <- queryAuthorsDoc      -< ()

  {-nn         <- queryNodeNodeTable -< ()
  restrict -< nn_node1_id nn .== _node_id doc
  -- restrict -< nn_delete   nn .== (pgBool t)
  -}

  restrict -< _node_id   contact   .== (toNullable $ pgNodeId cId)
  restrict -< _node_typename doc   .== (pgInt4 $ nodeTypeId nt)

  returnA  -< FacetDoc (_node_id doc) (_node_date doc) (_node_name doc) (_node_hyperdata doc) (pgBool True) (pgInt4 1)

queryAuthorsDoc :: Query (NodeRead, (NodeNgramReadNull, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull))))
queryAuthorsDoc = leftJoin5 queryNodeTable queryNodeNgramTable queryNgramsTable queryNodeNgramTable queryNodeTable cond12 cond23 cond34 cond45
    where
         cond12 :: (NodeNgramRead, NodeRead) -> Column PGBool
         cond12 (nodeNgram, doc) =  _node_id                  doc
                                .== nng_node_id nodeNgram

         cond23 :: (NgramsRead, (NodeNgramRead, NodeReadNull)) -> Column PGBool
         cond23 (ngrams, (nodeNgram, _)) =  ngrams_id                  ngrams
                                        .== nng_ngrams_id nodeNgram
         
         cond34 :: (NodeNgramRead, (NgramsRead, (NodeNgramReadNull, NodeReadNull))) -> Column PGBool
         cond34 (nodeNgram2, (ngrams, (_,_)))= ngrams_id ngrams     .== nng_ngrams_id       nodeNgram2
         
         cond45 :: (NodeRead, (NodeNgramRead, (NgramsReadNull, (NodeNgramReadNull, NodeReadNull)))) -> Column PGBool
         cond45 (contact, (nodeNgram2, (_, (_,_)))) = _node_id  contact    .== nng_node_id         nodeNgram2


------------------------------------------------------------------------

runViewDocuments :: CorpusId -> Trash -> Maybe Offset -> Maybe Limit -> Maybe OrderBy -> Cmd err [FacetDoc]
runViewDocuments cId t o l order =
    runOpaQuery $ filterWith o l order $ viewDocuments cId t ntId
  where
    ntId = nodeTypeId NodeDocument

viewDocuments :: CorpusId -> Trash -> NodeTypeId -> Query FacetDocRead
viewDocuments cId t ntId = proc () -> do
  n  <- queryNodeTable     -< ()
  nn <- queryNodeNodeTable -< ()
  restrict -< _node_id          n  .== nn_node2_id nn
  restrict -< nn_node1_id nn .== (pgNodeId cId)
  restrict -< _node_typename    n  .== (pgInt4 ntId)
  restrict -< nn_delete   nn .== (pgBool t)
  returnA  -< FacetDoc (_node_id n) (_node_date n) (_node_name n) (_node_hyperdata n) (nn_favorite nn) (pgInt4 1)


------------------------------------------------------------------------
filterWith :: (PGOrd date, PGOrd title, PGOrd score) =>
     Maybe Gargantext.Core.Types.Offset
     -> Maybe Gargantext.Core.Types.Limit
     -> Maybe OrderBy
     -> Select (Facet id (Column date) (Column title) hyperdata (Column score) ngramCount)
     -> Select (Facet id (Column date) (Column title) hyperdata (Column score) ngramCount)
filterWith o l order q = limit' l $ offset' o $ orderBy (orderWith order) q


orderWith :: (PGOrd b1, PGOrd b2, PGOrd b3) => Maybe OrderBy -> Order (Facet id (Column b1) (Column b2) hyperdata (Column b3) score)
orderWith order = case order of
  (Just DateAsc)   -> asc  facetDoc_created
  
  (Just TitleAsc)  -> asc  facetDoc_title
  (Just TitleDesc) -> desc facetDoc_title
  
  (Just ScoreAsc)  -> asc  facetDoc_favorite
  (Just ScoreDesc) -> desc facetDoc_favorite
  _                -> desc facetDoc_created

