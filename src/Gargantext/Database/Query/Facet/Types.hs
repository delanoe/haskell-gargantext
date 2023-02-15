{-# LANGUAGE TemplateHaskell           #-}

module Gargantext.Database.Query.Facet.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Swagger
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Segment (jour)
import Gargantext.Core.Types
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger, wellNamedSchema)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument, arbitraryHyperdataDocuments)
import Opaleye
import Protolude hiding (null, map, sum, not)
import Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary

-- | DocFacet

-- data Facet = FacetDoc | FacetSources | FacetAuthors | FacetTerms
--    deriving (Show, Generic)
--instance FromJSON Facet
--instance ToJSON   Facet

type Category = Int
type Score    = Double
type Title    = Text

-- TODO remove Title
type FacetDoc = Facet NodeId UTCTime Title HyperdataDocument (Maybe Category) (Maybe Double) (Maybe Score)
type FacetDocAgg' = Facet NodeId UTCTime Title HyperdataDocument (Maybe Category) Int64 (Maybe Score)
-- type FacetSources = FacetDoc
-- type FacetAuthors = FacetDoc
-- type FacetTerms   = FacetDoc


data Facet id created title hyperdata category ngramCount score =
     FacetDoc { facetDoc_id         :: id
              , facetDoc_created    :: created
              , facetDoc_title      :: title
              , facetDoc_hyperdata  :: hyperdata
              , facetDoc_category   :: category
              , facetDoc_ngramCount :: ngramCount
              , facetDoc_score      :: score
              } deriving (Show, Generic)
{- | TODO after demo
data Facet id date hyperdata score =
     FacetDoc { facetDoc_id        :: id
              , facetDoc_date      :: date
              , facetDoc_hyperdata :: hyperdata
              , facetDoc_score     :: score
              } deriving (Show, Generic)
-}



data Pair i l = Pair {
    _p_id    :: i
  , _p_label :: l
  } deriving (Show, Generic)
$(deriveJSON (unPrefix "_p_") ''Pair)
$(makeAdaptorAndInstance "pPair" ''Pair)

instance (Typeable i, Typeable l, ToSchema i, ToSchema l) => ToSchema (Pair i l) where
  declareNamedSchema = wellNamedSchema "_p_"
instance (Arbitrary i, Arbitrary l) => Arbitrary (Pair i l) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

data FacetPaired id date hyperdata score =
  FacetPaired { _fp_id        :: id
              , _fp_date      :: date
              , _fp_hyperdata :: hyperdata
              , _fp_score     :: score }
  deriving (Show, Generic)
$(deriveJSON (unPrefix "_fp_") ''FacetPaired)
$(makeAdaptorAndInstance "pFacetPaired" ''FacetPaired)



instance ( ToSchema id
         , ToSchema date
         , ToSchema hyperdata
         , ToSchema score
         , Typeable id
         , Typeable date
         , Typeable hyperdata
         , Typeable score
         ) => ToSchema (FacetPaired id date hyperdata score) where
  declareNamedSchema = wellNamedSchema "_fp_"

instance ( Arbitrary id
         , Arbitrary date
         , Arbitrary hyperdata
         , Arbitrary score
         ) => Arbitrary (FacetPaired id date hyperdata score) where
  arbitrary = FacetPaired <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FacetPairedRead = FacetPaired (Field SqlInt4       )
                                   (Field SqlTimestamptz)
                                   (Field SqlJsonb      )
                                   (Field SqlInt4       )




-- | JSON instance
$(deriveJSON (unPrefix "facetDoc_") ''Facet)

-- | Documentation instance
instance ToSchema FacetDoc where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "facetDoc_")

-- | Mock and Quickcheck instances
instance Arbitrary FacetDoc where
    arbitrary = elements [ FacetDoc id' (jour year 01 01) t hp (Just cat) (Just ngramCount) (Just score)
                         | id'        <- [1..10]
                         , year       <- [1990..2000]
                         , t          <- ["title", "another title"]
                         , hp         <- arbitraryHyperdataDocuments
                         , cat        <- [0..2]
                         , ngramCount <- [3..100]
                         , score      <- [3..100]
                         ]

-- Facets / Views for the Front End
-- | Database instances
$(makeAdaptorAndInstance "pFacetDoc" ''Facet)
-- $(makeLensesWith abbreviatedFields   ''Facet)

type FacetDocRead = Facet (Field SqlInt4       )
                          (Field SqlTimestamptz)
                          (Field SqlText       )
                          (Field SqlJsonb      )
                          (FieldNullable SqlInt4) -- Category
                          (FieldNullable SqlFloat8) -- Ngrams Count
                          (FieldNullable SqlFloat8) -- Score

type FacetDocAgg = Facet (Field SqlInt4       )
                         (Field SqlTimestamptz)
                         (Field SqlText       )
                         (Field SqlJsonb      )
                         (Field SqlInt4) -- Category
                         (Field SqlInt8) -- Ngrams Count
                         (Field SqlFloat8) -- Score

type FacetDocAggPart = Facet (Field SqlInt4       )
                             (Field SqlTimestamptz)
                             (Field SqlText       )
                             (Field SqlJsonb      )
                             (Field SqlInt4) -- Category
                             (Field SqlInt4) -- Ngrams Count
                             (Field SqlFloat8) -- Score

-----------------------------------------------------------------------
-----------------------------------------------------------------------
data OrderBy =  DateAsc   | DateDesc
             | TitleAsc   | TitleDesc
             | ScoreDesc  | ScoreAsc
             | SourceAsc  | SourceDesc
             | TagAsc     | TagDesc
             deriving (Generic, Enum, Bounded, Read, Show)

instance FromHttpApiData OrderBy
  where
    parseUrlPiece "DateAsc"    = pure DateAsc
    parseUrlPiece "DateDesc"   = pure DateDesc
    parseUrlPiece "TitleAsc"   = pure TitleAsc
    parseUrlPiece "TitleDesc"  = pure TitleDesc
    parseUrlPiece "ScoreAsc"   = pure ScoreAsc
    parseUrlPiece "ScoreDesc"  = pure ScoreDesc
    parseUrlPiece "SourceAsc"  = pure SourceAsc
    parseUrlPiece "SourceDesc" = pure SourceDesc
    parseUrlPiece "TagAsc"     = pure TagAsc
    parseUrlPiece "TagDesc"    = pure TagDesc
    parseUrlPiece _            = Left "Unexpected value of OrderBy"
instance ToHttpApiData OrderBy where
  toUrlPiece = T.pack . show

instance ToParamSchema OrderBy
instance FromJSON  OrderBy
instance ToJSON    OrderBy
instance ToSchema  OrderBy
instance Arbitrary OrderBy
  where
    arbitrary = elements [minBound..maxBound]
