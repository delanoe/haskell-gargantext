{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Data.Gargantext.Database.Node where



import           Database.PostgreSQL.Simple.FromField (Conversion, ResultError(ConversionFailed), FromField, fromField, returnError)
import           Database.PostgreSQL.Simple.Internal  (Field)
import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Gargantext.Database.Instances
import Data.Gargantext.Database.Private (infoGargandb)
import Data.Gargantext.Prelude
import Data.Gargantext.Types
import Data.Gargantext.Utils.Prefix
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable.Internal (Typeable)
import GHC.Generics (Generic)
import qualified Data.ByteString.Internal as DBI
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as O
import Opaleye (Column, PGBool, PGInt4, PGText, PGTimestamptz, PGFloat8
               , Table(Table), PGJsonb, Query
               , QueryRunnerColumnDefault, queryRunnerColumnDefault 
               , fieldQueryRunnerColumn 
               , (.==), (.>)
               )


-- | Types for Node Database Management

type NodeWrite = NodePoly  (Maybe (Column PGInt4))        (Column PGInt4)
                                  (Column PGInt4)         (Column PGInt4)
                                  (Column PGText)  (Maybe (Column PGTimestamptz))
                                  (Column PGJsonb)

type NodeRead = NodePoly  (Column PGInt4) (Column PGInt4)
                          (Column PGInt4) (Column PGInt4)
                          (Column PGText) (Column PGTimestamptz)
                          (Column PGJsonb)

instance FromField HyperdataCorpus where
    fromField = fromField'

instance FromField HyperdataDocument where
    fromField = fromField'

--instance FromField HyperdataProject where
--    fromField = fromField'

--instance FromField HyperdataUser where
--    fromField = fromField'


fromField' :: (Typeable b, FromJSON b) => Field -> Maybe DBI.ByteString -> Conversion b
fromField' field mb = do
    v <- fromField field mb
    valueToHyperdata v
      where
          valueToHyperdata v = case fromJSON v of
             Success a -> pure a
             Error err -> returnError ConversionFailed field "cannot parse hyperdata"


instance O.QueryRunnerColumnDefault PGJsonb HyperdataDocument where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance O.QueryRunnerColumnDefault PGJsonb HyperdataCorpus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn




$(makeAdaptorAndInstance "pNode" ''NodePoly)
$(makeLensesWith abbreviatedFields   ''NodePoly)


nodeTable :: O.Table NodeWrite NodeRead 
nodeTable = O.Table "nodes" (pNode Node { node_id         = O.optional "id"
                                        , node_typename   = O.required "typename"
                                        , node_userId     = O.required "user_id"
                                        , node_parentId   = O.required "parent_id"
                                        , node_name       = O.required "name"
                                        , node_date       = O.optional "date"
                                        , node_hyperdata  = O.required "hyperdata"
                                        }
                            )




selectNodes :: Column PGInt4 -> Query (Column O.PGText)
selectNodes node_id = proc () -> do
    row@(Node n_id tn u p n d h) <- queryNodeTable -< ()
    O.restrict -< n_id .== node_id
    returnA -< n


runGetNodes :: PGS.Connection -> Query NodeRead -> IO [Document]
runGetNodes = O.runQuery


queryNodeTable :: Query NodeRead
queryNodeTable = O.queryTable nodeTable


selectNode :: Column PGInt4 -> Query NodeRead
selectNode node_id = proc () -> do
    row@(Node id tn u p_id n d h) <- queryNodeTable -< ()
    O.restrict -< p_id .== node_id
    returnA -< row


getNodes :: Column PGInt4 -> IO [Document]
getNodes node_id = do
    conn <- PGS.connect infoGargandb
    O.runQuery conn $ selectNode node_id

getCorpusDocument :: Column PGInt4 -> IO [Document]
getCorpusDocument node_id = PGS.connect infoGargandb >>= 
                          \conn -> O.runQuery conn (selectNode node_id)

getProjectCorpora :: Column PGInt4 -> IO [Corpus]
getProjectCorpora node_id = do
    conn <- PGS.connect infoGargandb
    O.runQuery conn $ selectNode node_id



