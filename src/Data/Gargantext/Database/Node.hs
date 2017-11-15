{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Gargantext.Database.Node where

import           Database.PostgreSQL.Simple.FromField (Conversion, ResultError(ConversionFailed), FromField, fromField, returnError)
import           Database.PostgreSQL.Simple.Internal  (Field)
import Control.Arrow (returnA)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Data.Aeson
import Data.Gargantext.Database.Private (infoGargandb)
import Data.Gargantext.Types
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Internal as DBI
import qualified Database.PostgreSQL.Simple as PGS
import Opaleye


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
             Error _err -> returnError ConversionFailed field "cannot parse hyperdata"


instance QueryRunnerColumnDefault PGJsonb HyperdataDocument where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGJsonb HyperdataCorpus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn




$(makeAdaptorAndInstance "pNode" ''NodePoly)
$(makeLensesWith abbreviatedFields   ''NodePoly)


nodeTable :: Table NodeWrite NodeRead
nodeTable = Table "nodes" (pNode Node { node_id           = optional "id"
                                        , node_typename   = required "typename"
                                        , node_userId     = required "user_id"
                                        , node_parentId   = required "parent_id"
                                        , node_name       = required "name"
                                        , node_date       = optional "date"
                                        , node_hyperdata  = required "hyperdata"
                                        }
                            )


selectNodes :: Column PGInt4 -> Query (Column PGText)
selectNodes node_id = proc () -> do
    (Node n_id _tn _u _p n _d _h) <- queryNodeTable -< ()
    restrict -< n_id .== node_id
    returnA -< n

instance QueryRunnerColumnDefault PGInt4 Integer where
    queryRunnerColumnDefault = fieldQueryRunnerColumn


runGetNodes :: PGS.Connection -> Query NodeRead -> IO [Document]
runGetNodes = runQuery


queryNodeTable :: Query NodeRead
queryNodeTable = queryTable nodeTable


selectNode :: Column PGInt4 -> Query NodeRead
selectNode node_id = proc () -> do
    row@(Node _id _tn _u p_id _n _d _h) <- queryNodeTable -< ()
    restrict -< p_id .== node_id
    returnA -< row


getNodes :: Column PGInt4 -> IO [Document]
getNodes node_id = do
    conn <- PGS.connect infoGargandb
    runQuery conn $ selectNode node_id

getCorpusDocument :: Column PGInt4 -> IO [Document]
getCorpusDocument node_id = PGS.connect infoGargandb >>=
                          \conn -> runQuery conn (selectNode node_id)

getProjectCorpora :: Column PGInt4 -> IO [Corpus]
getProjectCorpora node_id = do
    conn <- PGS.connect infoGargandb
    runQuery conn $ selectNode node_id
