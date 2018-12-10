{-|
Module      : Gargantext.Database.Schema.Ngrams
Description : Ngram connection to the Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams connection to the Database.

-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Schema.Ngrams where


import Control.Lens (makeLenses, view, _Just, traverse)
import Data.ByteString.Internal (ByteString)
import Data.Map (Map, fromList, lookup, fromListWith)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Set (Set)
import Data.Text (Text, splitOn)
import Database.PostgreSQL.Simple as DPS (Connection)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow   (toRow)
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Gargantext.Core.Types (CorpusId)
import Gargantext.Core.Types -- (fromListTypeId, ListType, NodePoly(Node))
import Gargantext.Database.Config (nodeTypeId,userMaster)
import Gargantext.Database.Root (getRoot)
import Gargantext.Database.Types.Node (NodeType)
import Gargantext.Database.Schema.Node (getListsWithParentId, getCorporaWithParentId)
import Gargantext.Database.Utils (mkCmd, Cmd(..), Cmd')
import Gargantext.Prelude
import Opaleye
import Prelude (Enum, Bounded, minBound, maxBound)
import qualified Data.Set as DS
import qualified Database.PostgreSQL.Simple as DPS

--{-
data NgramsPoly id terms n = NgramsDb { ngrams_id    :: id
                                    , ngrams_terms :: terms
                                    , ngrams_n     :: n
                                    } deriving (Show)

--}
type NgramsWrite = NgramsPoly (Maybe (Column PGInt4))
                                   (Column PGText)
                                   (Column PGInt4)

type NgramsRead  = NgramsPoly (Column PGInt4)
                              (Column PGText)
                              (Column PGInt4)

type NgramsReadNull = NgramsPoly (Column (Nullable PGInt4))
                                 (Column (Nullable PGText))
                                 (Column (Nullable PGInt4))

--{-
type NgramsDb = NgramsPoly Int Text Int

$(makeAdaptorAndInstance "pNgramsDb"    ''NgramsPoly)
-- $(makeLensesWith abbreviatedFields   ''NgramsPoly)

ngramsTable :: Table NgramsWrite NgramsRead
ngramsTable = Table "ngrams" (pNgramsDb NgramsDb { ngrams_id    = optional "id"
                                            , ngrams_terms = required "terms"
                                            , ngrams_n     = required "n"
                                            }
                                )
--{-
queryNgramsTable :: Query NgramsRead
queryNgramsTable = queryTable ngramsTable

dbGetNgramsDb :: DPS.Connection -> IO [NgramsDb]
dbGetNgramsDb conn = runQuery conn queryNgramsTable
--}

-- | Main Ngrams Types
-- | Typed Ngrams
-- Typed Ngrams localize the context of the ngrams
-- ngrams in source field of document has Sources Type
-- ngrams in authors field of document has Authors Type
-- ngrams in text (title or abstract) of documents has Terms Type
data NgramsType = Authors | Institutes | Sources | NgramsTerms
  deriving (Eq, Show, Ord, Enum, Bounded)

ngramsTypeId :: NgramsType -> Int
ngramsTypeId Authors     = 1
ngramsTypeId Institutes  = 2
ngramsTypeId Sources     = 3
ngramsTypeId NgramsTerms = 4

fromNgramsTypeId :: Int -> Maybe NgramsType
fromNgramsTypeId id = lookup id $ fromList [(ngramsTypeId nt,nt) | nt <- [minBound .. maxBound] :: [NgramsType]]

type NgramsTerms = Text
type NgramsId    = Int
type Size        = Int

------------------------------------------------------------------------
-- | TODO put it in Gargantext.Text.Ngrams
data Ngrams = Ngrams { _ngramsTerms :: Text
                     , _ngramsSize  :: Int
           } deriving (Generic, Show, Eq, Ord)

makeLenses ''Ngrams
instance DPS.ToRow Ngrams where
  toRow (Ngrams t s) = [toField t, toField s]

text2ngrams :: Text -> Ngrams
text2ngrams txt = Ngrams txt $ length $ splitOn " " txt

-------------------------------------------------------------------------
-- | TODO put it in Gargantext.Text.Ngrams
-- Named entity are typed ngrams of Terms Ngrams
data NgramsT a =
  NgramsT { _ngramsType :: NgramsType
          , _ngramsT    :: a
          } deriving (Generic, Show, Eq, Ord)

makeLenses ''NgramsT
-----------------------------------------------------------------------
data NgramsIndexed =
  NgramsIndexed
  { _ngrams   :: Ngrams
  , _ngramsId :: NgramsId
  } deriving (Show, Generic, Eq, Ord)

makeLenses ''NgramsIndexed
------------------------------------------------------------------------
data NgramIds =
  NgramIds
  { ngramId    :: Int
  , ngramTerms :: Text
  } deriving (Show, Generic, Eq, Ord)

instance DPS.FromRow NgramIds where
  fromRow = NgramIds <$> field <*> field

----------------------
indexNgramsT :: Map NgramsTerms NgramsId -> NgramsT Ngrams -> NgramsT NgramsIndexed
indexNgramsT m ngrId = indexNgramsTWith f ngrId
  where
    f n = maybe (panic "indexNgramsT: should not happen") identity (lookup n m)

indexNgramsTWith :: (NgramsTerms -> NgramsId) -> NgramsT Ngrams-> NgramsT NgramsIndexed
indexNgramsTWith f (NgramsT t n) = NgramsT t (NgramsIndexed n ((f . _ngramsTerms) n))

insertNgrams :: [Ngrams] -> Cmd (Map NgramsTerms NgramsId)
insertNgrams ns = fromList <$> map (\(NgramIds i t) -> (t, i)) <$> (insertNgrams' ns)

insertNgrams' :: [Ngrams] -> Cmd [NgramIds]
insertNgrams' ns = mkCmd $ \conn -> DPS.query conn queryInsertNgrams (DPS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]

insertNgrams_Debug :: [(NgramsTerms, Size)] -> Cmd ByteString
insertNgrams_Debug ns = mkCmd $ \conn -> DPS.formatQuery conn queryInsertNgrams (DPS.Only $ Values fields ns)
  where
    fields = map (\t -> QualifiedIdentifier Nothing t) ["text", "int4"]

----------------------
queryInsertNgrams :: DPS.Query
queryInsertNgrams = [sql|
    WITH input_rows(terms,n) AS (?)
    , ins AS (
       INSERT INTO ngrams (terms,n)
       SELECT * FROM input_rows
       ON CONFLICT (terms) DO NOTHING -- unique index created here
       RETURNING id,terms
       )

    SELECT id, terms
    FROM   ins
    UNION  ALL
    SELECT c.id, terms
    FROM   input_rows
    JOIN   ngrams c USING (terms);     -- columns of unique index
           |]


-- | Ngrams Table
-- TODO: the way we are getting main Master Corpus and List ID is not clean
-- TODO: if ids are not present -> create
-- TODO: Global Env State Monad to keep in memory the ids without retrieving it each time
getNgramsTableDb :: NodeType -> NgramsType
                 -> NgramsTableParamUser
                 -> Cmd' err ([NgramsTableData], MapToParent, MapToChildren)
getNgramsTableDb nt ngrt ntp@(NgramsTableParam listIdUser _)  = do
  
  
  maybeRoot <- head <$> getRoot userMaster
  let path = "Garg.Db.Ngrams.getTableNgrams: "
  let masterRootId = maybe (panic $ path <> "no userMaster Tree") (view node_id) maybeRoot
  -- let errMess = panic "Error"

  corpusMasterId <- maybe (panic "error master corpus") (view node_id) <$> head <$> getCorporaWithParentId masterRootId
  
  listMasterId   <- maybe (panic "error master list") (view node_id) <$> head <$> getListsWithParentId corpusMasterId
  
  ngramsTableData <- getNgramsTableData nt ngrt ntp (NgramsTableParam listMasterId corpusMasterId)
  
  (mapToParent,mapToChildren) <- getNgramsGroup listIdUser listMasterId
  pure (ngramsTableData, mapToParent,mapToChildren)


data NgramsTableParam =
     NgramsTableParam { _nt_listId     :: Int
                      , _nt_corpusId   :: Int
                      }

type NgramsTableParamUser   = NgramsTableParam
type NgramsTableParamMaster = NgramsTableParam

data NgramsTableData = NgramsTableData { _ntd_ngrams   :: Text
                                       , _ntd_n        :: Int
                                       , _ntd_listType :: Maybe ListType
                                       , _ntd_weight   :: Double
    } deriving (Show)

getNgramsTableData :: DPS.Connection
                   -> NodeType -> NgramsType
                   -> NgramsTableParamUser -> NgramsTableParamMaster 
                   -> IO [NgramsTableData]
getNgramsTableData conn nodeT ngrmT (NgramsTableParam ul uc) (NgramsTableParam ml mc) =
  trace ("Ngrams table params" <> show params) <$>
  map (\(t,n,nt,w) -> NgramsTableData t n (fromListTypeId nt) w) <$>
    DPS.query conn querySelectTableNgrams params
      where
        nodeTId = nodeTypeId   nodeT
        ngrmTId = ngramsTypeId ngrmT
        params  = (ul,uc,nodeTId,ngrmTId,ml,mc,nodeTId,ngrmTId,uc)



querySelectTableNgrams :: DPS.Query
querySelectTableNgrams = [sql|

    WITH tableUser AS (
      SELECT ngs.terms, ngs.n, list.ngrams_type, corp.weight FROM ngrams ngs
        JOIN nodes_ngrams list ON list.ngram_id = ngs.id
        JOIN nodes_ngrams corp ON corp.ngram_id = ngs.id
        JOIN nodes        n    ON n.id          = corp.node_id
      
      WHERE list.node_id     = ?   -- User listId
        AND n.parent_id      = ?   -- User CorpusId or AnnuaireId
        AND n.typename       = ?   -- both type of childs (Documents or Contacts)
        AND corp.ngrams_type = ?   -- both type of ngrams (Authors or Terms or...)
    )
    , tableMaster AS (
      SELECT ngs.terms, ngs.n, list.ngrams_type, corp.weight FROM ngrams ngs
        JOIN nodes_ngrams list ON list.ngram_id = ngs.id
        JOIN nodes_ngrams corp ON corp.ngram_id = ngs.id
        JOIN nodes        n    ON n.id          = corp.node_id
        JOIN nodes_nodes  nn   ON nn.node2_id  = n.id
        
      WHERE list.node_id     = ?   -- Master listId
        AND n.parent_id      = ?   -- Master CorpusId or AnnuaireId
        AND n.typename       = ?   -- Master childs (Documents or Contacts)
        AND corp.ngrams_type = ?   -- both type of ngrams (Authors or Terms?)
        AND nn.node1_id      = ?   -- User CorpusId or AnnuaireId
    )
    
  SELECT COALESCE(tu.terms,tm.terms) AS terms
       , COALESCE(tu.n,tm.n)         AS n
       , COALESCE(tu.ngrams_type,tm.ngrams_type) AS ngrams_type
       , SUM(COALESCE(tu.weight,tm.weight)) AS weight
  FROM tableUser tu RIGHT JOIN tableMaster tm ON tu.terms = tm.terms
  GROUP BY tu.terms,tm.terms,tu.n,tm.n,tu.ngrams_type,tm.ngrams_type;

  |]

type ListIdUser   = Int
type ListIdMaster = Int

type MapToChildren = Map Text (Set Text)
type MapToParent   = Map Text Text

getNgramsGroup :: DPS.Connection -> ListIdUser -> ListIdMaster -> IO (MapToParent, MapToChildren)
getNgramsGroup conn lu lm = do
  groups <- getNgramsGroup' conn lu lm
  let mapChildren = fromListWith (<>) $ map (\(a,b) -> (a, DS.singleton b)) groups
  let mapParent   = fromListWith (<>) $ map (\(a,b) -> (b, a)) groups
  pure (mapParent, mapChildren)

getNgramsGroup' :: DPS.Connection -> ListIdUser -> ListIdMaster -> IO [(Text,Text)]
getNgramsGroup' conn lu lm = DPS.query conn querySelectNgramsGroup (lu,lm)

getNgramsGroup'' :: ListIdUser -> ListIdMaster -> Cmd [(Text, Text)]
getNgramsGroup'' lu lm = mkCmd $ \conn -> DPS.query conn querySelectNgramsGroup (lu,lm)

querySelectNgramsGroup :: DPS.Query
querySelectNgramsGroup = [sql|
    WITH groupUser AS (
      SELECT n1.terms AS t1, n2.terms AS t2 FROM nodes_ngrams_ngrams nnn
        JOIN ngrams n1 ON n1.id = nnn.ngram1_id
        JOIN ngrams n2 ON n2.id = nnn.ngram2_id
        WHERE
        nnn.node_id = ? -- User listId
      ),
      groupMaster AS (
      SELECT n1.terms AS t1, n2.terms AS t2 FROM nodes_ngrams_ngrams nnn
        JOIN ngrams n1 ON n1.id = nnn.ngram1_id
        JOIN ngrams n2 ON n2.id = nnn.ngram2_id
        WHERE
        nnn.node_id = ? -- Master listId
      )
    SELECT COALESCE(gu.t1,gm.t1) AS ngram1_id
         , COALESCE(gu.t2,gm.t2) AS ngram2_id
      FROM groupUser gu RIGHT JOIN groupMaster gm ON gu.t1 = gm.t1
  |]

