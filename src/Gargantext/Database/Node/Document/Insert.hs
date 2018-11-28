{-|
Module      : Gargantext.Database.Node.Document.Insert
Description : Importing context of texts (documents)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

* Purpose of this module

Enabling "common goods" of text data and respecting privacy.

Gargantext shares as "common good" the links between context of texts
and terms / words / ngrams.

Basically a context of text can be defined as a document (see 'Gargantext.Text').

Issue to tackle in that module: each global document of Gargantext has
to be unique, then shared, but how to respect privacy if needed ?


* Methodology to get uniqueness and privacy by design

As a consequence, when importing/inserting a new document in Gargantext,
a policy for the uniqueness of the inserted docuemnts has to be defined.

That is the purpose of this module which defines its main concepts.

Unique identifier in database is of a 3-tuple of 3 policies that
together define uniqueness:

- Design policy: type of node is needed as TypenameId, that is a
Document or Individual or something else;

- Privacy policy: with ParentId, parent becomes unique, then it enables
users to get their own copy without sharing it with all the users of the
database (in others words parent_id is necessary to preserve privacy for
instance).

- Hash policy: this UniqId is a sha256 uniq id which is the result of
the concatenation of the parameters defined by @hashParameters@.

> -- * Example
> insertTest :: FromRow r => CorpusId -> [Node HyperdataDocument] -> IO [r]
> insertTest :: IO [ReturnId]
> insertTest = connectGargandb "gargantext.ini"
>          >>= \conn -> insertDocuments conn 1 452162 hyperdataDocuments

-}
------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}
------------------------------------------------------------------------
module Gargantext.Database.Node.Document.Insert where

import Control.Lens (set)
import Data.Aeson (toJSON, Value)
import Data.Maybe (maybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (FromRow, Query, query, Only(..))
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import GHC.Generics (Generic)
import Gargantext.Database.Config (nodeTypeId)
import Gargantext.Database.Node (mkCmd, Cmd(..))
import Gargantext.Database.Node.Contact (HyperdataContact(..))
import Gargantext.Database.Types.Node
import Gargantext.Prelude
import qualified Data.ByteString.Lazy.Char8  as DC (pack)
import qualified Data.Digest.Pure.SHA        as SHA (sha256, showDigest)
import qualified Data.Text                   as DT (pack, unpack, concat, take)

-- TODO : the import of Document constructor below does not work
-- import Gargantext.Database.Types.Node (Document)
--import Gargantext.Database.Types.Node (docExample, hyperdataDocument, HyperdataDocument(..)
--                                  , hyperdataDocument_uniqId
--                                  , hyperdataDocument_title
--                                  , hyperdataDocument_abstract
--                                  , hyperdataDocument_source
--                                  , Node(..), node_typename
--                                            , node_userId
--                                            , node_parentId, node_name, node_hyperdata, hyperdataDocuments
--                                  , NodeTypeId
--                                  )
{-| To Print result query
import Data.ByteString.Internal (ByteString)
import Database.PostgreSQL.Simple (formatQuery)
-}

---------------------------------------------------------------------------
-- * Main Insert functions

-- ** Database configuration
-- Administrator of the database has to create a uniq index as following SQL command:
-- `create unique index on nodes (typename, parent_id, (hyperdata ->> 'uniqId'));`

-- | Insert Document main function
-- UserId : user who is inserting the documents
-- ParentId : folder ID which is parent of the inserted documents


data ToDbData = ToDbDocument HyperdataDocument | ToDbContact HyperdataContact

insertDocuments :: UserId -> ParentId -> [ToDbData] -> Cmd [ReturnId]
insertDocuments uId pId hs = mkCmd $ \c -> query c queryInsert (Only $ Values fields $ prepare uId pId hs)
  where
    fields    = map (\t-> QualifiedIdentifier Nothing t) inputSqlTypes

-- | Debug SQL function
--
-- to print rendered query (Debug purpose) use @formatQuery@ function.
{-
insertDocuments_Debug :: (Hyperdata a, ToJSON a, ToRow a) => UserId -> ParentId -> [a] -> Cmd ByteString
insertDocuments_Debug uId pId hs = mkCmd $ \conn -> formatQuery conn queryInsert (Only $ Values fields inputData)
  where
    fields    = map (\t-> QualifiedIdentifier Nothing t) inputSqlTypes
    inputData = prepare uId pId hs
-}


-- | Input Tables: types of the tables
inputSqlTypes :: [Text]
inputSqlTypes = map DT.pack ["int4","int4","int4","text","jsonb"]

-- | SQL query to insert documents inside the database
queryInsert :: Query
queryInsert = [sql|
    WITH input_rows(typename,user_id,parent_id,name,hyperdata) AS (?)
    , ins AS (
       INSERT INTO nodes (typename,user_id,parent_id,name,hyperdata)
       SELECT * FROM input_rows
       ON CONFLICT ((hyperdata ->> 'uniqIdBdd')) DO NOTHING -- on unique index
       -- ON CONFLICT (typename, parent_id, (hyperdata ->> 'uniqId')) DO NOTHING -- on unique index
       RETURNING id,hyperdata
       )

    SELECT true AS source                     -- true for 'newly inserted'
         , id
         , hyperdata ->> 'uniqId'  as doi
    FROM   ins
    UNION  ALL
    SELECT false AS source                    -- false for 'not inserted'
         , c.id
         , hyperdata ->> 'uniqId' as doi
    FROM   input_rows
    JOIN   nodes c USING (hyperdata);         -- columns of unique index
           |]

prepare :: UserId -> ParentId -> [ToDbData] -> [InputData]
prepare uId pId = map (\h -> InputData tId uId pId (name h) (toJSON' h))
  where
    tId    = nodeTypeId NodeDocument
    
    toJSON' (ToDbDocument hd) = toJSON hd
    toJSON' (ToDbContact  hc) = toJSON hc
    
    name h = DT.take 255 <$> maybe "No Title" identity $ f h
      where
        f (ToDbDocument hd) = _hyperdataDocument_title hd
        f (ToDbContact  _ ) = Just "Contact" -- TODO view FirstName . LastName

------------------------------------------------------------------------
-- * Main Types used

-- ** Return Types

-- | When documents are inserted
-- ReturnType after insertion
data ReturnId = ReturnId { reInserted :: Bool -- ^ if the document is inserted (True: is new, False: is not new)
                         , reId       :: Int  -- ^ always return the id of the document (even new or not new)
                                         --   this is the uniq id in the database
                         , reUniqId   :: Text -- ^ Hash Id with concatenation of hash parameters
                         } deriving (Show, Generic)

instance FromRow ReturnId where
  fromRow = ReturnId <$> field <*> field <*> field

-- ** Insert Types

type UserId   = Int
type ParentId = Int

data InputData = InputData { inTypenameId :: NodeTypeId
                           , inUserId     :: UserId
                           , inParentId   :: ParentId
                           , inName       :: Text
                           , inHyper      :: Value
                           } deriving (Show, Generic, Typeable)

instance ToRow InputData where
  toRow inputData = [ toField (inTypenameId inputData)
                    , toField (inUserId     inputData)
                    , toField (inParentId   inputData)
                    , toField (inName       inputData)
                    , toField (inHyper      inputData)
                    ]

---------------------------------------------------------------------------
-- * Uniqueness of document definition

addUniqIds :: HyperdataDocument -> HyperdataDocument
addUniqIds doc = set hyperdataDocument_uniqIdBdd (Just hashBdd)
              $ set hyperdataDocument_uniqId    (Just hash) doc
  where
    hash    = uniqId $ DT.concat $ map ($ doc) hashParameters
    hashBdd = uniqId $ DT.concat $ map ($ doc) ([(\d -> maybe' (_hyperdataDocument_bdd d))] <> hashParameters)

    uniqId :: Text -> Text
    uniqId = DT.pack . SHA.showDigest . SHA.sha256 . DC.pack . DT.unpack


hashParameters :: [(HyperdataDocument -> Text)]
hashParameters = [ \d -> maybe' (_hyperdataDocument_title    d)
                 , \d -> maybe' (_hyperdataDocument_abstract d)
                 , \d -> maybe' (_hyperdataDocument_source   d)
                 , \d -> maybe' (_hyperdataDocument_publication_date   d)
                 ]

maybe' :: Maybe Text -> Text
maybe' = maybe (DT.pack "") identity

---------------------------------------------------------------------------

