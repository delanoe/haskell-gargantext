{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-- TODO-ACCESS:
--   check userId       CanFillUserCorpus   userCorpusId
--   check masterUserId CanFillMasterCorpus masterCorpusId

-- TODO-ACCESS: check uId CanInsertDoc pId && checkDocType nodeType
-- TODO-EVENTS: InsertedNodes
-}

{-# OPTIONS_GHC -fno-warn-orphans    #-}

{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE TemplateHaskell         #-}

module Gargantext.Database.Action.Flow -- (flowDatabase, ngrams2list)
  ( FlowCmdM
  , getDataText
  , flowDataText

  , flowCorpusFile
  , flowCorpus
  , flowAnnuaire

  , getOrMkRoot
  , getOrMk_RootWithCorpus
  , TermType(..)
  , DataOrigin(..)
  , allDataOrigins

  , do_api
  )
    where

import Control.Lens ((^.), view, _Just, makeLenses)
import Data.Aeson.TH (deriveJSON)
import Data.Either
import Data.List (concat)
import qualified Data.Map  as Map
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Monoid
import Data.Swagger
import Data.Text (splitOn, intercalate)
import Data.Traversable (traverse)
import Data.Tuple.Extra (first, second)
import GHC.Generics (Generic)
import System.FilePath (FilePath)

import Gargantext.Core (Lang(..))
import Gargantext.Core.Flow.Types
import Gargantext.Core.Types (Terms(..))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Main
import Gargantext.Database.Action.Flow.List
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Flow.Utils (insertDocNgrams)
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Contact -- (HyperdataContact(..), ContactWho(..))
import Gargantext.Database.Query.Table.Node.Document.Insert -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Query.Tree.Root (getOrMkRoot, getOrMk_RootWithCorpus)
import Gargantext.Database.Action.Search (searchInDatabase)
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Ngrams
import Gargantext.Database.Query.Table.NodeNgrams (listInsertDb , getCgramsId)
import Gargantext.Database.Query.Table.NodeNodeNgrams2
import Gargantext.Ext.IMT (toSchoolName)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Ext.IMTUser (deserialiseImtUsersFromFile)
import Gargantext.Text
import Gargantext.Prelude
import Gargantext.Text.Corpus.Parsers (parseFile, FileFormat)
import Gargantext.Text.List (buildNgramsLists,StopSize(..))
import Gargantext.Text.Terms.Mono.Stem.En (stemIt)
import Gargantext.Text.Terms
import qualified Gargantext.Database.Query.Table.Node.Document.Add  as Doc  (add)
import qualified Gargantext.Text.Corpus.API as API

------------------------------------------------------------------------
-- TODO use internal with API name (could be old data)
data DataOrigin = InternalOrigin { _do_api :: API.ExternalAPIs }
                | ExternalOrigin { _do_api :: API.ExternalAPIs }
               -- TODO Web
  deriving (Generic, Eq)

makeLenses ''DataOrigin
deriveJSON (unPrefix "_do_") ''DataOrigin
instance ToSchema DataOrigin where
  declareNamedSchema = genericDeclareNamedSchema (unPrefixSwagger "_do_")

allDataOrigins :: [DataOrigin]
allDataOrigins = map InternalOrigin API.externalAPIs
              <> map ExternalOrigin API.externalAPIs

---------------

data DataText = DataOld ![NodeId]
              | DataNew ![[HyperdataDocument]]


-- TODO use the split parameter in config file
getDataText :: FlowCmdM env err m
            => DataOrigin
            -> TermType Lang
            -> API.Query
            -> Maybe API.Limit
            -> m DataText
getDataText (ExternalOrigin api) la q li = liftBase $ DataNew
                                  <$> splitEvery 500
                                  <$> API.get api (_tt_lang la) q li
getDataText (InternalOrigin _) _la q _li = do
  (_masterUserId, _masterRootId, cId) <- getOrMk_RootWithCorpus
                                           (UserName userMaster)
                                           (Left "")
                                           (Nothing :: Maybe HyperData)
  ids <-  map fst <$> searchInDatabase cId (stemIt q)
  pure $ DataOld ids

-------------------------------------------------------------------------------
flowDataText :: FlowCmdM env err m
             => User
             -> DataText
             -> TermType Lang
             -> CorpusId
             -> m CorpusId
flowDataText u (DataOld ids) tt cid = flowCorpusUser (_tt_lang tt) u (Right [cid]) corpusType ids
  where
    corpusType = (Nothing :: Maybe HyperData)
flowDataText u (DataNew txt) tt cid = flowCorpus u (Right [cid]) tt txt

------------------------------------------------------------------------
-- TODO use proxy
flowAnnuaire :: FlowCmdM env err m
             => User
             -> Either CorpusName [CorpusId]
             -> (TermType Lang)
             -> FilePath
             -> m AnnuaireId
flowAnnuaire u n l filePath = do
  docs <- liftBase $ (( splitEvery 500 <$> deserialiseImtUsersFromFile filePath) :: IO [[HyperdataContact]])
  flow (Nothing :: Maybe HyperdataAnnuaire) u n l docs

------------------------------------------------------------------------
flowCorpusFile :: FlowCmdM env err m
           => User
           -> Either CorpusName [CorpusId]
           -> Limit -- Limit the number of docs (for dev purpose)
           -> TermType Lang -> FileFormat -> FilePath
           -> m CorpusId
flowCorpusFile u n l la ff fp = do
  docs <- liftBase ( splitEvery 500
                 <$> take l
                 <$> parseFile ff fp
                 )
  flowCorpus u n la (map (map toHyperdataDocument) docs)

------------------------------------------------------------------------
-- | TODO improve the needed type to create/update a corpus
-- (For now, Either is enough)
flowCorpus :: (FlowCmdM env err m, FlowCorpus a)
           => User
           -> Either CorpusName [CorpusId]
           -> TermType Lang
           -> [[a]]
           -> m CorpusId
flowCorpus = flow (Nothing :: Maybe HyperData)


flow :: (FlowCmdM env err m, FlowCorpus a, MkCorpus c)
     => Maybe c
     -> User
     -> Either CorpusName [CorpusId]
     -> TermType Lang
     -> [[a]]
     -> m CorpusId
flow c u cn la docs = do
  ids <- traverse (insertMasterDocs c la) docs
  flowCorpusUser (la ^. tt_lang) u cn c (concat ids)

------------------------------------------------------------------------
flowCorpusUser :: (FlowCmdM env err m, MkCorpus c)
               => Lang
               -> User
               -> Either CorpusName [CorpusId]
               -> Maybe c
               -> [NodeId]
               -> m CorpusId
flowCorpusUser l user corpusName ctype ids = do
  -- User Flow
  (userId, _rootId, userCorpusId) <- getOrMk_RootWithCorpus user corpusName ctype
  listId <- getOrMkList userCorpusId userId
  _cooc  <- mkNode NodeListCooc listId userId
  -- TODO: check if present already, ignore
  _ <- Doc.add userCorpusId ids

  _tId <- mkNode NodeTexts userCorpusId userId
  -- printDebug "Node Text Id" tId

  -- User List Flow
  (masterUserId, _masterRootId, masterCorpusId) <- getOrMk_RootWithCorpus (UserName userMaster) (Left "") ctype
  ngs         <- buildNgramsLists l 2 3 (StopSize 3) userCorpusId masterCorpusId
  _userListId <- flowList_DbRepo listId ngs
  _mastListId <- getOrMkList masterCorpusId masterUserId
  -- _ <- insertOccsUpdates userCorpusId mastListId
  -- printDebug "userListId" userListId
  -- User Graph Flow
  _ <- mkDashboard userCorpusId userId
  _ <- mkGraph  userCorpusId userId
  --_ <- mkPhylo  userCorpusId userId

  -- Annuaire Flow
  -- _ <- mkAnnuaire  rootUserId userId
  pure userCorpusId


insertMasterDocs :: ( FlowCmdM env err m
                    , FlowCorpus a
                    , MkCorpus   c
                    )
                 => Maybe c
                 -> TermType Lang
                 -> [a]
                 -> m [DocId]
insertMasterDocs c lang hs  =  do
  (masterUserId, _, masterCorpusId) <- getOrMk_RootWithCorpus (UserName userMaster) (Left corpusMasterName) c

  -- TODO Type NodeDocumentUnicised
  let docs = map addUniqId hs
  ids <- insertDb masterUserId masterCorpusId docs
  let
    ids' = map reId ids
    documentsWithId = mergeData (toInserted ids) (Map.fromList $ map viewUniqId' docs)
  -- TODO
  -- create a corpus with database name (CSV or PubMed)
  -- add documents to the corpus (create node_node link)
  -- this will enable global database monitoring

  -- maps :: IO Map Ngrams (Map NgramsType (Map NodeId Int))
  maps <- mapNodeIdNgrams
       <$> documentIdWithNgrams (extractNgramsT $ withLang lang documentsWithId) documentsWithId

  terms2id <- insertNgrams $ Map.keys maps
  -- to be removed
  let indexedNgrams = Map.mapKeys (indexNgrams terms2id) maps

  -- new
  lId      <- getOrMkList masterCorpusId masterUserId
  mapCgramsId <- listInsertDb lId toNodeNgramsW'
                $ map (first _ngramsTerms . second Map.keys)
                $ Map.toList maps
  -- insertDocNgrams
  _return <- insertNodeNodeNgrams2
           $ catMaybes [ NodeNodeNgrams2 <$> Just nId
                                         <*> getCgramsId mapCgramsId ngrams_type (_ngramsTerms terms'')
                                         <*> Just (fromIntegral w :: Double)
                       | (terms'', mapNgramsTypes) <- Map.toList maps
                       , (ngrams_type, mapNodeIdWeight) <- Map.toList mapNgramsTypes
                       , (nId, w) <- Map.toList mapNodeIdWeight
                       ]

  _ <- Doc.add masterCorpusId ids'
  _cooc <- mkNode NodeListCooc lId masterUserId
  -- to be removed
  _   <- insertDocNgrams lId indexedNgrams

  pure ids'


------------------------------------------------------------------------



------------------------------------------------------------------------
viewUniqId' :: UniqId a
            => a
            -> (HashId, a)
viewUniqId' d = maybe err (\h -> (h,d)) (view uniqId d)
      where
        err = panic "[ERROR] Database.Flow.toInsert"


toInserted :: [ReturnId]
           -> Map HashId ReturnId
toInserted =
  Map.fromList . map    (\r ->  (reUniqId r, r)    )
               . filter (\r -> reInserted r == True)

mergeData :: Map HashId ReturnId
          -> Map HashId a
          -> [DocumentWithId a]
mergeData rs = catMaybes . map toDocumentWithId . Map.toList
  where
    toDocumentWithId (sha,hpd) =
      DocumentWithId <$> fmap reId (lookup sha rs)
                     <*> Just hpd

------------------------------------------------------------------------

instance HasText HyperdataContact
  where
    hasText = undefined

------------------------------------------------------------------------
------------------------------------------------------------------------

documentIdWithNgrams :: HasNodeError err
                     => (a
                     -> Cmd err (Map Ngrams (Map NgramsType Int)))
                     -> [DocumentWithId a]
                     -> Cmd err [DocumentIdWithNgrams a]
documentIdWithNgrams f = traverse toDocumentIdWithNgrams
  where
    toDocumentIdWithNgrams d = do
      e <- f $ documentData         d
      pure   $ DocumentIdWithNgrams d e


------------------------------------------------------------------------


instance ExtractNgramsT HyperdataContact
  where
    extractNgramsT l hc = filterNgramsT 255 <$> extract l hc
      where
        extract :: TermType Lang -> HyperdataContact
                -> Cmd err (Map Ngrams (Map NgramsType Int))
        extract _l hc' = do
          let authors = map text2ngrams
                     $ maybe ["Nothing"] (\a -> [a])
                     $ view (hc_who . _Just . cw_lastName) hc'

          pure $ Map.fromList $ [(a', Map.singleton Authors     1) | a' <- authors    ]

instance HasText HyperdataDocument
  where
    hasText h = catMaybes [ _hyperdataDocument_title    h
                          , _hyperdataDocument_abstract h
                          ]

instance ExtractNgramsT HyperdataDocument
  where
    extractNgramsT :: TermType Lang
                   -> HyperdataDocument
                   -> Cmd err (Map Ngrams (Map NgramsType Int))
    extractNgramsT lang hd = filterNgramsT 255 <$> extractNgramsT' lang hd
      where
        extractNgramsT' :: TermType Lang
                        -> HyperdataDocument
                       -> Cmd err (Map Ngrams (Map NgramsType Int))
        extractNgramsT' lang' doc = do
          let source    = text2ngrams
                        $ maybe "Nothing" identity
                        $ _hyperdataDocument_source doc

              institutes = map text2ngrams
                         $ maybe ["Nothing"] (map toSchoolName . (splitOn ", "))
                         $ _hyperdataDocument_institutes doc

              authors    = map text2ngrams
                         $ maybe ["Nothing"] (splitOn ", ")
                         $ _hyperdataDocument_authors doc

          terms' <- map text2ngrams
                 <$> map (intercalate " " . _terms_label)
                 <$> concat
                 <$> liftBase (extractTerms lang' $ hasText doc)

          pure $ Map.fromList $  [(source, Map.singleton Sources 1)]
                             <> [(i', Map.singleton Institutes  1) | i' <- institutes ]
                             <> [(a', Map.singleton Authors     1) | a' <- authors    ]
                             <> [(t', Map.singleton NgramsTerms 1) | t' <- terms'     ]


