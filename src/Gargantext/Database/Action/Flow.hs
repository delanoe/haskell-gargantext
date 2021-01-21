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

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}

module Gargantext.Database.Action.Flow -- (flowDatabase, ngrams2list)
  ( FlowCmdM
  , getDataText
  , flowDataText
  , flow

  , flowCorpusFile
  , flowCorpus
  , flowAnnuaire
  , insertMasterDocs

  , getOrMkRoot
  , getOrMk_RootWithCorpus
  , TermType(..)
  , DataOrigin(..)
  , allDataOrigins

  , do_api
  , indexAllDocumentsWithPosTag
  )
    where

import Control.Lens ((^.), view, _Just, makeLenses)
import Data.Aeson.TH (deriveJSON)
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List (concat)
import Data.Map (Map, lookup)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Swagger
import Data.Text (splitOn)
import Data.Traversable (traverse)
import Data.Tuple.Extra (first, second)
import GHC.Generics (Generic)
import System.FilePath (FilePath)
import qualified Data.HashMap.Strict as HashMap
import qualified Gargantext.Data.HashMap.Strict.Utils as HashMap
import qualified Data.Map as Map

import Gargantext.Core (Lang(..), PosTagAlgo(..))
import Gargantext.Core.Ext.IMT (toSchoolName)
import Gargantext.Core.Ext.IMTUser (deserialiseImtUsersFromFile)
import Gargantext.Core.Flow.Types
import Gargantext.Core.Text
import Gargantext.Core.Text.List.Group.WithStem ({-StopSize(..),-} GroupParams(..))
import Gargantext.Core.Text.Corpus.Parsers (parseFile, FileFormat)
import Gargantext.Core.Text.List (buildNgramsLists)
import Gargantext.Core.Text.Terms
import Gargantext.Core.Text.Terms.Mono.Stem.En (stemIt)
import Gargantext.Core.Types (POS(NP))
import Gargantext.Core.Types.Individu (User(..))
import Gargantext.Core.Types.Main
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixSwagger)
import Gargantext.Database.Action.Flow.List
import Gargantext.Database.Action.Flow.Types
import Gargantext.Database.Action.Flow.Utils (insertDocNgrams, DocumentIdWithNgrams(..))
import Gargantext.Database.Action.Search (searchDocInDatabase)
import Gargantext.Database.Admin.Config (userMaster, corpusMasterName)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Ngrams
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Document.Insert -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Table.NodeNgrams (listInsertDb , getCgramsId)
import Gargantext.Database.Query.Table.NodeNodeNgrams2
import Gargantext.Database.Query.Tree.Root (getOrMkRoot, getOrMk_RootWithCorpus)
import Gargantext.Database.Schema.Node (NodePoly(..), node_id)
import Gargantext.Database.Types
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (Hash)
import qualified Gargantext.Core.Text.Corpus.API as API
import qualified Gargantext.Database.Query.Table.Node.Document.Add  as Doc  (add)

------------------------------------------------------------------------
-- Impots for upgrade function
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Database.Query.Tree (findNodesId)
import qualified Data.List as List
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
                                           (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchDocInDatabase cId (stemIt q)
  pure $ DataOld ids

-------------------------------------------------------------------------------
flowDataText :: ( FlowCmdM env err m
                )
                => User
                -> DataText
                -> TermType Lang
                -> CorpusId
                -> m CorpusId
flowDataText u (DataOld ids) tt cid = flowCorpusUser (_tt_lang tt) u (Right [cid]) corpusType ids
  where
    corpusType = (Nothing :: Maybe HyperdataCorpus)
flowDataText u (DataNew txt) tt cid = flowCorpus u (Right [cid]) tt txt

------------------------------------------------------------------------
-- TODO use proxy
flowAnnuaire :: (FlowCmdM env err m)
             => User
             -> Either CorpusName [CorpusId]
             -> (TermType Lang)
             -> FilePath
             -> m AnnuaireId
flowAnnuaire u n l filePath = do
  docs <- liftBase $ (( splitEvery 500 <$> deserialiseImtUsersFromFile filePath) :: IO [[HyperdataContact]])
  flow (Nothing :: Maybe HyperdataAnnuaire) u n l docs

------------------------------------------------------------------------
flowCorpusFile :: (FlowCmdM env err m)
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
flowCorpus = flow (Nothing :: Maybe HyperdataCorpus)


flow :: ( FlowCmdM env err m
        , FlowCorpus a
        , MkCorpus c
        )
        => Maybe c
        -> User
        -> Either CorpusName [CorpusId]
        -> TermType Lang
        -> [[a]]
        -> m CorpusId
flow c u cn la docs = do
  -- TODO if public insertMasterDocs else insertUserDocs
  ids <- traverse (insertMasterDocs c la) docs
  flowCorpusUser (la ^. tt_lang) u cn c (concat ids)

------------------------------------------------------------------------
flowCorpusUser :: ( FlowCmdM env err m
                  , MkCorpus c
                  )
               => Lang
               -> User
               -> Either CorpusName [CorpusId]
               -> Maybe c
               -> [NodeId]
               -> m CorpusId
flowCorpusUser l user corpusName ctype ids = do
  -- User Flow
  (userId, _rootId, userCorpusId) <- getOrMk_RootWithCorpus user corpusName ctype
  -- NodeTexts is first
  _tId <- insertDefaultNode NodeTexts userCorpusId userId
  -- printDebug "NodeTexts: " tId

  -- NodeList is second
  listId <- getOrMkList userCorpusId userId
  -- _cooc  <- insertDefaultNode NodeListCooc listId userId
  -- TODO: check if present already, ignore
  _ <- Doc.add userCorpusId ids

  -- printDebug "Node Text Ids:" tId

  -- User List Flow
  (masterUserId, _masterRootId, masterCorpusId) <- getOrMk_RootWithCorpus (UserName userMaster) (Left "") ctype

  --let gp = (GroupParams l 2 3 (StopSize 3)) 
  let gp = GroupWithPosTag l CoreNLP HashMap.empty 
  ngs         <- buildNgramsLists gp user userCorpusId masterCorpusId

  _userListId <- flowList_DbRepo listId ngs
  _mastListId <- getOrMkList masterCorpusId masterUserId
  -- _ <- insertOccsUpdates userCorpusId mastListId
  -- printDebug "userListId" userListId
  -- User Graph Flow
  _ <- insertDefaultNode NodeDashboard userCorpusId userId
  _ <- insertDefaultNode NodeGraph     userCorpusId userId
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
  (ids', documentsWithId) <- insertDocs masterUserId masterCorpusId (map (toNode masterUserId masterCorpusId) hs )
  _ <- Doc.add masterCorpusId ids'
  -- TODO
  -- create a corpus with database name (CSV or PubMed)
  -- add documents to the corpus (create node_node link)
  -- this will enable global database monitoring

  -- maps :: IO Map Ngrams (Map NgramsType (Map NodeId Int))
  mapNgramsDocs' :: HashMap ExtractedNgrams (Map NgramsType (Map NodeId Int))
                <- mapNodeIdNgrams
                <$> documentIdWithNgrams
                    (extractNgramsT $ withLang lang documentsWithId)
                    documentsWithId

  terms2id <- insertExtractedNgrams $ HashMap.keys mapNgramsDocs'
  let mapNgramsDocs = HashMap.mapKeys extracted2ngrams mapNgramsDocs'

  -- to be removed
  let indexedNgrams = HashMap.mapKeys (indexNgrams terms2id) mapNgramsDocs

  -- new
  lId      <- getOrMkList masterCorpusId masterUserId
  mapCgramsId <- listInsertDb lId toNodeNgramsW'
               $ map (first _ngramsTerms . second Map.keys)
               $ HashMap.toList mapNgramsDocs
  -- insertDocNgrams
  _return <- insertNodeNodeNgrams2
           $ catMaybes [ NodeNodeNgrams2 <$> Just nId
                                         <*> getCgramsId mapCgramsId ngrams_type (_ngramsTerms terms'')
                                         <*> Just (fromIntegral w :: Double)
                       | (terms'', mapNgramsTypes)      <- HashMap.toList mapNgramsDocs
                       , (ngrams_type, mapNodeIdWeight) <- Map.toList mapNgramsTypes
                       , (nId, w)                       <- Map.toList mapNodeIdWeight
                       ]

  -- _cooc <- insertDefaultNode NodeListCooc lId masterUserId
  -- to be removed
  _   <- insertDocNgrams lId indexedNgrams
  pure ids'

------------------------------------------------------------------------
-- TODO Type NodeDocumentUnicised
insertDocs :: ( FlowCmdM env err m
              -- , FlowCorpus a
              , FlowInsertDB a
              )
              => UserId
              -> CorpusId
              -> [a]
              -> m ([DocId], [Indexed NodeId a])
insertDocs uId cId hs = do
  let docs = map addUniqId hs
  newIds <- insertDb uId cId docs
  -- printDebug "newIds" newIds
  let
    newIds' = map reId newIds
    documentsWithId = mergeData (toInserted newIds) (Map.fromList $ map viewUniqId' docs)
  _ <- Doc.add cId newIds'
  pure (newIds', documentsWithId)


------------------------------------------------------------------------
viewUniqId' :: UniqId a
            => a
            -> (Hash, a)
viewUniqId' d = maybe err (\h -> (h,d)) (view uniqId d)
      where
        err = panic "[ERROR] Database.Flow.toInsert"


toInserted :: [ReturnId]
           -> Map Hash ReturnId
toInserted =
  Map.fromList . map    (\r -> (reUniqId r, r)     )
               . filter (\r -> reInserted r == True)

mergeData :: Map Hash ReturnId
          -> Map Hash a
          -> [Indexed NodeId a]
mergeData rs = catMaybes . map toDocumentWithId . Map.toList
  where
    toDocumentWithId (sha,hpd) =
      Indexed <$> fmap reId (lookup sha rs)
              <*> Just hpd

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
documentIdWithNgrams :: HasNodeError err
                     => (a
                     -> Cmd err (HashMap b (Map NgramsType Int)))
                     -> [Indexed NodeId a]
                     -> Cmd err [DocumentIdWithNgrams a b]
documentIdWithNgrams f = traverse toDocumentIdWithNgrams
  where
    toDocumentIdWithNgrams d = do
      e <- f $ _unIndex         d
      pure   $ DocumentIdWithNgrams d e


-- | TODO check optimization
mapNodeIdNgrams :: (Ord b, Hashable b)
                => [DocumentIdWithNgrams a b]
                -> HashMap b
                       (Map NgramsType 
                            (Map NodeId Int)
                       )
mapNodeIdNgrams = HashMap.unionsWith (Map.unionWith (Map.unionWith (+))) . fmap f
  where
    f :: DocumentIdWithNgrams a b
      -> HashMap b (Map NgramsType (Map NodeId Int))
    f d = fmap (fmap (Map.singleton nId)) $ documentNgrams d
      where
        nId = _index $ documentWithId d


------------------------------------------------------------------------
instance ExtractNgramsT HyperdataContact
  where
    extractNgramsT l hc = HashMap.mapKeys (cleanExtractedNgrams 255) <$> extract l hc
      where
        extract :: TermType Lang -> HyperdataContact
                -> Cmd err (HashMap ExtractedNgrams (Map NgramsType Int))
        extract _l hc' = do
          let authors = map text2ngrams
                      $ maybe ["Nothing"] (\a -> [a])
                      $ view (hc_who . _Just . cw_lastName) hc'

          pure $ HashMap.fromList $ [(SimpleNgrams a', Map.singleton Authors 1) | a' <- authors ]


instance ExtractNgramsT HyperdataDocument
  where
    extractNgramsT :: TermType Lang
                   -> HyperdataDocument
                   -> Cmd err (HashMap ExtractedNgrams (Map NgramsType Int))
    extractNgramsT lang hd = HashMap.mapKeys (cleanExtractedNgrams 255) <$> extractNgramsT' lang hd
      where
        extractNgramsT' :: TermType Lang
                        -> HyperdataDocument
                       -> Cmd err (HashMap ExtractedNgrams (Map NgramsType Int))
        extractNgramsT' lang' doc = do
          let source    = text2ngrams
                        $ maybe "Nothing" identity
                        $ _hd_source doc

              institutes = map text2ngrams
                         $ maybe ["Nothing"] (map toSchoolName . (splitOn ", "))
                         $ _hd_institutes doc

              authors    = map text2ngrams
                         $ maybe ["Nothing"] (splitOn ", ")
                         $ _hd_authors doc

          terms' <- map (enrichedTerms (lang' ^. tt_lang) CoreNLP NP)
                 <$> concat
                 <$> liftBase (extractTerms lang' $ hasText doc)

          pure $ HashMap.fromList
               $  [(SimpleNgrams source, Map.singleton Sources     1)                    ]
               <> [(SimpleNgrams     i', Map.singleton Institutes  1) | i' <- institutes ]
               <> [(SimpleNgrams     a', Map.singleton Authors     1) | a' <- authors    ]
               <> [(EnrichedNgrams   t', Map.singleton NgramsTerms 1) | t' <- terms'     ]

instance (ExtractNgramsT a, HasText a) => ExtractNgramsT (Node a)
  where
    extractNgramsT l (Node _ _ _ _ _ _ _ h) = extractNgramsT l h

instance HasText a => HasText (Node a)
  where
    hasText (Node _ _ _ _ _ _ _ h) = hasText h



-- | TODO putelsewhere
-- | Upgrade function
-- Suppose all documents are English (this is the case actually)
indexAllDocumentsWithPosTag :: FlowCmdM env err m => m ()
indexAllDocumentsWithPosTag = do
  rootId    <- getRootId (UserName userMaster)
  corpusIds <- findNodesId rootId [NodeCorpus]
  docs      <- List.concat <$> mapM getDocumentsWithParentId corpusIds

  printDebug "Nb of docs" (List.length docs)

  let documentsWithId = map (\doc -> Indexed (doc ^. node_id) doc) docs

  mapNgramsDocs' :: HashMap ExtractedNgrams (Map NgramsType (Map NodeId Int))
                <- mapNodeIdNgrams
                <$> documentIdWithNgrams
                    (extractNgramsT $ withLang (Multi EN) documentsWithId)
                    documentsWithId

  _ <- insertExtractedNgrams $ HashMap.keys mapNgramsDocs'

  pure ()












