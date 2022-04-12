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
  ( DataText(..)
  , getDataText
  , getDataText_Debug
  , flowDataText
  , flow

  , flowCorpusFile
  , flowCorpus
  , flowAnnuaire
  , insertMasterDocs
  , saveDocNgramsWith

  , getOrMkRoot
  , getOrMk_RootWithCorpus
  , TermType(..)
  , DataOrigin(..)
  , allDataOrigins

  , do_api
  , indexAllDocumentsWithPosTag
  )
    where

import Conduit
import Control.Lens ((^.), view, _Just, makeLenses)
import Data.Aeson.TH (deriveJSON)
import Data.Conduit.Internal (zipSources)
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List (concat)
import Data.Map (Map, lookup)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Swagger
import qualified Data.Text as T
import Data.Traversable (traverse)
import Data.Tuple.Extra (first, second)
import GHC.Generics (Generic)
import Servant.Client (ClientError)
import System.FilePath (FilePath)
import qualified Data.HashMap.Strict as HashMap
import qualified Gargantext.Data.HashMap.Strict.Utils as HashMap
import qualified Data.Map as Map
import qualified Data.Conduit.List as CL
import qualified Data.Conduit      as C

import Gargantext.API.Admin.Orchestrator.Types (JobLog(..))
import Gargantext.Core (Lang(..), PosTagAlgo(..))
import Gargantext.Core.Ext.IMT (toSchoolName)
import Gargantext.Core.Ext.IMTUser (readFile_Annuaire)
import Gargantext.Core.Flow.Types
import Gargantext.Core.Text
import Gargantext.Core.Text.Corpus.Parsers (parseFile, FileFormat, FileType)
import Gargantext.Core.Text.List (buildNgramsLists)
import Gargantext.Core.Text.List.Group.WithStem ({-StopSize(..),-} GroupParams(..))
import Gargantext.Core.Text.List.Social (FlowSocialListWith)
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
import Gargantext.Database.Action.Metrics (updateNgramsOccurrences)
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.ContextNodeNgrams2
import Gargantext.Database.Query.Table.Ngrams
import Gargantext.Database.Query.Table.Node
import Gargantext.Database.Query.Table.Node.Document.Insert -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Query.Table.Node.Error (HasNodeError(..))
import Gargantext.Database.Query.Table.NodeNgrams (listInsertDb , getCgramsId)
import Gargantext.Database.Query.Tree.Root (getOrMkRoot, getOrMk_RootWithCorpus)
import Gargantext.Database.Schema.Node (NodePoly(..), node_id)
import Gargantext.Database.Types
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (Hash)
import qualified Gargantext.Core.Text.Corpus.API as API
import qualified Gargantext.Database.Query.Table.Node.Document.Add  as Doc  (add)
import qualified Prelude

------------------------------------------------------------------------
-- Imports for upgrade function
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
              | DataNew !(Maybe Integer, ConduitT () HyperdataDocument IO ())
              -- | DataNew ![[HyperdataDocument]]

-- Show instance is not possible because of IO
printDataText :: DataText -> IO ()
printDataText (DataOld xs) = putStrLn $ show xs
printDataText (DataNew (maybeInt, conduitData)) = do
  res <- C.runConduit (conduitData .| CL.consume)
  putStrLn $ show (maybeInt, res)

-- TODO use the split parameter in config file
getDataText :: FlowCmdM env err m
            => DataOrigin
            -> TermType Lang
            -> API.Query
            -> Maybe API.Limit
            -> m (Either ClientError DataText)
getDataText (ExternalOrigin api) la q li = liftBase $ do
  eRes <- API.get api (_tt_lang la) q li
  pure $ DataNew <$> eRes

getDataText (InternalOrigin _) _la q _li = do
  (_masterUserId, _masterRootId, cId) <- getOrMk_RootWithCorpus
                                           (UserName userMaster)
                                           (Left "")
                                           (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchDocInDatabase cId (stemIt q)
  pure $ Right $ DataOld ids

getDataText_Debug :: FlowCmdM env err m
            => DataOrigin
            -> TermType Lang
            -> API.Query
            -> Maybe API.Limit
            -> m ()
getDataText_Debug a l q li = do
  result <- getDataText a l q li
  case result of
    Left  err -> liftBase $ putStrLn $ show err
    Right res -> liftBase $ printDataText res



-------------------------------------------------------------------------------
flowDataText :: forall env err m.
                ( FlowCmdM env err m
                )
                => User
                -> DataText
                -> TermType Lang
                -> CorpusId
                -> Maybe FlowSocialListWith
                -> (JobLog -> m ())
                -> m CorpusId
flowDataText u (DataOld ids) tt cid mfslw _ = flowCorpusUser (_tt_lang tt) u (Right [cid]) corpusType ids mfslw
  where
    corpusType = (Nothing :: Maybe HyperdataCorpus)
flowDataText u (DataNew (mLen, txtC)) tt cid mfslw logStatus =
  flowCorpus u (Right [cid]) tt mfslw (mLen, (transPipe liftBase txtC)) logStatus

------------------------------------------------------------------------
-- TODO use proxy
flowAnnuaire :: (FlowCmdM env err m)
             => User
             -> Either CorpusName [CorpusId]
             -> (TermType Lang)
             -> FilePath
             -> (JobLog -> m ())
             -> m AnnuaireId
flowAnnuaire u n l filePath logStatus = do
  -- TODO Conduit for file
  docs <- liftBase $ ((readFile_Annuaire filePath) :: IO [HyperdataContact])
  flow (Nothing :: Maybe HyperdataAnnuaire) u n l Nothing (Just $ fromIntegral $ length docs, yieldMany docs) logStatus

------------------------------------------------------------------------
flowCorpusFile :: (FlowCmdM env err m)
           => User
           -> Either CorpusName [CorpusId]
           -> Limit -- Limit the number of docs (for dev purpose)
           -> TermType Lang
           -> FileType
           -> FileFormat
           -> FilePath
           -> Maybe FlowSocialListWith
           -> (JobLog -> m ())
           -> m CorpusId
flowCorpusFile u n _l la ft ff fp mfslw logStatus = do
  eParsed <- liftBase $ parseFile ft ff fp
  case eParsed of
    Right parsed -> do
      flowCorpus u n la mfslw (Just $ fromIntegral $ length parsed, yieldMany parsed .| mapC toHyperdataDocument) logStatus
      --let docs = splitEvery 500 $ take l parsed
      --flowCorpus u n la mfslw (yieldMany $ map (map toHyperdataDocument) docs) logStatus
    Left e       -> panic $ "Error: " <> T.pack e

------------------------------------------------------------------------
-- | TODO improve the needed type to create/update a corpus
-- (For now, Either is enough)
flowCorpus :: (FlowCmdM env err m, FlowCorpus a)
           => User
           -> Either CorpusName [CorpusId]
           -> TermType Lang
           -> Maybe FlowSocialListWith
           -> (Maybe Integer, ConduitT () a m ())
           -> (JobLog -> m ())
           -> m CorpusId
flowCorpus = flow (Nothing :: Maybe HyperdataCorpus)


flow :: forall env err m a c.
        ( FlowCmdM env err m
        , FlowCorpus a
        , MkCorpus c
        )
        => Maybe c
        -> User
        -> Either CorpusName [CorpusId]
        -> TermType Lang
        -> Maybe FlowSocialListWith
        -> (Maybe Integer, ConduitT () a m ())
        -> (JobLog -> m ())
        -> m CorpusId
flow c u cn la mfslw (mLength, docsC) logStatus = do
  -- TODO if public insertMasterDocs else insertUserDocs
  ids <- runConduit $
      zipSources (yieldMany [1..]) docsC
      .| mapMC insertDoc
      .| sinkList
--  ids <- traverse (\(idx, doc) -> do
--                      id <- insertMasterDocs c la doc
--                      logStatus JobLog { _scst_succeeded = Just $ 1 + idx
--                                       , _scst_failed    = Just 0
--                                       , _scst_remaining = Just $ length docs - idx
--                                       , _scst_events    = Just []
--                                       }
--                      pure id
--                  ) (zip [1..] docs)
  flowCorpusUser (la ^. tt_lang) u cn c ids mfslw

  where
    insertDoc :: (Integer, a) -> m NodeId
    insertDoc (idx, doc) = do
      id <- insertMasterDocs c la [doc]
      case mLength of
        Nothing -> pure ()
        Just len -> do
          logStatus JobLog { _scst_succeeded = Just $ fromIntegral $ 1 + idx
                           , _scst_failed    = Just 0
                           , _scst_remaining = Just $ fromIntegral $ len - idx
                           , _scst_events    = Just []
                           }
      pure $ Prelude.head id
      


------------------------------------------------------------------------
flowCorpusUser :: ( FlowCmdM env err m
                  , MkCorpus c
                  )
               => Lang
               -> User
               -> Either CorpusName [CorpusId]
               -> Maybe c
               -> [NodeId]
               -> Maybe FlowSocialListWith
               -> m CorpusId
flowCorpusUser l user corpusName ctype ids mfslw = do
  -- User Flow
  (userId, _rootId, userCorpusId) <- getOrMk_RootWithCorpus user corpusName ctype
  -- NodeTexts is first
  _tId <- insertDefaultNodeIfNotExists NodeTexts userCorpusId userId
  -- printDebug "NodeTexts: " tId

  -- NodeList is second
  listId <- getOrMkList userCorpusId userId
  -- _cooc  <- insertDefaultNode NodeListCooc listId userId
  -- TODO: check if present already, ignore
  _ <- Doc.add userCorpusId ids

  -- printDebug "Node Text Ids:" tId

  -- User List Flow
  (masterUserId, _masterRootId, masterCorpusId)
    <- getOrMk_RootWithCorpus (UserName userMaster) (Left "") ctype

  --let gp = (GroupParams l 2 3 (StopSize 3))
  -- Here the PosTagAlgo should be chosen according the Lang
  let gp = GroupWithPosTag l CoreNLP HashMap.empty 
  ngs         <- buildNgramsLists user userCorpusId masterCorpusId mfslw gp

  -- printDebug "flowCorpusUser:ngs" ngs

  _userListId <- flowList_DbRepo listId ngs
  _mastListId <- getOrMkList masterCorpusId masterUserId
  -- _ <- insertOccsUpdates userCorpusId mastListId
  -- printDebug "userListId" userListId
  -- User Graph Flow
  _ <- insertDefaultNodeIfNotExists NodeDashboard userCorpusId userId
  _ <- insertDefaultNodeIfNotExists NodeGraph     userCorpusId userId
  --_ <- mkPhylo  userCorpusId userId
  -- Annuaire Flow
  -- _ <- mkAnnuaire  rootUserId userId
  _ <- updateNgramsOccurrences userCorpusId (Just listId)

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

  lId      <- getOrMkList masterCorpusId masterUserId
  _ <- saveDocNgramsWith lId mapNgramsDocs'

  -- _cooc <- insertDefaultNode NodeListCooc lId masterUserId
  pure ids'

saveDocNgramsWith :: ( FlowCmdM env err m)
                  => ListId
                  -> HashMap ExtractedNgrams (Map NgramsType (Map NodeId Int))
                  -> m ()
saveDocNgramsWith lId mapNgramsDocs' = do
  terms2id <- insertExtractedNgrams $ HashMap.keys mapNgramsDocs'
  --printDebug "terms2id" terms2id

  let mapNgramsDocs = HashMap.mapKeys extracted2ngrams mapNgramsDocs'

  -- new
  mapCgramsId <- listInsertDb lId toNodeNgramsW'
               $ map (first _ngramsTerms . second Map.keys)
               $ HashMap.toList mapNgramsDocs

  --printDebug "saveDocNgramsWith" mapCgramsId
  -- insertDocNgrams
  _return <- insertContextNodeNgrams2
           $ catMaybes [ ContextNodeNgrams2 <$> Just nId
                                            <*> (getCgramsId mapCgramsId ngrams_type (_ngramsTerms terms''))
                                            <*> Just (fromIntegral w :: Double)
                       | (terms'', mapNgramsTypes)      <- HashMap.toList mapNgramsDocs
                       , (ngrams_type, mapNodeIdWeight) <- Map.toList mapNgramsTypes
                       , (nId, w)                       <- Map.toList mapNodeIdWeight
                       ]

  -- to be removed
  _   <- insertDocNgrams lId $ HashMap.mapKeys (indexNgrams terms2id) mapNgramsDocs

  pure ()


------------------------------------------------------------------------
-- TODO Type NodeDocumentUnicised
insertDocs :: ( FlowCmdM env err m
              -- , FlowCorpus a
              , FlowInsertDB a
              )
              => UserId
              -> CorpusId
              -> [a]
              -> m ([ContextId], [Indexed ContextId a])
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
                         $ maybe ["Nothing"] (map toSchoolName . (T.splitOn ", "))
                         $ _hd_institutes doc

              authors    = map text2ngrams
                         $ maybe ["Nothing"] (T.splitOn ", ")
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
indexAllDocumentsWithPosTag :: FlowCmdM env err m
                            => m ()
indexAllDocumentsWithPosTag = do
  rootId    <- getRootId (UserName userMaster)
  corpusIds <- findNodesId rootId [NodeCorpus]
  docs      <- List.concat <$> mapM getDocumentsWithParentId corpusIds
  _ <- mapM extractInsert (splitEvery 1000 docs)
  pure ()

extractInsert :: FlowCmdM env err m
              => [Node HyperdataDocument] -> m ()
extractInsert docs = do
  let documentsWithId = map (\doc -> Indexed (doc ^. node_id) doc) docs
  mapNgramsDocs' <- mapNodeIdNgrams
                <$> documentIdWithNgrams
                    (extractNgramsT $ withLang (Multi EN) documentsWithId)
                    documentsWithId
  _ <- insertExtractedNgrams $ HashMap.keys mapNgramsDocs'
  pure ()
