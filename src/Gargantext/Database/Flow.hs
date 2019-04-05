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

-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}

module Gargantext.Database.Flow -- (flowDatabase, ngrams2list)
    where

--import Gargantext.Database.Metrics.Count (getNgramsElementsWithParentNodeId)
--import Gargantext.Database.Metrics.TFICF (getTficf)
--import Gargantext.Database.Node.Contact (HyperdataContact(..))
--import Gargantext.Database.Schema.NodeNgram (NodeNgramPoly(..), insertNodeNgrams)
--import Gargantext.Database.Schema.NodeNgramsNgrams (NodeNgramsNgramsPoly(..), insertNodeNgramsNgramsNew)
--import Gargantext.Database.Schema.User (insertUsers, simpleUser, gargantuaUser)
import Gargantext.Ext.IMTUser (deserialiseImtUsersFromFile)
--import Gargantext.Text.Metrics.TFICF (Tficf(..))
--import Debug.Trace (trace)
import Control.Lens ((^.), view, Lens', _Just)
import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (concat)
import Data.Map (Map, lookup, toList)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Monoid
import Data.Text (Text, splitOn, intercalate)
import GHC.Show (Show)
import Gargantext.API.Ngrams (HasRepoVar)
import Gargantext.API.Ngrams (NgramsElement, putListNgrams, RepoCmdM)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (NodePoly(..), Terms(..))
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Core.Types.Main
import Gargantext.Database.TextSearch (searchInDatabase)
import Gargantext.Database.Config (userMaster, corpusMasterName)
import Gargantext.Database.Flow.Utils (insertToNodeNgrams)
import Gargantext.Database.Node.Document.Insert -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Node.Contact -- (HyperdataContact(..), ContactWho(..))
import Gargantext.Database.Root (getRoot)
import Gargantext.Database.Schema.Ngrams -- (insertNgrams, Ngrams(..), NgramsIndexed(..), indexNgrams,  NgramsType(..), text2ngrams, ngramsTypeId)
import Gargantext.Database.Schema.Node -- (mkRoot, mkCorpus, getOrMkList, mkGraph, mkDashboard, mkAnnuaire, getCorporaWithParentId, HasNodeError, NodeError(..), nodeError)
import Gargantext.Database.Schema.User (getUser, UserLight(..))
import Gargantext.Database.Types.Node -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Utils (Cmd, CmdM)
import Gargantext.Ext.IMT (toSchoolName)
import Gargantext.Prelude
import Gargantext.Text.List (buildNgramsLists,StopSize(..))
import Gargantext.Text.Parsers (parseDocs, FileFormat)
import Gargantext.Text.Terms (TermType(..), tt_lang)
import Gargantext.Text.Terms (extractTerms)
import Gargantext.Text.Terms.Mono.Stem.En (stemIt)
import qualified Gargantext.Text.Parsers.GrandDebat as GD
import Servant (ServantErr)
import System.FilePath (FilePath)
import qualified Data.Map as DM
import qualified Data.Text as Text
import qualified Gargantext.Database.Node.Document.Add  as Doc  (add)

type FlowCmdM env err m =
  ( CmdM     env err m
  , RepoCmdM env err m
  , HasNodeError err
  , HasRepoVar env
  )

type FlowCorpus a = ( AddUniqId a
                    , UniqId a
                    , InsertDb a
                    , ExtractNgramsT a
                    )

------------------------------------------------------------------------

flowAnnuaire :: FlowCmdM env ServantErr m 
             => Username -> CorpusName -> (TermType Lang) -> FilePath -> m AnnuaireId
flowAnnuaire u n l filePath = do
  docs <- liftIO $ (( splitEvery 500 <$> deserialiseImtUsersFromFile filePath) :: IO [[HyperdataContact]])
  flow (Nothing :: Maybe HyperdataAnnuaire) u n l docs


flowCorpusDebat :: FlowCmdM env ServantErr m
            => Username -> CorpusName
            -> Limit -> FilePath
            -> m CorpusId
flowCorpusDebat u n l fp = do
  docs <- liftIO ( splitEvery 500
                 <$> take l
                 <$> GD.readFile fp
                 :: IO [[GD.GrandDebatReference ]]
                 )
  flowCorpus u n (Multi FR) (map (map toHyperdataDocument) docs)


flowCorpusFile :: FlowCmdM env ServantErr m
           => Username -> CorpusName
           -> Limit -- ^ Limit the number of docs (for dev purpose)
           -> TermType Lang -> FileFormat -> FilePath
           -> m CorpusId
flowCorpusFile u n l la ff fp = do
  docs <- liftIO ( splitEvery 500
                 <$> take l
                 <$> parseDocs ff fp
                 )
  flowCorpus u n la (map (map toHyperdataDocument) docs)

-- TODO query with complex query
flowCorpusSearchInDatabase :: FlowCmdM env ServantErr m
          => Username -> Lang -> Text -> m CorpusId
flowCorpusSearchInDatabase u la q = do
  (_masterUserId, _masterRootId, cId) <- getOrMkRootWithCorpus userMaster "" (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchInDatabase cId (stemIt q)
  flowCorpusUser la u q (Nothing :: Maybe HyperdataCorpus) ids

------------------------------------------------------------------------

-- TODO-ACCESS: check uId CanInsertDoc pId && checkDocType nodeType
-- TODO-EVENTS: InsertedNodes


flow :: (FlowCmdM env ServantErr m, FlowCorpus a, MkCorpus c)
     => Maybe c -> Username -> CorpusName -> TermType Lang -> [[a]] -> m CorpusId
flow c u cn la docs = do
  ids <- mapM (insertMasterDocs c la ) docs
  flowCorpusUser (la ^. tt_lang) u cn c (concat ids)

flowCorpus :: (FlowCmdM env ServantErr m, FlowCorpus a)
     => Username -> CorpusName -> TermType Lang -> [[a]] -> m CorpusId
flowCorpus = flow (Nothing :: Maybe HyperdataCorpus)


flowCorpusUser :: (FlowCmdM env ServantErr m, MkCorpus c)
               => Lang -> Username -> CorpusName -> Maybe c -> [NodeId] -> m CorpusId
flowCorpusUser l userName corpusName ctype ids = do
  -- User Flow
  (userId, _rootId, userCorpusId) <- getOrMkRootWithCorpus userName corpusName ctype
  -- TODO: check if present already, ignore
  _ <- Doc.add userCorpusId ids

  -- User List Flow
  --{-
  (_masterUserId, _masterRootId, masterCorpusId) <- getOrMkRootWithCorpus userMaster "" ctype
  ngs         <- buildNgramsLists l 2 3 (StopSize 3) userCorpusId masterCorpusId
  userListId  <- flowList userId userCorpusId ngs
  printDebug "userListId" userListId
  -- User Graph Flow
  _ <- mkGraph  userCorpusId userId
  --}

  -- User Dashboard Flow
  -- _ <- mkDashboard userCorpusId userId

  -- Annuaire Flow
  -- _ <- mkAnnuaire  rootUserId userId
  pure userCorpusId


insertMasterDocs :: ( FlowCmdM env ServantErr m
                    , FlowCorpus a
                    , MkCorpus   c
                    )
                 => Maybe c -> TermType Lang -> [a] -> m [DocId]
insertMasterDocs c lang hs  =  do
  (masterUserId, _, masterCorpusId) <- getOrMkRootWithCorpus userMaster corpusMasterName c

  -- TODO Type NodeDocumentUnicised
  let hs' = map addUniqId hs
  ids <- insertDb masterUserId masterCorpusId hs'
  let documentsWithId = mergeData (toInserted ids) (DM.fromList $ map viewUniqId' hs')
  
  docsWithNgrams     <- documentIdWithNgrams (extractNgramsT lang) documentsWithId

  let maps            = mapNodeIdNgrams docsWithNgrams

  terms2id <- insertNgrams $ DM.keys maps
  let indexedNgrams = DM.mapKeys (indexNgrams terms2id) maps
  _                <- insertToNodeNgrams indexedNgrams
  pure $ map reId ids



type CorpusName = Text

getOrMkRootWithCorpus :: (HasNodeError err, MkCorpus a)
              => Username -> CorpusName -> Maybe a
              -> Cmd err (UserId, RootId, CorpusId)
getOrMkRootWithCorpus username cName c = do
  maybeUserId <- getUser username
  userId <- case maybeUserId of
        Nothing   -> nodeError NoUserFound
        Just user -> pure $ userLight_id user

  rootId' <- map _node_id <$> getRoot username

  rootId'' <- case rootId' of
        []  -> mkRoot username userId
        n   -> case length n >= 2 of
            True  -> nodeError ManyNodeUsers
            False -> pure rootId'

  rootId <- maybe (nodeError NoRootFound) pure (head rootId'')

  corpusId'' <- if username == userMaster
                  then do
                    ns <- getCorporaWithParentId rootId
                    pure $ map _node_id ns
                  else
                    pure []
  
  corpusId' <- if corpusId'' /= []
                  then pure corpusId''
                  else mk (Just cName) c rootId userId

  corpusId <- maybe (nodeError NoCorpusFound) pure (head corpusId')

  pure (userId, rootId, corpusId)


------------------------------------------------------------------------


class UniqId a
  where
    uniqId :: Lens' a (Maybe HashId)


instance UniqId HyperdataDocument
  where
    uniqId = hyperdataDocument_uniqId

instance UniqId HyperdataContact
  where
    uniqId = hc_uniqId

viewUniqId' :: UniqId a => a -> (HashId, a)
viewUniqId' d = maybe err (\h -> (h,d)) (view uniqId d)
      where
        err = panic "[ERROR] Database.Flow.toInsert"


toInserted :: [ReturnId] -> Map HashId ReturnId
toInserted = DM.fromList . map    (\r ->  (reUniqId r, r)    )
                         . filter (\r -> reInserted r == True)

data DocumentWithId a = DocumentWithId
  { documentId   :: !NodeId
  , documentData :: !a
  } deriving (Show)

mergeData :: Map HashId ReturnId
          -> Map HashId a
          -> [DocumentWithId a]
mergeData rs = catMaybes . map toDocumentWithId . DM.toList
  where
    toDocumentWithId (hash,hpd) =
      DocumentWithId <$> fmap reId (lookup hash rs)
                     <*> Just hpd

------------------------------------------------------------------------
data DocumentIdWithNgrams a = DocumentIdWithNgrams
  { documentWithId  :: !(DocumentWithId a)
  , document_ngrams :: !(Map Ngrams (Map NgramsType Int))
  } deriving (Show)

-- TODO extractNgrams according to Type of Data

class ExtractNgramsT h
  where
    extractNgramsT :: TermType Lang -> h -> Cmd err (Map Ngrams (Map NgramsType Int))


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
        
          pure $ DM.fromList $ [(a', DM.singleton Authors     1) | a' <- authors    ]




instance ExtractNgramsT HyperdataDocument
  where
    extractNgramsT = extractNgramsT'

extractNgramsT' :: TermType Lang -> HyperdataDocument
               -> Cmd err (Map Ngrams (Map NgramsType Int))
extractNgramsT' lang hd = filterNgramsT 255 <$> extractNgramsT'' lang hd
  where
    extractNgramsT'' :: TermType Lang -> HyperdataDocument
                   -> Cmd err (Map Ngrams (Map NgramsType Int))
    extractNgramsT'' lang' doc = do
      let source    = text2ngrams
                    $ maybe "Nothing" identity
                    $ _hyperdataDocument_source doc

          institutes = map text2ngrams
                     $ maybe ["Nothing"] (map toSchoolName . (splitOn ", "))
                     $ _hyperdataDocument_institutes doc

          authors    = map text2ngrams
                     $ maybe ["Nothing"] (splitOn ", ")
                     $ _hyperdataDocument_authors doc

          leText = catMaybes [ _hyperdataDocument_title    doc
                             , _hyperdataDocument_abstract doc
                             ]

      terms' <- map text2ngrams
             <$> map (intercalate " " . _terms_label)
             <$> concat
             <$> liftIO (extractTerms lang' leText)

      pure $ DM.fromList $  [(source, DM.singleton Sources 1)]
                         <> [(i', DM.singleton Institutes  1) | i' <- institutes ]
                         <> [(a', DM.singleton Authors     1) | a' <- authors    ]
                         <> [(t', DM.singleton NgramsTerms 1) | t' <- terms'     ]


filterNgramsT :: Int -> Map Ngrams (Map NgramsType Int)
                     -> Map Ngrams (Map NgramsType Int)
filterNgramsT s ms = DM.fromList $ map (\a -> filter' s a) $ DM.toList ms
  where
    filter' s' (ng@(Ngrams t n),y) = case (Text.length t) < s' of
          True  -> (ng,y)
          False -> (Ngrams (Text.take s' t) n , y)


documentIdWithNgrams :: HasNodeError err
                     => (a
                     -> Cmd err (Map Ngrams (Map NgramsType Int)))
                     -> [DocumentWithId a]
                     -> Cmd err [DocumentIdWithNgrams a]
documentIdWithNgrams f = mapM toDocumentIdWithNgrams
  where
    toDocumentIdWithNgrams d = do
      e <- f $ documentData d
      pure $ DocumentIdWithNgrams d e



-- FLOW LIST
-- | TODO check optimization
mapNodeIdNgrams :: [DocumentIdWithNgrams a]
                -> Map Ngrams (Map NgramsType (Map NodeId Int))
mapNodeIdNgrams = DM.unionsWith (DM.unionWith (DM.unionWith (+))) . fmap f
  where
    f :: DocumentIdWithNgrams a
      -> Map Ngrams (Map NgramsType (Map NodeId Int))
    f d = fmap (fmap (DM.singleton nId)) $ document_ngrams d
      where
        nId = documentId $ documentWithId d

------------------------------------------------------------------------
listInsert :: FlowCmdM env err m
             => ListId -> Map NgramsType [NgramsElement]
             -> m ()
listInsert lId ngs = mapM_ (\(typeList, ngElmts)
                             -> putListNgrams lId typeList ngElmts
                             ) $ toList ngs

flowList :: FlowCmdM env err m => UserId -> CorpusId
         -> Map NgramsType [NgramsElement]
         -> m ListId
flowList uId cId ngs = do
  lId <- getOrMkList cId uId
  printDebug "listId flowList" lId
  listInsert lId ngs
  pure lId

