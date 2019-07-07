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

{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}

module Gargantext.Database.Flow -- (flowDatabase, ngrams2list)
    where
import Prelude (String)
import Debug.Trace (trace)
import Control.Lens ((^.), view, _Just)
import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (concat)
import Data.Map (Map, lookup, toList)
import Data.Maybe (Maybe(..), catMaybes)
import Data.Monoid
import Data.Text (Text, splitOn, intercalate)
import GHC.Show (Show)
import Gargantext.API.Ngrams (HasRepoVar)
import Gargantext.API.Ngrams (NgramsElement(..), putListNgrams, RepoCmdM)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (NodePoly(..), Terms(..))
import Gargantext.Core.Types.Individu (Username)
import Gargantext.Core.Flow
import Gargantext.Core.Types.Main
import Gargantext.Database.Config (userMaster, corpusMasterName)
import Gargantext.Database.Flow.Utils (insertDocNgrams)
import Gargantext.Database.Node.Contact -- (HyperdataContact(..), ContactWho(..))
import Gargantext.Database.Node.Document.Insert -- (insertDocuments, ReturnId(..), addUniqIdsDoc, addUniqIdsContact, ToDbData(..))
import Gargantext.Database.Root (getRoot)
import Gargantext.Database.Schema.Ngrams -- (insertNgrams, Ngrams(..), NgramsIndexed(..), indexNgrams,  NgramsType(..), text2ngrams, ngramsTypeId)
import Gargantext.Database.Schema.Node -- (mkRoot, mkCorpus, getOrMkList, mkGraph, mkPhylo, mkDashboard, mkAnnuaire, getCorporaWithParentId, HasNodeError, NodeError(..), nodeError)
import Gargantext.Database.Schema.User (getUser, UserLight(..))
import Gargantext.Database.TextSearch (searchInDatabase)
import Gargantext.Database.Types.Node -- (HyperdataDocument(..), NodeType(..), NodeId, UserId, ListId, CorpusId, RootId, MasterCorpusId, MasterUserId)
import Gargantext.Database.Utils (Cmd, CmdM)
import Gargantext.Ext.IMT (toSchoolName)
import Gargantext.Ext.IMTUser (deserialiseImtUsersFromFile)
import Gargantext.Prelude
import Gargantext.Text.Terms.Eleve (buildTries, toToken)
import Gargantext.Text.List (buildNgramsLists,StopSize(..))
import Gargantext.Text.Corpus.Parsers (parseFile, FileFormat)
import qualified Gargantext.Text.Corpus.API.Isidore as Isidore
import Gargantext.Text.Terms (TermType(..), tt_lang, extractTerms, uniText)
import Gargantext.Text.Terms.Mono.Stem.En (stemIt)
import Servant (ServantErr)
import System.FilePath (FilePath)
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified Gargantext.Database.Node.Document.Add  as Doc  (add)
import qualified Gargantext.Text.Corpus.Parsers.GrandDebat as GD

type FlowCmdM env err m =
  ( CmdM     env err m
  , RepoCmdM env err m
  , HasNodeError err
  , HasRepoVar env
  )

------------------------------------------------------------------------

data ApiQuery = ApiIsidoreQuery Text | ApiIsidoreAuth Text
-- | APIs
-- TODO instances
getDataApi :: Lang
           -> Maybe Limit
           -> ApiQuery
           -> IO [HyperdataDocument]
getDataApi lang limit (ApiIsidoreQuery q) = Isidore.get lang limit (Just q) Nothing
getDataApi lang limit (ApiIsidoreAuth  q) = Isidore.get lang limit Nothing  (Just q)


flowCorpusApi :: ( FlowCmdM env ServantErr m)
           => Username -> CorpusName
           -> TermType Lang
           -> Maybe Limit
           -> ApiQuery
           -> m CorpusId
flowCorpusApi u n tt l q = do
  docs <- liftIO $ splitEvery 500 <$> getDataApi (_tt_lang tt) l q
  flowCorpus u n tt docs

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
           -> Limit -- Limit the number of docs (for dev purpose)
           -> TermType Lang -> FileFormat -> FilePath
           -> m CorpusId
flowCorpusFile u n l la ff fp = do
  docs <- liftIO ( splitEvery 500
                 <$> take l
                 <$> parseFile ff fp
                 )
  flowCorpus u n la (map (map toHyperdataDocument) docs)

-- TODO query with complex query
flowCorpusSearchInDatabase :: FlowCmdM env err m
          => Username -> Lang -> Text -> m CorpusId
flowCorpusSearchInDatabase u la q = do
  (_masterUserId, _masterRootId, cId) <- getOrMkRootWithCorpus userMaster "" (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchInDatabase cId (stemIt q)
  flowCorpusUser la u q (Nothing :: Maybe HyperdataCorpus) ids


flowCorpusSearchInDatabase' :: FlowCmdM env ServantErr m
          => Username -> Lang -> Text -> m CorpusId
flowCorpusSearchInDatabase' u la q = do
  (_masterUserId, _masterRootId, cId) <- getOrMkRootWithCorpus userMaster "" (Nothing :: Maybe HyperdataCorpus)
  ids <-  map fst <$> searchInDatabase cId (stemIt q)
  flowCorpusUser la u q (Nothing :: Maybe HyperdataCorpus) ids

------------------------------------------------------------------------

flow :: (FlowCmdM env ServantErr m, FlowCorpus a, MkCorpus c)
     => Maybe c -> Username -> CorpusName -> TermType Lang -> [[a]] -> m CorpusId
flow c u cn la docs = do
  ids <- mapM (insertMasterDocs c la ) docs
  flowCorpusUser (la ^. tt_lang) u cn c (concat ids)

flowCorpus :: (FlowCmdM env ServantErr m, FlowCorpus a)
     => Username -> CorpusName -> TermType Lang -> [[a]] -> m CorpusId
flowCorpus = flow (Nothing :: Maybe HyperdataCorpus)


flowCorpusUser :: (FlowCmdM env err m, MkCorpus c)
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
  _ <- mkPhylo  userCorpusId userId
  --}

  -- User Dashboard Flow
  _ <- mkDashboard userCorpusId userId

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
  let documentsWithId = mergeData (toInserted ids) (Map.fromList $ map viewUniqId' hs')
  
  let 
    fixLang (Unsupervised l n s m) = Unsupervised l n s m'
      where
        m' = case m of
          Nothing -> trace ("buildTries here" :: String)
                  $ Just
                  $ buildTries n ( fmap toToken $ uniText
                                                $ Text.intercalate " . "
                                                $ List.concat
                                                $ map hasText documentsWithId
                                 )
          just_m -> just_m
    fixLang l = l

    lang' = fixLang lang
  -- maps :: IO Map Ngrams (Map NgramsType (Map NodeId Int))
  maps <- mapNodeIdNgrams <$> documentIdWithNgrams (extractNgramsT lang') documentsWithId
  terms2id <- insertNgrams $ Map.keys maps
  let indexedNgrams = Map.mapKeys (indexNgrams terms2id) maps
  
  lId <- getOrMkList masterCorpusId masterUserId
  _   <- insertDocNgrams lId indexedNgrams
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


viewUniqId' :: UniqId a => a -> (HashId, a)
viewUniqId' d = maybe err (\h -> (h,d)) (view uniqId d)
      where
        err = panic "[ERROR] Database.Flow.toInsert"


toInserted :: [ReturnId] -> Map HashId ReturnId
toInserted = Map.fromList . map    (\r ->  (reUniqId r, r)    )
                         . filter (\r -> reInserted r == True)

data DocumentWithId a = DocumentWithId
  { documentId   :: !NodeId
  , documentData :: !a
  } deriving (Show)

instance HasText a => HasText (DocumentWithId a)
  where
    hasText (DocumentWithId _ a) = hasText a

mergeData :: Map HashId ReturnId
          -> Map HashId a
          -> [DocumentWithId a]
mergeData rs = catMaybes . map toDocumentWithId . Map.toList
  where
    toDocumentWithId (hash,hpd) =
      DocumentWithId <$> fmap reId (lookup hash rs)
                     <*> Just hpd

------------------------------------------------------------------------
data DocumentIdWithNgrams a = DocumentIdWithNgrams
  { documentWithId  :: !(DocumentWithId a)
  , document_ngrams :: !(Map Ngrams (Map NgramsType Int))
  } deriving (Show)


instance HasText HyperdataContact
  where
    hasText = undefined

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
    extractNgramsT :: TermType Lang -> HyperdataDocument -> Cmd err (Map Ngrams (Map NgramsType Int))
    extractNgramsT lang hd = filterNgramsT 255 <$> extractNgramsT' lang hd
      where
        extractNgramsT' :: TermType Lang -> HyperdataDocument
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
                 <$> liftIO (extractTerms lang' $ hasText doc)
    
          pure $ Map.fromList $  [(source, Map.singleton Sources 1)]
                             <> [(i', Map.singleton Institutes  1) | i' <- institutes ]
                             <> [(a', Map.singleton Authors     1) | a' <- authors    ]
                             <> [(t', Map.singleton NgramsTerms 1) | t' <- terms'     ]
    

filterNgramsT :: Int -> Map Ngrams (Map NgramsType Int)
                     -> Map Ngrams (Map NgramsType Int)
filterNgramsT s ms = Map.fromList $ map (\a -> filter' s a) $ Map.toList ms
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
      e <- f $ documentData         d
      pure   $ DocumentIdWithNgrams d e


-- FLOW LIST
-- | TODO check optimization
mapNodeIdNgrams :: [DocumentIdWithNgrams a]
                -> Map Ngrams (Map NgramsType (Map NodeId Int))
mapNodeIdNgrams = Map.unionsWith (Map.unionWith (Map.unionWith (+))) . fmap f
  where
    f :: DocumentIdWithNgrams a
      -> Map Ngrams (Map NgramsType (Map NodeId Int))
    f d = fmap (fmap (Map.singleton nId)) $ document_ngrams d
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
  --trace (show $ List.filter (\n -> _ne_ngrams n == "versatile") $ List.concat $ Map.elems ngs) $ listInsert lId ngs
  pure lId

