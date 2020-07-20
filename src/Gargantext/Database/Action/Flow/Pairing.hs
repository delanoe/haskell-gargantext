{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

# Spécifications for pairing

database:

add NodeType Community (instead of texts, contacts)

nodes_nodes
corpusId_communitId

get defaultList Id of each (for now)

corpusId_docId
listId_ngramsId (authors)

listId_docId_[ngrams]
listId_contactId_[ngramsId']


if isSame ngramsId ngramsId'
 then
   insert listId_docId_contactId
 else
   nothing


-}

{-# LANGUAGE QuasiQuotes       #-}
-- {-# LANGUAGE Arrows #-}

module Gargantext.Database.Action.Flow.Pairing
  (pairing)
    where

import Data.Set (Set)
import Control.Lens (_Just, (^.))
import Data.Map (Map, fromList, fromListWith)
import Data.Maybe (catMaybes)
import Data.Text (Text, toLower)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Gargantext.Core.Types (TableResult(..))
import Gargantext.Database.Action.Flow.Utils
import Gargantext.Database.Admin.Types.Node (AnnuaireId, CorpusId, ListId{-, DocId, ContactId-})
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
import Gargantext.Database.Query.Table.Node.Children (getAllContacts)
import Gargantext.Database.Admin.Types.Hyperdata -- (HyperdataContact(..))
import Gargantext.Database.Schema.Ngrams -- (NgramsType(..))
import Gargantext.Prelude hiding (sum)
import Safe (lastMay)
import qualified Data.Map  as DM
import qualified Data.Text as DT
import qualified Data.Set  as Set

-- TODO mv this type in Types Main
type Terms = Text

{-
pairing'' :: (CorpusId, CorpusId) -> (DocId -> DocId)
pairing'' = undefined

pairing' :: (CorpusId, AnnuaireId) -> (DocId -> ContactId)
pairing' = undefined
-}

-- | TODO : add paring policy as parameter
pairing :: CorpusId   -- (CorpusId,   ListId) -- Pair (Either CorpusId AnnuaireId) ListId
        -> AnnuaireId -- (AnnuaireId, ListId) -- Pair (Either CorpusId AnnuaireId) ListId
        -> ListId
        -> Cmd err Int
pairing cId aId lId = do
  contacts'       <- getAllContacts aId
  let contactsMap = pairingPolicyToMap toLower
                  $ toMaps extractNgramsT (tr_docs contacts')

  ngramsMap'    <- getNgramsTindexed cId Authors
  let ngramsMap = pairingPolicyToMap lastName ngramsMap'

  let indexedNgrams = pairMaps contactsMap ngramsMap

  insertDocNgrams lId indexedNgrams

-- TODO: this method is dangerous (maybe equalities of the result are
-- not taken into account emergency demo plan...)
pairingPolicyToMap :: (Terms -> Terms)
                   -> Map (NgramsT Ngrams) a
                   -> Map (NgramsT Ngrams) a
pairingPolicyToMap f = DM.mapKeys (pairingPolicy f)

lastName :: Terms -> Terms
lastName texte = DT.toLower
               $ maybe texte (\x -> if DT.length x > 3 then x else texte)
                             (lastName' texte)
  where
    lastName' = lastMay . DT.splitOn " "


pairingPolicy :: (Terms -> Terms)
              -> NgramsT Ngrams
              -> NgramsT Ngrams
pairingPolicy f (NgramsT nt (Ngrams ng _)) = (NgramsT nt (Ngrams (f ng) 1))

-- | TODO : use Occurrences in place of Int
extractNgramsT :: HyperdataContact
               -> Map (NgramsT Ngrams) Int
extractNgramsT contact = fromList [(NgramsT Authors    a' , 1)| a' <- authors    ]
  where
    authors    = map text2ngrams
               $ catMaybes [ contact^.(hc_who . _Just . cw_lastName) ]


pairMaps :: Map (NgramsT Ngrams) a
         -> Map (NgramsT Ngrams) NgramsId
         -> Map NgramsIndexed (Map NgramsType a)
pairMaps m1 m2 =
  DM.fromList
    [ (NgramsIndexed ng nId, DM.singleton nt n2i)
    | (k@(NgramsT nt ng),n2i) <- DM.toList m1
    , Just nId <- [DM.lookup k m2]
    ]

-----------------------------------------------------------------------
getNgramsTindexed :: CorpusId
                  -> NgramsType
                  -> Cmd err (Map (NgramsT Ngrams) NgramsId)
getNgramsTindexed corpusId ngramsType' = fromList
    <$> map (\(ngramsId',t,n) -> (NgramsT ngramsType' (Ngrams t n),ngramsId'))
    <$> selectNgramsTindexed corpusId ngramsType'
  where
    selectNgramsTindexed :: CorpusId
                         -> NgramsType
                         -> Cmd err [(NgramsId, Terms, Int)]
    selectNgramsTindexed corpusId' ngramsType'' = runPGSQuery selectQuery (corpusId', ngramsTypeId ngramsType'')
      where
        selectQuery = [sql| SELECT n.id,n.terms,n.n from ngrams n
                      JOIN node_node_ngrams occ ON occ.ngrams_id = n.id
                      -- JOIN node_node_ngrams2 occ ON occ.ngrams_id = n.id
                      JOIN nodes_nodes      nn  ON nn.node2_id   = occ.node2_id

                      WHERE nn.node1_id     = ?
                        AND occ.ngrams_type = ?
                        AND occ.node2_id = nn.node2_id
                      GROUP BY n.id;
                     |]

------------------------------------------------------------------------


-- resultPairing ::

type ContactName = Text
type DocAuthor   = Text

data ToProject = ContactName | DocAuthor

type Projected  = Text


type Projection a = Map a Projected


projection :: Set ToProject -> (ToProject -> Projected) -> Projection ToProject
projection = undefined

align :: Projection ContactName      -> Projection DocAuthor
      -> Map ContactName [ContactId] -> Map DocAuthor [DocId]
      -> Map ContactId (Set DocId)
align = undefined

-- insert ContactId_DocId as NodeNode
-- then each ContactId could become a corpus with its DocIds


------------------------------------------------------------------------

getNgramsContactId :: AnnuaireId
                   -> ListId
                   -- -> ContactType
                   -> Cmd err (Map Text [Int])
getNgramsContactId = undefined

-- | TODO
-- filter Trash / map Authors
-- Indexing all ngramsType like Authors
getNgramsDocId :: CorpusId
                  -> ListId
                  -> NgramsType
                  -> Cmd err (Map Text [Int])
getNgramsDocId corpusId listId ngramsType
  = fromListWith (<>)
  <$> map (\(t,nId) -> (t,[nId]))
  <$> selectNgramsDocId corpusId listId ngramsType

selectNgramsDocId :: CorpusId
                   -> ListId
                   -> NgramsType
                   -> Cmd err [(Text, Int)]
selectNgramsDocId corpusId' listId' ngramsType' =
 runPGSQuery selectQuery (corpusId', listId', ngramsTypeId ngramsType')
   where
     selectQuery = [sql| SELECT ng.terms,nnng.node2_id from ngrams ng
                    JOIN node_node_ngrams nnng ON nnng.ngrams_id = ng.id
                    JOIN nodes_nodes      nn   ON nn.node2_id    = nnng.node2_id

                    WHERE nn.node1_id = ?
                      AND nnng.node1_id    = ?
                      AND nnng.ngrams_type = ?
                    ;
                   |]





{- | TODO more typed SQL queries
selectNgramsTindexed :: CorpusId -> NgramsType -> Query NgramsRead
selectNgramsTindexed corpusId ngramsType = proc () -> do
    nodeNode   <- queryNodeNodeTable     -< ()
    nodeNgrams <- queryNodesNgramsTable  -< ()
    ngrams     <- queryNgramsTable       -< ()

    restrict -< node1_id nodeNode .== pgInt4 corpusId
    restrict -< node2_id nodeNode .== node_id nodeNgrams
    restrict -< ngrams_id ngrams  .== node_ngrams nodeNgrams

    result <- aggregate groupBy (ngrams_id ngrams)
    returnA -< result
--}
