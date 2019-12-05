{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- {-# LANGUAGE Arrows #-}

module Gargantext.Database.Flow.Pairing
  (pairing)
    where

--import Debug.Trace (trace)
import Control.Lens (_Just, (^.))
import Database.PostgreSQL.Simple.SqlQQ (sql)
-- import Opaleye
-- import Opaleye.Aggregate
-- import Control.Arrow (returnA)
import Data.Maybe (catMaybes)
import Data.Map (Map, fromList)
import Safe (lastMay)
import qualified Data.Map as DM
import Data.Text (Text, toLower)
import qualified Data.Text as DT
import Gargantext.Prelude hiding (sum)
import Gargantext.Database.Schema.Ngrams -- (NgramsType(..))
import Gargantext.Database.Node.Contact -- (HyperdataContact(..))
import Gargantext.Database.Flow.Utils
import Gargantext.Database.Utils (Cmd, runPGSQuery)
import Gargantext.Database.Types.Node (AnnuaireId, CorpusId, ListId)
import Gargantext.Database.Node.Children (getAllContacts)

-- TODO mv this type in Types Main
type Terms = Text

-- | TODO : add paring policy as parameter
pairing :: AnnuaireId
        -> CorpusId
        -> ListId
        -> Cmd err Int
pairing aId cId lId = do
  contacts' <- getAllContacts aId
  let contactsMap = pairingPolicyToMap toLower
                  $ toMaps extractNgramsT contacts'

  ngramsMap' <- getNgramsTindexed cId Authors
  let ngramsMap = pairingPolicyToMap lastName ngramsMap'

  let indexedNgrams = pairMaps contactsMap ngramsMap

  insertDocNgrams lId indexedNgrams

lastName :: Terms -> Terms
lastName texte = DT.toLower
               $ maybe texte (\x -> if DT.length x > 3 then x else texte) (lastName' texte)
  where
    lastName' = lastMay . DT.splitOn " "

-- TODO: this method is dangerous (maybe equalities of the result are not taken into account
-- emergency demo plan...)
pairingPolicyToMap :: (Terms -> Terms)
                   -> Map (NgramsT Ngrams) a
                   -> Map (NgramsT Ngrams) a
pairingPolicyToMap f = DM.mapKeys (pairingPolicy f)

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
                      JOIN nodes_nodes      nn  ON nn.node2_id   = occ.node2_id

                      WHERE nn.node1_id     = ?
                        AND occ.ngrams_type = ?
                        AND occ.node2_id = nn.node2_id
                      GROUP BY n.id;
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
