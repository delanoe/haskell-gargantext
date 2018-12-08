{-|
Module      : Gargantext.Database.Flow
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Arrows #-}

module Gargantext.Database.Flow.Pairing
    where

--import Debug.Trace (trace)
import Control.Lens (_Just,view)
import Database.PostgreSQL.Simple (Connection, query)
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
--import Gargantext.Database.Node.Contact -- (HyperdataContact(..))
--import Gargantext.Database.Types.Node -- (Hyperdata(..))
import Gargantext.Database.Node.Contact
import Gargantext.Database.Flow.Utils
import Gargantext.Database.Schema.Node (Cmd, mkCmd)
import Gargantext.Database.Node.Children
import Gargantext.Core.Types.Main
import Gargantext.Core.Types (NodeType(..))
import Gargantext.Database.Bashql (runCmd')

-- TODO mv this type in Types Main
type Terms = Text

-- | TODO : add paring policy as parameter
pairing :: AnnuaireId -> CorpusId -> IO Int
pairing aId cId = do
  contacts' <- runCmd' $ getContacts aId (Just NodeContact)
  let contactsMap = pairingPolicyToMap toLower $ toMaps extractNgramsT contacts'
  
  ngramsMap' <- runCmd' $ getNgramsTindexed cId Authors
  let ngramsMap = pairingPolicyToMap lastName ngramsMap'

  let indexedNgrams = pairMaps contactsMap ngramsMap
  
  runCmd' $ insertToNodeNgrams indexedNgrams
  -- TODO add List

lastName :: Terms -> Terms
lastName texte = DT.toLower $ maybe texte (\x -> if DT.length x > 3 then x else texte) (lastName' texte)
  where
    lastName' = lastMay . DT.splitOn " "

-- TODO: this methods is dangerous (maybe equalities of the result are not taken into account
-- emergency demo plan...
pairingPolicyToMap :: (Terms -> Terms)
                    -> Map (NgramsT Ngrams) a -> Map (NgramsT Ngrams) a
pairingPolicyToMap f = DM.mapKeys (pairingPolicy f)

pairingPolicy :: (Terms -> Terms) -> NgramsT Ngrams -> NgramsT Ngrams
pairingPolicy f (NgramsT nt (Ngrams ng _)) = (NgramsT nt (Ngrams (f ng) 1))

-- | TODO : use Occurrences in place of Int
extractNgramsT :: HyperdataContact -> Map (NgramsT Ngrams) Int
extractNgramsT contact = fromList [(NgramsT Authors    a' , 1)| a' <- authors    ]
  where
    authors    = map text2ngrams $ catMaybes [view (hc_who . _Just . cw_lastName) contact]
--}


pairMaps :: Map (NgramsT Ngrams) (Map ContactId Int)
         -> Map (NgramsT Ngrams) NgramsId
         -> Map (NgramsT NgramsIndexed) (Map ContactId Int)
pairMaps m1 m2 = DM.fromList $ catMaybes $ map (\(k,n) -> (,) <$> lookup' k m2 <*> Just n) $ DM.toList m1
  where
    lookup' k@(NgramsT nt ng) m = case DM.lookup k m of
        Nothing -> Nothing
        Just nId -> Just $ NgramsT nt (NgramsIndexed ng nId)


-----------------------------------------------------------------------
getNgramsTindexed:: CorpusId -> NgramsType -> Cmd (Map (NgramsT Ngrams) NgramsId)
getNgramsTindexed corpusId ngramsType' = mkCmd $ \c -> fromList
    <$> map (\(ngramsId',t,n) -> (NgramsT ngramsType' (Ngrams t n),ngramsId'))
    <$> selectNgramsTindexed c corpusId ngramsType'

selectNgramsTindexed :: Connection -> CorpusId -> NgramsType -> IO [(NgramsId, Terms, Int)]
selectNgramsTindexed c corpusId ngramsType'' = query c selectQuery (corpusId, ngramsTypeId ngramsType'')
  where
    selectQuery = [sql| SELECT n.id,n.terms,n.n from ngrams n
                  JOIN nodes_ngrams occ ON occ.ngram_id = n.id
                  JOIN nodes_nodes  nn  ON nn.node2_id = occ.node_id
                  
                  WHERE nn.node1_id     = ?
                    AND occ.ngrams_type = ?
                    AND occ.node_id = nn.node2_id
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


