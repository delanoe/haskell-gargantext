{-|
Module      : Gargantext.Database.Metrics.TFICF
Description : Ngrams by Node user and master
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE QuasiQuotes       #-}

module Gargantext.Database.Action.Metrics.TFICF
  where

-- import Debug.Trace (trace)
-- import Gargantext.Core (Lang(..))
import Data.Map.Strict (Map, toList, fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Gargantext.Core.Text.Metrics.TFICF
import Gargantext.Database.Action.Metrics.NgramsByNode (getNodesByNgramsUser, getOccByNgramsOnlyFast)
import Gargantext.Database.Admin.Types.Node -- (ListId, CorpusId, NodeId)
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.NodeNode (selectCountDocs)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

getTficf :: UserCorpusId
         -> MasterCorpusId
         -> NgramsType
         -> Cmd err (Map Text Double)
getTficf cId mId nt = do
  mapTextDoubleLocal <- Map.filter (> 1)
     <$> Map.map (fromIntegral . Set.size)
     <$> getNodesByNgramsUser cId nt

  mapTextDoubleGlobal <- Map.map fromIntegral
                     <$> getOccByNgramsOnlyFast mId nt (Map.keys mapTextDoubleLocal)

  countLocal  <- selectCountDocs cId
  countGlobal <- selectCountDocs mId

  pure $ fromList [ ( t
                    , tficf (TficfInfra (Count n                                               )
                                        (Total $ fromIntegral countLocal ))
                            (TficfSupra (Count $ fromMaybe 0 $ Map.lookup t mapTextDoubleGlobal)
                                        (Total $ fromIntegral countGlobal))
                    )
                  | (t, n) <- toList mapTextDoubleLocal
                  ]

