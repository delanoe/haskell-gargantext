{-|
Module      : Gargantext.Text.Ngrams.Lists
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Gargantext.Text.List
  where

import Data.Map (Map)
import Data.Text (Text)
import Gargantext.API.Ngrams (NgramsElement, mkNgramsElement, mSetFromList)
import Gargantext.Core (Lang(..))
import Gargantext.Core.Types (ListType(..), MasterCorpusId, UserCorpusId)
import Gargantext.Database.Metrics.NgramsByNode (getTficf', sortTficf, ngramsGroup)
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Database.Utils (Cmd)
import Gargantext.Prelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

buildNgramsList :: UserCorpusId -> MasterCorpusId -> Cmd err (Map NgramsType [NgramsElement])
buildNgramsList uCid mCid = do
            candidates   <- sortTficf <$> getTficf' uCid mCid (ngramsGroup EN 2)
            printDebug "candidate" (length candidates)
            
            let termList = toTermList (isStopTerm . fst) candidates
            printDebug "termlist" (length termList)


            let ngs = map (\(lt, (stm, (_score, setext)))
                                  -> mkNgramsElement stm lt
                                        (Just stm)
                                        (mSetFromList $ Set.toList setext)
                                  ) termList

            pure $ Map.fromList [(NgramsTerms, ngs)]

toTermList :: (a -> Bool) -> [a] -> [(ListType, a)]
toTermList stop ns =  map (toTermList' stop CandidateTerm) xs
                   <> map (toTermList' stop GraphTerm)     ys
                   <> map (toTermList' stop CandidateTerm) zs
    where
      toTermList' stop' l n = case stop' n of
          True  -> (StopTerm, n)
          False -> (l, n)

      -- TODO use % of size of list
      -- TODO user ML
      xs = take a ns
      ys = take b $ drop a ns
      zs = drop b $ drop a ns

      a = 100
      b = 1000

isStopTerm :: Text -> Bool
isStopTerm x = Text.length x < 3


