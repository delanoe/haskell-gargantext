{-|
Module      : Gargantext.Database.Flow.Utils
Description : Database Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Database.Action.Flow.Utils
    where

import Data.Map.Strict (Map)
import Data.HashMap.Strict (HashMap)
import Gargantext.Core.Types (TermsCount)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Database.Query.Table.ContextNodeNgrams
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Types
import Gargantext.Prelude
import Control.Lens ((^.))
import qualified Data.Map.Strict as DM
import qualified Data.HashMap.Strict as HashMap


data DocumentIdWithNgrams a b =
     DocumentIdWithNgrams
     { documentWithId :: Indexed NodeId a
     , documentNgrams :: HashMap b (Map NgramsType Int, TermsCount)
     } deriving (Show)

insertDocNgrams :: ListId
                -> HashMap (Indexed NgramsId Ngrams) (Map NgramsType (Map DocId (Int, TermsCount)))
                -> Cmd err Int
insertDocNgrams lId m = do
  -- printDebug "[insertDocNgrams] ns" ns
  insertContextNodeNgrams ns
  where
    ns = [ ContextNodeNgrams docId lId (ng^.index)
                                   (ngramsTypeId t)
                                   (fromIntegral i)
                                   cnt
         | (ng, t2n2i) <- HashMap.toList m
         , (t,  n2i)   <- DM.toList t2n2i
         , (docId,  (i, cnt))     <- DM.toList n2i
         ]

-- [(NodeId, {Ngrams: ({NgramsType: Int}, TermsCount)})]
-- {Ngrams: {NgramsType: {NodeId: (Int, TermsCount)}}}
