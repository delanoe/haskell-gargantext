{-|
Module      : Gargantext.Core.Flow.Types
Description : Core Flow main Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Gargantext.Core.Flow.Types where

import Control.Lens
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Schema.Node (node_hash_id)
import Gargantext.Prelude
import Gargantext.Prelude.Crypto.Hash (Hash)

class UniqId a
  where
    uniqId :: Lens' a (Maybe Hash)

instance UniqId HyperdataDocument
  where
    uniqId = hd_uniqId

instance UniqId HyperdataContact
  where
    uniqId = hc_uniqId

instance UniqId (Node a)
  where
    uniqId = node_hash_id


{-
data DocumentIdWithNgrams a = DocumentIdWithNgrams
  { documentWithId  :: !(Indexed NodeId a)
  , documentNgrams :: !(Map Ngrams (Map NgramsType Int))
  } deriving (Show)
-}
