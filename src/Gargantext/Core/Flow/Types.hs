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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Gargantext.Core.Flow.Types where

import Control.Lens (Lens')
import Data.Map (Map)
import Data.Maybe (Maybe)
import Gargantext.Text (HasText(..))
import Gargantext.Core.Types.Main (HashId)
import Gargantext.Database.Action.Query.Node.Contact -- (HyperdataContact(..))
import Gargantext.Database.Admin.Types.Node -- (HyperdataDocument(..))
import Gargantext.Database.Schema.Ngrams (Ngrams, NgramsType)
import Gargantext.Prelude

class UniqId a
  where
    uniqId :: Lens' a (Maybe HashId)

instance UniqId HyperdataDocument
  where
    uniqId = hyperdataDocument_uniqId

instance UniqId HyperdataContact
  where
    uniqId = hc_uniqId

data DocumentIdWithNgrams a = DocumentIdWithNgrams
  { documentWithId  :: !(DocumentWithId a)
  , document_ngrams :: !(Map Ngrams (Map NgramsType Int))
  } deriving (Show)

data DocumentWithId a = DocumentWithId
  { documentId   :: !NodeId
  , documentData :: !a
  } deriving (Show)

instance HasText a => HasText (DocumentWithId a)
  where
    hasText (DocumentWithId _ a) = hasText a

