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
import Data.Text (Text)
import Gargantext.Core (Lang)
import Gargantext.Core.Types.Main (HashId)
import Gargantext.Database.Action.Query.Node.Contact -- (HyperdataContact(..))
import Gargantext.Database.Action.Query.Node.Document.Insert (AddUniqId, InsertDb)
import Gargantext.Database.Admin.Types.Node -- (HyperdataDocument(..))
import Gargantext.Database.Admin.Utils (Cmd)
import Gargantext.Database.Schema.Ngrams (Ngrams, NgramsType)
import Gargantext.Prelude
import Gargantext.Text.Terms (TermType)

type FlowCorpus a = ( AddUniqId      a
                    , UniqId         a
                    , InsertDb       a
                    , ExtractNgramsT a
                    , HasText        a
                    )

class UniqId a
  where
    uniqId :: Lens' a (Maybe HashId)

class ExtractNgramsT h
  where
    extractNgramsT :: HasText h
                   => TermType Lang
                   -> h
                   -> Cmd err (Map Ngrams (Map NgramsType Int))

class HasText h
  where
    hasText :: h -> [Text]

instance UniqId HyperdataDocument
  where
    uniqId = hyperdataDocument_uniqId

instance UniqId HyperdataContact
  where
    uniqId = hc_uniqId
