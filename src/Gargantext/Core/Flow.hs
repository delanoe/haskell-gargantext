{-|
Module      : Gargantext.Core.Flow
Description : Core Flow main Types
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Gargantext.Core.Flow where

import Control.Lens ((^.), view, Lens', _Just)
import Data.Map (Map)
import Data.Text (Text)
import Gargantext.Text.Terms (TermType)
import Gargantext.Core (Lang)
import Gargantext.Database.Schema.Ngrams (Ngrams, NgramsType)
import Gargantext.Core.Types.Main (HashId)
import Gargantext.Database.Types.Node -- (HyperdataDocument(..))
import Gargantext.Database.Node.Contact -- (HyperdataContact(..))
import Gargantext.Database.Node.Document.Insert (AddUniqId, InsertDb)
import Gargantext.Database.Utils (Cmd, CmdM)

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
    extractNgramsT :: HasText h => TermType Lang -> h -> Cmd err (Map Ngrams (Map NgramsType Int))

class HasText h
  where
    hasText :: h -> [Text]

------------------------------------------------------------------------

instance UniqId HyperdataDocument
  where
    uniqId = hyperdataDocument_uniqId

instance UniqId HyperdataContact
  where
    uniqId = hc_uniqId


