{-|
Module      : Gargantext.Database.Flow.Types
Description : Types for Flow
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans    #-}

{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}

module Gargantext.Database.Flow.Types
    where

import Data.Map (Map)
import Gargantext.Prelude
import Gargantext.Core.Flow.Types
import Gargantext.API.Ngrams (HasRepoVar, RepoCmdM)
import Gargantext.Database.Schema.Ngrams (Ngrams(..), NgramsType(..))
import Gargantext.Database.Types.Node (NodeId)
import Gargantext.Database.Types.Errors (HasNodeError)
import Gargantext.Database.Utils (CmdM)

type FlowCmdM env err m =
  ( CmdM     env err m
  , RepoCmdM env err m
  , HasNodeError err
  , HasRepoVar env
  )

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


