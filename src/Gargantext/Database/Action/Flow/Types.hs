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
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE InstanceSigs            #-}

module Gargantext.Database.Action.Flow.Types
    where

import Gargantext.Core.Flow.Types
import Gargantext.Text
import Gargantext.Text.Terms
import Gargantext.API.Ngrams (HasRepoVar, RepoCmdM)
import Gargantext.Database.Query.Table.Node.Error (HasNodeError)
import Gargantext.Database.Prelude (CmdM)
import Gargantext.Database.Query.Table.Node.Document.Insert

type FlowCmdM env err m =
  ( CmdM     env err m
  , RepoCmdM env err m
  , HasNodeError err
  , HasRepoVar env
  )

type FlowCorpus a = ( AddUniqId      a
                    , UniqId         a
                    , InsertDb       a
                    , ExtractNgramsT a
                    , HasText        a
                    )

