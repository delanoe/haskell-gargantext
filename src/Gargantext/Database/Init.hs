{-|
Module      : Gargantext.Database.Init
Description : Triggers configuration
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams by node enable contextual metrics.

-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Init
  where

-- import Database.PostgreSQL.Simple.Types (Values(..), QualifiedIdentifier(..))
import Gargantext.Database.Utils (Cmd)
import Gargantext.Prelude
import Gargantext.Database.Triggers.Nodes (triggerSearchUpdate)
import Gargantext.Database.Triggers.NodesNodes (triggerDeleteCount, triggerInsertCount, triggerUpdateAdd, triggerUpdateDel, MasterListId)
import Gargantext.Database.Triggers.NodeNodeNgrams (triggerCountInsert, triggerCountInsert2, triggerCoocInsert)
------------------------------------------------------------------------

initTriggers :: MasterListId -> Cmd err [Int64]
initTriggers lId = do
  t0 <- triggerSearchUpdate
  t1 <- triggerCountInsert
  t1' <- triggerCountInsert2
  t1'' <- triggerCoocInsert
  t2 <- triggerDeleteCount lId
  t3 <- triggerInsertCount lId
  t4 <- triggerUpdateAdd   lId
  t5 <- triggerUpdateDel   lId
  pure [t0,t1,t1',t1'',t2,t3,t4,t5]


