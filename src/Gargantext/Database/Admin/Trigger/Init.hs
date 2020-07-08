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

module Gargantext.Database.Admin.Trigger.Init
  where

import Gargantext.Database.Admin.Trigger.NodeNodeNgrams (triggerCountInsert, triggerCountInsert2)
import Gargantext.Database.Admin.Trigger.Nodes (triggerSearchUpdate)
import Gargantext.Database.Admin.Trigger.NodesNodes (triggerDeleteCount, triggerInsertCount, triggerUpdateAdd, triggerUpdateDel, MasterListId) -- , triggerCoocInsert)
import Gargantext.Database.Prelude (Cmd)
import Gargantext.Prelude

------------------------------------------------------------------------

initTriggers :: MasterListId -> Cmd err [Int64]
initTriggers lId = do
  t0 <- triggerSearchUpdate
  t1 <- triggerCountInsert
  t1' <- triggerCountInsert2
  -- t1'' <- triggerCoocInsert lId
  t2 <- triggerDeleteCount lId
  t3 <- triggerInsertCount lId
  t4 <- triggerUpdateAdd   lId
  t5 <- triggerUpdateDel   lId
  pure [t0
       ,t1
       ,t1'
       -- ,t1''
       ,t2
       ,t3
       ,t4
       ,t5]


