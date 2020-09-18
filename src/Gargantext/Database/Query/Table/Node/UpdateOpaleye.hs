{-|
Module      : Gargantext.Database.Node.UpdateOpaleye
Description : Update Node in Database (Postgres)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE QuasiQuotes       #-}


module Gargantext.Database.Query.Table.Node.UpdateOpaleye
  where


import Opaleye
import Data.Aeson (encode, ToJSON)
import Gargantext.Prelude
import Gargantext.Database.Schema.Node 
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude (Cmd, mkCmd)

updateHyperdata :: ToJSON a => NodeId -> a -> Cmd err Int64
updateHyperdata i h = mkCmd $ \c -> runUpdate_ c (updateHyperdataQuery i h)

updateHyperdataQuery :: ToJSON a => NodeId -> a -> Update Int64
updateHyperdataQuery i h = Update
   { uTable      = nodeTable
   , uUpdateWith = updateEasy (\  (Node _ni _nh _nt _nu _np _nn _nd _h)
                                -> Node _ni _nh _nt _nu _np _nn _nd h'
                              )
   , uWhere      = (\row -> _node_id row .== pgNodeId i )
   , uReturning  = rCount
   }
    where h' =  (pgJSONB $ cs $ encode $ h)

