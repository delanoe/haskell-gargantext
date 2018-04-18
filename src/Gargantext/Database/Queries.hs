{-|
Module      : Gargantext.Database.Queries
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Gargantext.Database.Queries where

import Gargantext.Prelude
import Gargantext.Types (Limit, Offset, NodePoly)
import Data.Maybe (Maybe, maybe)
import Control.Arrow ((>>>))
import Control.Applicative ((<*>))
import Opaleye 
-- (Query, limit, offset)


type NodeWrite = NodePoly  (Maybe (Column  PGInt4              ))
                                  (Column  PGInt4               )
                                  (Column  PGInt4               )
                                  (Column (Nullable PGInt4     ))
                                  (Column (PGText              ))
                                  (Maybe  (Column PGTimestamptz))
                                  (Column  PGJsonb              )
                                  -- (Maybe (Column PGTSVector))

type NodeRead = NodePoly  (Column  PGInt4           )
                          (Column  PGInt4           )
                          (Column  PGInt4           )
                          (Column (Nullable PGInt4 ))
                          (Column (PGText          ))
                          (Column PGTimestamptz     )
                          (Column PGJsonb) 
                          -- (Column PGTSVector)



type NodeReadNull = NodePoly  (Column  (Nullable PGInt4           ))
                              (Column  (Nullable PGInt4           ))
                              (Column  (Nullable PGInt4           ))
                              (Column (Nullable PGInt4 ))
                              (Column (Nullable PGText          ))
                              (Column (Nullable PGTimestamptz     ))
                              (Column (Nullable PGJsonb))




join3 :: Query columnsA -> Query columnsB -> Query columnsC 
      -> ((columnsA, columnsB, columnsC) -> Column PGBool) 
      -> Query (columnsA, columnsB, columnsC)
join3 q1 q2 q3 cond = ((,,) <$> q1 <*> q2 <*> q3) >>> keepWhen cond


--leftJoin3 :: Query columnsL1 -> Query columnsR -> Query columnsL
--     -> ((columnsL1, columnsR) -> Column PGBool)
--     -> ((columnsL, (columnsL1, nullableColumnsR1)) -> Column PGBool)
--     -> Query (columnsL, nullableColumnsR)
--leftJoin3 q1 q2 q3 cond12 cond23 = leftJoin q3 (leftJoin q1 q2 cond12) cond23

limit' ::  Maybe Limit -> Query a -> Query a
limit' maybeLimit query = maybe query (\l -> limit l query) maybeLimit

offset' :: Maybe Offset -> Query a  -> Query a
offset' maybeOffset query = maybe query (\o -> offset o query) maybeOffset



