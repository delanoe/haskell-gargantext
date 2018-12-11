{-|
Module      : Gargantext.Database.Queries.Join
Description : Main requests of Node to the database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
------------------------------------------------------------------------
module Gargantext.Database.Queries.Join
  where
------------------------------------------------------------------------

import Control.Applicative ((<*>))
import Control.Arrow ((>>>))
import Data.Profunctor.Product.Default
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Node
import Gargantext.Database.Schema.NodeNode
import Gargantext.Prelude
import Opaleye
import Opaleye.Internal.Join (NullMaker(..))
import qualified Opaleye.Internal.Unpackspec()


--leftJoin3 :: Query columnsL1 -> Query columnsR -> Query columnsL
--     -> ((columnsL1, columnsR) -> Column PGBool)
--     -> ((columnsL, (columnsL1, nullableColumnsR1)) -> Column PGBool)
--     -> Query (columnsL, nullableColumnsR)
--leftJoin3 q1 q2 q3 cond12 cond23 = leftJoin q3 (leftJoin q1 q2 cond12) cond23
join3 :: Query columnsA -> Query columnsB -> Query columnsC 
      -> ((columnsA, columnsB, columnsC) -> Column PGBool) 
      -> Query (columnsA, columnsB, columnsC)
join3 q1 q2 q3 cond = ((,,) <$> q1 <*> q2 <*> q3) >>> keepWhen cond

------------------------------------------------------------------------

leftJoin3Ex :: Query (NodeRead, (NodeNodeReadNull, NodeReadNull))
leftJoin3Ex = leftJoin3 queryNodeNodeTable queryNodeTable queryNodeTable cond12 cond23
    where
         cond12 = undefined
         cond23 :: (NodeRead, (NodeNodeRead, NodeReadNull)) -> Column PGBool
         cond23 = undefined


leftJoin3 :: ( Default Unpackspec columnsL1 columnsL1
             , Default Unpackspec columnsL2 columnsL2
             , Default Unpackspec columnsL3 columnsL3
             
             , Default Unpackspec nullableColumnsL2 nullableColumnsL2
             
             , Default NullMaker  columnsL2  nullableColumnsL2
             , Default NullMaker (columnsL1, nullableColumnsL2) nullableColumnsL3
             )
             =>
              Query columnsL1 -> Query columnsL2 -> Query columnsL3
                -> ((columnsL1, columnsL2) -> Column PGBool)
                -> ((columnsL3, (columnsL1, nullableColumnsL2)) -> Column PGBool)
                -> Query (columnsL3, nullableColumnsL3)
leftJoin3 q1 q2 q3 cond12 cond23 = leftJoin q3 (leftJoin q1 q2 cond12) cond23


leftJoin3'
  :: (Default Unpackspec fieldsL1 fieldsL1,
      Default Unpackspec fieldsL2 fieldsL2,
      Default Unpackspec nullableFieldsR1 nullableFieldsR1,
      Default Unpackspec fieldsR fieldsR,
      Default NullMaker fieldsR nullableFieldsR1,
      Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR2) =>
     Opaleye.Select fieldsR
     -> Opaleye.Select fieldsL2
     -> Opaleye.Select fieldsL1
     -> ((fieldsL2, fieldsR) -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
     -> Opaleye.Select (fieldsL1, nullableFieldsR2)
leftJoin3' q1 q2 q3 cond12 cond23 = leftJoin q3 (leftJoin q2 q1 cond12) cond23

--{-
leftJoin4' :: Query (NodeRead, (NodeReadNull, (NgramsReadNull, NodeReadNull)))
leftJoin4' = leftJoin4 queryNgramsTable queryNodeTable queryNodeTable queryNodeTable cond12 cond23 cond34
    where
         cond12 :: (NgramsRead, NodeRead) -> Column PGBool
         cond12 = undefined
         
         cond23 :: (NodeRead, (NgramsRead, NodeReadNull)) -> Column PGBool
         cond23 = undefined
         
         cond34 :: (NodeRead, (NodeRead, (NgramsReadNull, NodeReadNull))) -> Column PGBool
         cond34 = undefined

{-
rightJoin4' :: Query (((NodeReadNull, NodeReadNull), NodeReadNull), NodeRead)
rightJoin4' = rightJoin4 queryNodeTable queryNodeTable queryNodeTable queryNodeTable cond12 cond23 cond34
    where
         cond12 :: (NodeRead, NodeRead) -> Column PGBool
         cond12 = undefined
         
         cond23 :: ((NodeReadNull, NodeRead), NodeRead) -> Column PGBool
         cond23 = undefined
         
         cond34 :: (((NodeReadNull, NodeReadNull), NodeRead), NodeRead) -> Column PGBool
         cond34 = undefined

--}


leftJoin4
  :: (Default Unpackspec fieldsL1 fieldsL1,
      Default Unpackspec fieldsL2 fieldsL2,
      Default Unpackspec nullableFieldsR1 nullableFieldsR1,
      Default Unpackspec fieldsL3 fieldsL3,
      Default Unpackspec nullableFieldsR2 nullableFieldsR2,
      Default Unpackspec fieldsR fieldsR,
      Default NullMaker fieldsR nullableFieldsR2,
      Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR3,
      Default NullMaker (fieldsL3, nullableFieldsR2) nullableFieldsR1) =>
     Opaleye.Select fieldsL3
     -> Opaleye.Select fieldsR
     -> Opaleye.Select fieldsL2
     -> Opaleye.Select fieldsL1
     -> ((fieldsL3, fieldsR) -> Column PGBool)
     -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
     -> Opaleye.Select (fieldsL1, nullableFieldsR3)
leftJoin4 q1 q2 q3 q4 cond12 cond23 cond34 = leftJoin q4 (leftJoin q3 (leftJoin q1 q2 cond12) cond23) cond34


-- rightJoin4 q1 q2 q3 q4 cond12 cond23 cond34 = rightJoin q4 (rightJoin q3 (rightJoin q1 q2 cond12) cond23) cond34

--{-
leftJoin5' :: Query (NodeRead, (NodeReadNull, (NodeReadNull, (NodeNodeReadNull, NodeSearchReadNull))))
leftJoin5' = leftJoin5 queryNodeSearchTable queryNodeNodeTable queryNodeTable queryNodeTable queryNodeTable cond12 cond23 cond34 cond45
    where
         cond12 :: (NodeNodeRead, NodeSearchRead) -> Column PGBool
         cond12 = undefined
         
         cond23 :: (NodeRead, (NodeNodeRead, NodeSearchReadNull)) -> Column PGBool
         cond23 = undefined
         
         cond34 :: (NodeRead, (NodeRead, (NodeNodeReadNull, NodeSearchReadNull))) -> Column PGBool
         cond34 = undefined
         
         cond45 :: (NodeRead, (NodeRead, (NodeReadNull, (NodeNodeReadNull, NodeSearchReadNull)))) -> Column PGBool
         cond45 = undefined
--}

leftJoin5 :: ( Default Unpackspec fieldsL1 fieldsL1,
               Default Unpackspec fieldsL2 fieldsL2,
               Default Unpackspec nullableFieldsR1 nullableFieldsR1,
               Default Unpackspec fieldsL3 fieldsL3,
               Default Unpackspec nullableFieldsR2 nullableFieldsR2,
               Default Unpackspec fieldsL4 fieldsL4,
               Default Unpackspec nullableFieldsR3 nullableFieldsR3,
               Default Unpackspec fieldsR fieldsR,
               Default NullMaker fieldsR nullableFieldsR3,
               Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR4,
               Default NullMaker (fieldsL3, nullableFieldsR2) nullableFieldsR1,
               Default NullMaker (fieldsL4, nullableFieldsR3) nullableFieldsR2) =>
               Query fieldsR
               -> Query fieldsL4
               -> Query fieldsL3
               -> Query fieldsL2
               -> Query fieldsL1
               -> ((fieldsL4, fieldsR) -> Column PGBool)
               -> ((fieldsL3, (fieldsL4, nullableFieldsR3)) -> Column PGBool)
               -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
               -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
               -> Query (fieldsL1, nullableFieldsR4)
leftJoin5 q1 q2 q3 q4 q5 cond12 cond23 cond34 cond45 = leftJoin q5 (leftJoin q4 (leftJoin q3 (leftJoin q2 q1 cond12) cond23) cond34) cond45


leftJoin6 :: ( Default Unpackspec fieldsL1 fieldsL1,
               Default Unpackspec fieldsL2 fieldsL2,
               Default Unpackspec nullableFieldsR1 nullableFieldsR1,
               Default Unpackspec fieldsL3 fieldsL3,
               Default Unpackspec nullableFieldsR2 nullableFieldsR2,
               Default Unpackspec fieldsL4 fieldsL4,
               Default Unpackspec nullableFieldsR3 nullableFieldsR3,
               Default Unpackspec fieldsL5 fieldsL5,
               Default Unpackspec nullableFieldsR4 nullableFieldsR4,
               Default Unpackspec fieldsR fieldsR,
               Default NullMaker fieldsR nullableFieldsR4,
               Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR5,
               Default NullMaker (fieldsL3, nullableFieldsR2) nullableFieldsR1,
               Default NullMaker (fieldsL4, nullableFieldsR3) nullableFieldsR2,
               Default NullMaker (fieldsL5, nullableFieldsR4) nullableFieldsR3) =>
     Query fieldsR
     -> Query fieldsL5
     -> Query fieldsL4
     -> Query fieldsL3
     -> Query fieldsL2
     -> Query fieldsL1 -> ((fieldsL5, fieldsR) -> Column PGBool)
     -> ((fieldsL4, (fieldsL5, nullableFieldsR4)) -> Column PGBool)
     -> ((fieldsL3, (fieldsL4, nullableFieldsR3)) -> Column PGBool)
     -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
     -> Query (fieldsL1, nullableFieldsR5)
leftJoin6 q1 q2 q3 q4 q5 q6 cond12 cond23 cond34 cond45 cond56 =
  leftJoin q6 (leftJoin q5 (leftJoin q4 (leftJoin q3 (leftJoin q2 q1 cond12) cond23) cond34) cond45) cond56

--{-
leftJoin6' :: Query (NodeRead, (NodeReadNull, (NodeReadNull, (NodeReadNull, (NodeNodeReadNull, NodeSearchReadNull)))))
leftJoin6' = leftJoin6 queryNodeSearchTable queryNodeNodeTable queryNodeTable queryNodeTable queryNodeTable queryNodeTable cond12 cond23 cond34 cond45 cond56
    where
         cond12 :: (NodeNodeRead, NodeSearchRead) -> Column PGBool
         cond12 = undefined
         
         cond23 :: (NodeRead, (NodeNodeRead, NodeSearchReadNull)) -> Column PGBool
         cond23 = undefined
         
         cond34 :: (NodeRead, (NodeRead, (NodeNodeReadNull, NodeSearchReadNull))) -> Column PGBool
         cond34 = undefined
         
         cond45 :: (NodeRead, (NodeRead, (NodeReadNull, (NodeNodeReadNull, NodeSearchReadNull)))) -> Column PGBool
         cond45 = undefined
         
         cond56 :: (NodeRead, (NodeRead, (NodeReadNull, (NodeReadNull, (NodeNodeReadNull, NodeSearchReadNull))))) -> Column PGBool
         cond56 = undefined

--}


