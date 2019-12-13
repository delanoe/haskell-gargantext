{-|
Module      : Gargantext.Database.Queries.Join
Description : Main Join queries (using Opaleye)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Multiple Join functions with Opaleye.

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

leftJoin3
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
leftJoin3 q1 q2     q3
         cond12 cond23 =
  leftJoin q3 ( leftJoin q2 q1 cond12) cond23


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
     Opaleye.Select fieldsR
     -> Opaleye.Select fieldsL3
     -> Opaleye.Select fieldsL2
     -> Opaleye.Select fieldsL1
     -> ((fieldsL3, fieldsR) -> Column PGBool)
     -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
     -> Opaleye.Select (fieldsL1, nullableFieldsR3)
leftJoin4 q1 q2     q3     q4
         cond12 cond23 cond34 =
  leftJoin q4 ( leftJoin q3
                ( leftJoin q2 q1
                    cond12
                ) cond23
              ) cond34


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
leftJoin5 q1 q2     q3     q4     q5
         cond12 cond23 cond34 cond45 =
  leftJoin q5 ( leftJoin q4
                ( leftJoin q3
                  ( leftJoin q2 q1
                       cond12
                   ) cond23
                 ) cond34
               ) cond45


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
leftJoin6 q1 q2     q3     q4     q5     q6
         cond12 cond23 cond34 cond45 cond56 =
  leftJoin q6 ( leftJoin q5
                ( leftJoin q4
                  ( leftJoin q3
                    ( leftJoin q2 q1
                         cond12
                     ) cond23
                  ) cond34
                ) cond45
              ) cond56


leftJoin7
  :: (Default Unpackspec fieldsL1 fieldsL1,
      Default Unpackspec fieldsL2 fieldsL2,
      Default Unpackspec nullableFieldsR1 nullableFieldsR1,
      Default Unpackspec fieldsL3 fieldsL3,
      Default Unpackspec nullableFieldsR2 nullableFieldsR2,
      Default Unpackspec fieldsL4 fieldsL4,
      Default Unpackspec nullableFieldsR3 nullableFieldsR3,
      Default Unpackspec fieldsL5 fieldsL5,
      Default Unpackspec nullableFieldsR4 nullableFieldsR4,
      Default Unpackspec fieldsL6 fieldsL6,
      Default Unpackspec nullableFieldsR5 nullableFieldsR5,
      Default Unpackspec fieldsR fieldsR,
      Default NullMaker fieldsR nullableFieldsR5,
      Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR6,
      Default NullMaker (fieldsL3, nullableFieldsR2) nullableFieldsR1,
      Default NullMaker (fieldsL4, nullableFieldsR3) nullableFieldsR2,
      Default NullMaker (fieldsL5, nullableFieldsR4) nullableFieldsR3,
      Default NullMaker (fieldsL6, nullableFieldsR5) nullableFieldsR4) =>
     Opaleye.Select fieldsR
     -> Opaleye.Select fieldsL6
     -> Opaleye.Select fieldsL5
     -> Opaleye.Select fieldsL4
     -> Opaleye.Select fieldsL3
     -> Opaleye.Select fieldsL2
     -> Opaleye.Select fieldsL1
     -> ((fieldsL6, fieldsR) -> Column PGBool)
     -> ((fieldsL5, (fieldsL6, nullableFieldsR5)) -> Column PGBool)
     -> ((fieldsL4, (fieldsL5, nullableFieldsR4)) -> Column PGBool)
     -> ((fieldsL3, (fieldsL4, nullableFieldsR3)) -> Column PGBool)
     -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
     -> Opaleye.Select (fieldsL1, nullableFieldsR6)
leftJoin7 q1 q2     q3     q4     q5     q6     q7
         cond12 cond23 cond34 cond45 cond56 cond67 =
  leftJoin q7 ( leftJoin q6
                ( leftJoin q5
                  ( leftJoin q4
                    ( leftJoin q3
                      ( leftJoin q2 q1
                       cond12
                    ) cond23
                   ) cond34
                  ) cond45
                 ) cond56
                ) cond67


leftJoin8
  :: (Default Unpackspec fieldsL1 fieldsL1,
      Default Unpackspec fieldsL2 fieldsL2,
      Default Unpackspec nullableFieldsR1 nullableFieldsR1,
      Default Unpackspec fieldsL3 fieldsL3,
      Default Unpackspec nullableFieldsR2 nullableFieldsR2,
      Default Unpackspec fieldsL4 fieldsL4,
      Default Unpackspec nullableFieldsR3 nullableFieldsR3,
      Default Unpackspec fieldsL5 fieldsL5,
      Default Unpackspec nullableFieldsR4 nullableFieldsR4,
      Default Unpackspec fieldsL6 fieldsL6,
      Default Unpackspec nullableFieldsR5 nullableFieldsR5,
      Default Unpackspec fieldsL7 fieldsL7,
      Default Unpackspec nullableFieldsR6 nullableFieldsR6,
      Default Unpackspec fieldsR fieldsR,
      Default NullMaker fieldsR nullableFieldsR6,
      Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR7,
      Default NullMaker (fieldsL3, nullableFieldsR2) nullableFieldsR1,
      Default NullMaker (fieldsL4, nullableFieldsR3) nullableFieldsR2,
      Default NullMaker (fieldsL5, nullableFieldsR4) nullableFieldsR3,
      Default NullMaker (fieldsL6, nullableFieldsR5) nullableFieldsR4,
      Default NullMaker (fieldsL7, nullableFieldsR6) nullableFieldsR5) =>
     Opaleye.Select fieldsR
     -> Opaleye.Select fieldsL7
     -> Opaleye.Select fieldsL6
     -> Opaleye.Select fieldsL5
     -> Opaleye.Select fieldsL4
     -> Opaleye.Select fieldsL3
     -> Opaleye.Select fieldsL2
     -> Opaleye.Select fieldsL1
     -> ((fieldsL7, fieldsR) -> Column PGBool)
     -> ((fieldsL6, (fieldsL7, nullableFieldsR6)) -> Column PGBool)
     -> ((fieldsL5, (fieldsL6, nullableFieldsR5)) -> Column PGBool)
     -> ((fieldsL4, (fieldsL5, nullableFieldsR4)) -> Column PGBool)
     -> ((fieldsL3, (fieldsL4, nullableFieldsR3)) -> Column PGBool)
     -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
     -> Opaleye.Select (fieldsL1, nullableFieldsR7)
leftJoin8 q1 q2     q3     q4     q5     q6     q7     q8
         cond12 cond23 cond34 cond45 cond56 cond67 cond78 =
  leftJoin q8 ( leftJoin q7
                ( leftJoin q6
                  ( leftJoin q5
                    ( leftJoin q4
                      ( leftJoin q3
                        ( leftJoin q2 q1
                             cond12
                        ) cond23
                      ) cond34
                    ) cond45
                  ) cond56
                ) cond67
              ) cond78


leftJoin9
  :: (Default Unpackspec fieldsL1 fieldsL1,
      Default Unpackspec fieldsL2 fieldsL2,
      Default Unpackspec nullableFieldsR1 nullableFieldsR1,
      Default Unpackspec fieldsL3 fieldsL3,
      Default Unpackspec nullableFieldsR2 nullableFieldsR2,
      Default Unpackspec fieldsL4 fieldsL4,
      Default Unpackspec nullableFieldsR3 nullableFieldsR3,
      Default Unpackspec fieldsL5 fieldsL5,
      Default Unpackspec nullableFieldsR4 nullableFieldsR4,
      Default Unpackspec fieldsL6 fieldsL6,
      Default Unpackspec nullableFieldsR5 nullableFieldsR5,
      Default Unpackspec fieldsL7 fieldsL7,
      Default Unpackspec nullableFieldsR6 nullableFieldsR6,
      Default Unpackspec fieldsL8 fieldsL8,
      Default Unpackspec nullableFieldsR7 nullableFieldsR7,
      Default Unpackspec fieldsR fieldsR,
      Default NullMaker fieldsR nullableFieldsR7,
      Default NullMaker (fieldsL2, nullableFieldsR1) nullableFieldsR8,
      Default NullMaker (fieldsL3, nullableFieldsR2) nullableFieldsR1,
      Default NullMaker (fieldsL4, nullableFieldsR3) nullableFieldsR2,
      Default NullMaker (fieldsL5, nullableFieldsR4) nullableFieldsR3,
      Default NullMaker (fieldsL6, nullableFieldsR5) nullableFieldsR4,
      Default NullMaker (fieldsL7, nullableFieldsR6) nullableFieldsR5,
      Default NullMaker (fieldsL8, nullableFieldsR7) nullableFieldsR6) =>
     Opaleye.Select fieldsR
     -> Opaleye.Select fieldsL8
     -> Opaleye.Select fieldsL7
     -> Opaleye.Select fieldsL6
     -> Opaleye.Select fieldsL5
     -> Opaleye.Select fieldsL4
     -> Opaleye.Select fieldsL3
     -> Opaleye.Select fieldsL2
     -> Opaleye.Select fieldsL1
     -> ((fieldsL8, fieldsR) -> Column PGBool)
     -> ((fieldsL7, (fieldsL8, nullableFieldsR7)) -> Column PGBool)
     -> ((fieldsL6, (fieldsL7, nullableFieldsR6)) -> Column PGBool)
     -> ((fieldsL5, (fieldsL6, nullableFieldsR5)) -> Column PGBool)
     -> ((fieldsL4, (fieldsL5, nullableFieldsR4)) -> Column PGBool)
     -> ((fieldsL3, (fieldsL4, nullableFieldsR3)) -> Column PGBool)
     -> ((fieldsL2, (fieldsL3, nullableFieldsR2)) -> Column PGBool)
     -> ((fieldsL1, (fieldsL2, nullableFieldsR1)) -> Column PGBool)
     -> Opaleye.Select (fieldsL1, nullableFieldsR8)
leftJoin9   q1 q2    q3     q4     q5     q6      q7    q8     q9
          cond12 cond23 cond34 cond45 cond56 cond67 cond78 cond89 =
  leftJoin q9 ( leftJoin q8
                ( leftJoin q7
                  ( leftJoin q6
                    ( leftJoin q5
                      ( leftJoin q4
                        ( leftJoin q3
                          ( leftJoin q2 q1
                              cond12
                          ) cond23
                        ) cond34
                      ) cond45
                    ) cond56
                  ) cond67
                ) cond78
              ) cond89

