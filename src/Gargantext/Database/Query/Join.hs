{-|
Module      : Gargantext.Database.Query.Join
Description : Main Join queries (using Opaleye)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Multiple Join functions with Opaleye.

-}

{-# OPTIONS_GHC -fno-warn-orphans        #-}


{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

------------------------------------------------------------------------

module Gargantext.Database.Query.Join ( leftJoin2
                                      , leftJoin3
                                      , leftJoin3'
                                      , leftJoin4
                                      , leftJoin4'
                                      , leftJoin5
                                      , leftJoin6
                                      , leftJoin7
                                      , leftJoin8
                                      , leftJoin9
                                      )
  where

import Control.Arrow ((>>>), returnA)
import Data.Profunctor.Product.Default
import Gargantext.Prelude
import Opaleye hiding (keepWhen)
import Opaleye.Internal.Join (NullMaker(..))
import qualified Opaleye.Internal.Unpackspec()


keepWhen :: (a -> Field SqlBool) -> SelectArr a a
keepWhen p = proc a -> do
  restrict  -< p a
  returnA -< a

------------------------------------------------------------------------
leftJoin2 :: (Default Unpackspec fieldsL fieldsL,
              Default Unpackspec fieldsR fieldsR,
              Default NullMaker fieldsR nullableFieldsR) =>
             Select fieldsL
             -> Select fieldsR
             -> ((fieldsL, fieldsR) -> Column SqlBool)
             -> Select (fieldsL, nullableFieldsR)
leftJoin2 = leftJoin

------------------------------------------------------------------------
-- | LeftJoin3 in two ways to write it
leftJoin3 :: Select columnsA -> Select columnsB -> Select columnsC
      -> ((columnsA, columnsB, columnsC) -> Column SqlBool)
      -> Select (columnsA, columnsB, columnsC)
leftJoin3 q1 q2 q3 cond = ((,,) <$> q1 <*> q2 <*> q3) >>> keepWhen cond


leftJoin3' :: (Default Unpackspec b2 b2, Default Unpackspec b3 b3,
               Default Unpackspec fieldsL fieldsL,
               Default Unpackspec fieldsR fieldsR, Default NullMaker b3 b4,
               Default NullMaker b2 b5, Default NullMaker fieldsR b2) =>
              Select fieldsL
              -> Select b3
              -> Select fieldsR
              -> ((fieldsL, (b3, b2)) -> Column SqlBool)
              -> ((b3, fieldsR) -> Column SqlBool)
              -> Select (fieldsL, (b4, b5))
leftJoin3' q1 q2 q3 cond12 cond23 = leftJoin q1 (leftJoin q2 q3 cond23) cond12

leftJoin4' :: Select columnsA
           -> Select columnsB
           -> Select columnsC
           -> Select columnsD
      -> ((columnsA, columnsB, columnsC, columnsD) -> Column SqlBool)
      -> Select (columnsA, columnsB, columnsC, columnsD)
leftJoin4' q1 q2 q3 q4 cond = ((,,,) <$> q1 <*> q2 <*> q3 <*> q4) >>> keepWhen cond


leftJoin4 :: (Default Unpackspec b2 b2,
              Default Unpackspec fieldsL fieldsL, Default Unpackspec b3 b3,
              Default Unpackspec b4 b4, Default Unpackspec b5 b5,
              Default Unpackspec b6 b6, Default Unpackspec fieldsR fieldsR,
              Default NullMaker b2 b7, Default NullMaker b5 b8,
              Default NullMaker b6 b9, Default NullMaker b3 b5,
              Default NullMaker b4 b6, Default NullMaker fieldsR b4) =>
             Select fieldsR
             -> Select b3
             -> Select b2
             -> Select fieldsL
             -> ((b3, fieldsR) -> Column SqlBool)
             -> ((b2, (b3, b4)) -> Column SqlBool)
             -> ((fieldsL, (b2, (b5, b6))) -> Column SqlBool)
             -> Select (fieldsL, (b7, (b8, b9)))
leftJoin4 q1 q2     q3     q4
         cond12 cond23 cond34 =
  leftJoin q4 ( leftJoin q3
                ( leftJoin q2 q1
                    cond12
                ) cond23
              ) cond34


leftJoin5 :: ( Default Unpackspec b2 b2, Default Unpackspec b3 b3
             , Default Unpackspec b4 b4, Default Unpackspec b5 b5
             , Default Unpackspec b6 b6, Default Unpackspec b7 b7
             , Default Unpackspec fieldsL fieldsL, Default Unpackspec b8 b8
             , Default Unpackspec b9 b9, Default Unpackspec b10 b10
             , Default Unpackspec fieldsR fieldsR, Default NullMaker b7 b6
             , Default NullMaker b6 b11, Default NullMaker b8 b12
             , Default NullMaker b3 b13, Default NullMaker b2 b14
             , Default NullMaker b9 b3, Default NullMaker b10 b2
             , Default NullMaker b5 b9, Default NullMaker b4 b10
             , Default NullMaker fieldsR b4) =>
           Select fieldsR
           -> Select b5
           -> Select b7
           -> Select b8
           -> Select fieldsL
           -> ((b5, fieldsR) -> Column SqlBool)
           -> ((b7, (b5, b4)) -> Column SqlBool)
           -> ((b8, (b7, (b9, b10))) -> Column SqlBool)
           -> ((fieldsL, (b8, (b6, (b3, b2)))) -> Column SqlBool)
           -> Select (fieldsL, (b12, (b11, (b13, b14))))
leftJoin5 q1 q2     q3     q4     q5
         cond12 cond23 cond34 cond45 =
  leftJoin q5 ( leftJoin q4
                ( leftJoin q3
                  ( leftJoin q2 q1
                       cond12
                   ) cond23
                 ) cond34
               ) cond45


leftJoin6 :: (Default Unpackspec b2 b2, Default Unpackspec b3 b3,
              Default Unpackspec b4 b4, Default Unpackspec b5 b5,
              Default Unpackspec fieldsL fieldsL, Default Unpackspec b6 b6,
              Default Unpackspec b7 b7, Default Unpackspec b8 b8,
              Default Unpackspec b9 b9, Default Unpackspec b10 b10,
              Default Unpackspec b11 b11, Default Unpackspec b12 b12,
              Default Unpackspec b13 b13, Default Unpackspec b14 b14,
              Default Unpackspec b15 b15, Default Unpackspec fieldsR fieldsR,
              Default NullMaker b5 b4, Default NullMaker b4 b16,
              Default NullMaker b6 b17, Default NullMaker b2 b18,
              Default NullMaker b7 b2, Default NullMaker b3 b7,
              Default NullMaker b12 b19, Default NullMaker b13 b20,
              Default NullMaker b10 b12, Default NullMaker b11 b13,
              Default NullMaker b14 b10, Default NullMaker b15 b11,
              Default NullMaker b8 b14, Default NullMaker b9 b15,
              Default NullMaker fieldsR b9) =>
             Select fieldsR
             -> Select b8
             -> Select b3
             -> Select b5
             -> Select b6
             -> Select fieldsL
             -> ((b8, fieldsR) -> Column SqlBool)
             -> ((b3, (b8, b9)) -> Column SqlBool)
             -> ((b5, (b3, (b14, b15))) -> Column SqlBool)
             -> ((b6, (b5, (b7, (b10, b11)))) -> Column SqlBool)
             -> ((fieldsL, (b6, (b4, (b2, (b12, b13))))) -> Column SqlBool)
             -> Select (fieldsL, (b17, (b16, (b18, (b19, b20)))))
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


leftJoin7 :: (Default Unpackspec b2 b2, Default Unpackspec b3 b3,
              Default Unpackspec b4 b4, Default Unpackspec b5 b5,
              Default Unpackspec b6 b6, Default Unpackspec b7 b7,
              Default Unpackspec b8 b8, Default Unpackspec b9 b9,
              Default Unpackspec b10 b10, Default Unpackspec b11 b11,
              Default Unpackspec b12 b12, Default Unpackspec b13 b13,
              Default Unpackspec fieldsL fieldsL, Default Unpackspec b14 b14,
              Default Unpackspec b15 b15, Default Unpackspec b16 b16,
              Default Unpackspec b17 b17, Default Unpackspec b18 b18,
              Default Unpackspec b19 b19, Default Unpackspec b20 b20,
              Default Unpackspec b21 b21, Default Unpackspec fieldsR fieldsR,
              Default NullMaker b11 b8, Default NullMaker b8 b10,
              Default NullMaker b10 b9, Default NullMaker b9 b22,
              Default NullMaker b16 b12, Default NullMaker b12 b17,
              Default NullMaker b17 b23, Default NullMaker b13 b24,
              Default NullMaker b15 b25, Default NullMaker b14 b15,
              Default NullMaker b3 b26, Default NullMaker b2 b27,
              Default NullMaker b18 b3, Default NullMaker b19 b2,
              Default NullMaker b5 b18, Default NullMaker b4 b19,
              Default NullMaker b20 b5, Default NullMaker b21 b4,
              Default NullMaker b7 b20, Default NullMaker b6 b21,
              Default NullMaker fieldsR b6) =>
             Select fieldsR
             -> Select b7
             -> Select b11
             -> Select b16
             -> Select b14
             -> Select b13
             -> Select fieldsL
             -> ((b7, fieldsR) -> Column SqlBool)
             -> ((b11, (b7, b6)) -> Column SqlBool)
             -> ((b16, (b11, (b20, b21))) -> Column SqlBool)
             -> ((b14, (b16, (b8, (b5, b4)))) -> Column SqlBool)
             -> ((b13, (b14, (b12, (b10, (b18, b19))))) -> Column SqlBool)
             -> ((fieldsL, (b13, (b15, (b17, (b9, (b3, b2))))))
                 -> Column SqlBool)
             -> Select (fieldsL, (b24, (b25, (b23, (b22, (b26, b27))))))
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


leftJoin8 :: (Default Unpackspec b2 b2, Default Unpackspec b3 b3,
              Default Unpackspec b4 b4, Default Unpackspec b5 b5,
              Default Unpackspec b6 b6, Default Unpackspec b7 b7,
              Default Unpackspec b8 b8, Default Unpackspec b9 b9,
              Default Unpackspec b10 b10, Default Unpackspec fieldsL fieldsL,
              Default Unpackspec b11 b11, Default Unpackspec b12 b12,
              Default Unpackspec b13 b13, Default Unpackspec b14 b14,
              Default Unpackspec b15 b15, Default Unpackspec b16 b16,
              Default Unpackspec b17 b17, Default Unpackspec b18 b18,
              Default Unpackspec b19 b19, Default Unpackspec b20 b20,
              Default Unpackspec b21 b21, Default Unpackspec b22 b22,
              Default Unpackspec b23 b23, Default Unpackspec b24 b24,
              Default Unpackspec b25 b25, Default Unpackspec b26 b26,
              Default Unpackspec b27 b27, Default Unpackspec b28 b28,
              Default Unpackspec fieldsR fieldsR, Default NullMaker b8 b5,
              Default NullMaker b5 b7, Default NullMaker b7 b6,
              Default NullMaker b6 b29, Default NullMaker b13 b9,
              Default NullMaker b9 b14, Default NullMaker b14 b30,
              Default NullMaker b10 b31, Default NullMaker b12 b32,
              Default NullMaker b11 b12, Default NullMaker b2 b33,
              Default NullMaker b15 b2, Default NullMaker b3 b15,
              Default NullMaker b16 b3, Default NullMaker b4 b16,
              Default NullMaker b23 b34, Default NullMaker b24 b35,
              Default NullMaker b21 b23, Default NullMaker b22 b24,
              Default NullMaker b25 b21, Default NullMaker b26 b22,
              Default NullMaker b19 b25, Default NullMaker b20 b26,
              Default NullMaker b27 b19, Default NullMaker b28 b20,
              Default NullMaker b17 b27, Default NullMaker b18 b28,
              Default NullMaker fieldsR b18) =>
             Select fieldsR
             -> Select b17
             -> Select b4
             -> Select b8
             -> Select b13
             -> Select b11
             -> Select b10
             -> Select fieldsL
             -> ((b17, fieldsR) -> Column SqlBool)
             -> ((b4, (b17, b18)) -> Column SqlBool)
             -> ((b8, (b4, (b27, b28))) -> Column SqlBool)
             -> ((b13, (b8, (b16, (b19, b20)))) -> Column SqlBool)
             -> ((b11, (b13, (b5, (b3, (b25, b26))))) -> Column SqlBool)
             -> ((b10, (b11, (b9, (b7, (b15, (b21, b22)))))) -> Column SqlBool)
             -> ((fieldsL, (b10, (b12, (b14, (b6, (b2, (b23, b24)))))))
                 -> Column SqlBool)
             -> Select (fieldsL, (b31, (b32, (b30, (b29, (b33, (b34, b35)))))))
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



leftJoin9 :: (Default Unpackspec b2 b2, Default Unpackspec b3 b3,
            Default Unpackspec b4 b4, Default Unpackspec b5 b5,
            Default Unpackspec b6 b6, Default Unpackspec b7 b7,
            Default Unpackspec b8 b8, Default Unpackspec b9 b9,
            Default Unpackspec b10 b10, Default Unpackspec b11 b11,
            Default Unpackspec b12 b12, Default Unpackspec b13 b13,
            Default Unpackspec b14 b14, Default Unpackspec b15 b15,
            Default Unpackspec b16 b16, Default Unpackspec b17 b17,
            Default Unpackspec b18 b18, Default Unpackspec b19 b19,
            Default Unpackspec b20 b20, Default Unpackspec b21 b21,
            Default Unpackspec fieldsL fieldsL, Default Unpackspec b22 b22,
            Default Unpackspec b23 b23, Default Unpackspec b24 b24,
            Default Unpackspec b25 b25, Default Unpackspec b26 b26,
            Default Unpackspec b27 b27, Default Unpackspec b28 b28,
            Default Unpackspec b29 b29, Default Unpackspec b30 b30,
            Default Unpackspec b31 b31, Default Unpackspec b32 b32,
            Default Unpackspec b33 b33, Default Unpackspec b34 b34,
            Default Unpackspec b35 b35, Default Unpackspec b36 b36,
            Default Unpackspec fieldsR fieldsR, Default NullMaker b15 b10,
            Default NullMaker b10 b14, Default NullMaker b14 b11,
            Default NullMaker b11 b13, Default NullMaker b13 b12,
            Default NullMaker b12 b37, Default NullMaker b28 b16,
            Default NullMaker b16 b29, Default NullMaker b29 b17,
            Default NullMaker b17 b30, Default NullMaker b30 b38,
            Default NullMaker b21 b20, Default NullMaker b20 b39,
            Default NullMaker b22 b40, Default NullMaker b18 b41,
            Default NullMaker b23 b18, Default NullMaker b19 b23,
            Default NullMaker b26 b42, Default NullMaker b25 b26,
            Default NullMaker b27 b25, Default NullMaker b24 b27,
            Default NullMaker b3 b43, Default NullMaker b2 b44,
            Default NullMaker b31 b3, Default NullMaker b32 b2,
            Default NullMaker b5 b31, Default NullMaker b4 b32,
            Default NullMaker b33 b5, Default NullMaker b34 b4,
            Default NullMaker b7 b33, Default NullMaker b6 b34,
            Default NullMaker b35 b7, Default NullMaker b36 b6,
            Default NullMaker b9 b35, Default NullMaker b8 b36,
            Default NullMaker fieldsR b8) =>
           Select fieldsR
           -> Select b9
           -> Select b15
           -> Select b28
           -> Select b24
           -> Select b19
           -> Select b21
           -> Select b22
           -> Select fieldsL
           -> ((b9, fieldsR) -> Column SqlBool)
           -> ((b15, (b9, b8)) -> Column SqlBool)
           -> ((b28, (b15, (b35, b36))) -> Column SqlBool)
           -> ((b24, (b28, (b10, (b7, b6)))) -> Column SqlBool)
           -> ((b19, (b24, (b16, (b14, (b33, b34))))) -> Column SqlBool)
           -> ((b21, (b19, (b27, (b29, (b11, (b5, b4)))))) -> Column SqlBool)
           -> ((b22, (b21, (b23, (b25, (b17, (b13, (b31, b32)))))))
               -> Column SqlBool)
           -> ((fieldsL, (b22, (b20, (b18, (b26, (b30, (b12, (b3, b2))))))))
               -> Column SqlBool)
           -> Select
                (fieldsL, (b40, (b39, (b41, (b42, (b38, (b37, (b43, b44))))))))
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
