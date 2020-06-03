{-|
Module      : Gargantext.Prelude.Utils
Description : Useful Tools near Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Nice optimization of the Fibonacci function.

Source:
Gabriel Gonzales, Blazing fast Fibonacci numbers using Monoids, 2020-04,
http://www.haskellforall.com/2020/04/blazing-fast-fibonacci-numbers-using.html
(This post illustrates a nifty application of Haskell’s standard library to solve a numeric problem.)

TODO: quikcheck

-}



module Gargantext.Prelude.Fibonacci where

import Protolude

import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup

-------------------------------------------------------------
fib' :: Integer -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = fib (n-1) + fib (n-2)
-------------------------------------------------------------


data Matrix2x2 = Matrix
    { x00 :: Integer, x01 :: Integer
    , x10 :: Integer, x11 :: Integer
    }

instance Monoid.Monoid Matrix2x2 where
    mempty =
        Matrix
            { x00 = 1, x01 = 0
            , x10 = 0, x11 = 1
            }

instance Semigroup.Semigroup Matrix2x2 where
    Matrix l00 l01 l10 l11 <> Matrix r00 r01 r10 r11 =
        Matrix
            { x00 = l00 * r00 + l01 * r10, x01 = l00 * r01 + l01 * r11
            , x10 = l10 * r00 + l11 * r10, x11 = l10 * r01 + l11 * r11
            }

fib :: Integer -> Integer
fib n = x01 (Semigroup.mtimesDefault n matrix)
  where
    matrix =
        Matrix
            { x00 = 0, x01 = 1
            , x10 = 1, x11 = 1
            }
