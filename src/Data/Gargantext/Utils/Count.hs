{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes                #-}

module Data.Gargantext.Utils.Count (head, last, all, any, sum, product, length)
    where

import Data.Monoid
import Protolude hiding ((<>), head, last, all, any, sum, product, length)

import qualified Data.Foldable
import Control.Lens (Getting, foldMapOf)

data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)

instance Functor (Fold i) where
    fmap k (Fold tally summarize) = Fold tally (k . summarize)

instance Applicative (Fold i) where
    pure o = Fold (\_ -> ()) (\_ -> o)

    Fold tallyF summarizeF <*> Fold tallyX summarizeX = Fold tally summarize
        where
            tally i = (tallyF i, tallyX i)
            summarize (nF, nX) = summarizeF nF (summarizeX nX)

focus :: (forall m . Monoid m => Getting m b a) -> Fold a o -> Fold b o
focus lens (Fold tally summarize) = Fold (foldMapOf lens tally) summarize


fold :: Fold i o -> [i] -> o
fold (Fold tally summarize) is = summarize (reduce (map tally is))
    where
        reduce = Data.Foldable.foldl' (<>) mempty

--
head :: Fold a (Maybe a)
head = Fold (First . Just) getFirst

last :: Fold a (Maybe a)
last = Fold (Last . Just) getLast
--
all :: (a -> Bool) -> Fold a Bool
all predicate = Fold (All . predicate) getAll

any :: (a -> Bool) -> Fold a Bool
any predicate = Fold (Any . predicate) getAny
--
sum :: Num n => Fold n n
sum = Fold Sum getSum

product :: Num n => Fold n n
product = Fold Product getProduct

length :: Num n => Fold i n
length = Fold (\_ -> Sum 1) getSum


-- | Average function optimized (/!\ need to test it)
data Average a = Average { numerator :: !a, denominator :: !Int }

instance Num a => Monoid (Average a) where
    mempty = Average 0 0
    mappend (Average xL nL) (Average xR nR) = Average (xL + xR) (nL + nR)

average :: Fractional a => Fold a a
average = Fold tally summarize
    where
        tally x = Average x 1
        summarize (Average numerator denominator) =
            numerator / fromIntegral denominator
