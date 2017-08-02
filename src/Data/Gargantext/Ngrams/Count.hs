{-# LANGUAGE OverloadedStrings #-}

module Data.Gargantext.Ngrams.Count where

import System.Environment (getArgs)

import Data.List (foldl', take)
import Data.Foldable as F

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Text.Lazy.IO as DTLIO
import qualified Data.Text.Lazy as DTL

-- | /O(n)/ Breaks a 'Text' up into each Text list of chars.
-- from slower to faster:
letters :: DTL.Text -> [DTL.Text]
letters text = DTL.chunksOf 1 text

letters' :: DTL.Text -> [DTL.Text]
letters' text = DTL.splitOn "#" $ DTL.intersperse '#' text

letters'' :: DTL.Text -> [DTL.Text]
letters'' = DTL.foldr (\ch xs -> DTL.singleton ch : xs) []


-- words
-- lines
-- words between punctuation
-- number of punctuation

occurrences :: Ord a => [a] -> Map a Int
occurrences xs = foldl' (\x y -> M.insertWith' (+) y 1 x) M.empty xs

-- for optimization :
--occurrences' :: Ord a => [a] -> Map a Integer
--occurrences' xs = DTL.foldl (\x y -> M.insertWith' (+) y 1 x) M.empty xs


countMain = do
  (fichier:rest) <- getArgs
  c <- DTLIO.readFile fichier
  --print $ occurrences $ DTL.chunksOf 1 c
  print $ occurrences $ letters'' c
  --print $ occurrences $ DTL.words $ DTL.toLower c

