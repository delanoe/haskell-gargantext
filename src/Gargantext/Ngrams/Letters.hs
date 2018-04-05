{-|
Module      : Gargantext.Ngrams.Letters
Description : Ngrams.Letters module
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Sugar to work on letters with Text.

-}

{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Ngrams.Letters where

import qualified Data.Text.Lazy as DTL
-- import qualified Data.Text.Lazy.IO as DTLIO
import Gargantext.Prelude


-- | /O(n)/ Breaks a 'Text' up into each Text list of chars.
-- from slower to faster:
letters :: DTL.Text -> [DTL.Text]
letters text = DTL.chunksOf 1 text

letters' :: DTL.Text -> [DTL.Text]
letters' text = DTL.splitOn "#" $ DTL.intersperse '#' text

letters'' :: DTL.Text -> [DTL.Text]
letters'' = DTL.foldr (\ch xs -> DTL.singleton ch : xs) []


