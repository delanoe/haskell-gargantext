{-|
Module      : Gargantext.Text.Terms.Mono
Description : Mono Terms module
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Mono-terms are Nterms where n == 1.

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Text.Terms.Mono (monoterms, monoterms')
  where

import Data.Text (Text, toLower, split, splitOn, pack)
import qualified Data.Set as S

import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Text.Terms.Mono.Stem (stem)

import Gargantext.Prelude
import Data.Char (isAlphaNum, isSpace)

monoterms' :: Lang -> Text -> [Terms]
monoterms' l txt = map (text2terms l) $ monoterms txt

monoterms :: Text -> [Text]
monoterms txt = map toLower $ split isWord txt
  where
    isWord c = c `elem` [' ', '\'', ',', ';']

text2terms :: Lang -> Text -> Terms
text2terms lang txt = Terms label stems
  where
    label = splitOn (pack " ") txt
    stems = S.fromList $ map (stem lang) label

  --monograms :: Text -> [Text]
--monograms xs = monograms $ toLower $ filter isGram xs

isGram :: Char -> Bool
isGram  c  = isAlphaNum c || isSpace c || c `elem` ['-','/','\'']

