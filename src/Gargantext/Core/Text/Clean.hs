{-|
Module      : Gargantext.Core.Text.Clean
Description : Tools to clean text
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Clean some texts before importing it.

For a given Language, chose a big master piece of litteracy to analyze
it with GarganText. Here is a an example with a famous French Writer
that could be the incarnation of the mythic Gargantua.

-}

{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Core.Text.Clean
  where

import Gargantext.Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

groupLines :: [Text] -> [Text]
groupLines (a:x:xs) = undefined

cleanText :: Text -> [Text]
cleanText txt = List.filter (/= "")
              $ toParagraphs
              $ Text.lines
              $ Text.replace "--" ""  -- removing bullets like of dialogs
              $ Text.replace "\xd" "" txt


toParagraphs :: [Text] -> [Text]
toParagraphs (a:x:xs) =
  if a == ""
     then [a] <> toParagraphs (x:xs)
     else if x == ""
            then [a] <> toParagraphs (x:xs)
            else toParagraphs $ [a <> " " <> x ] <> xs
toParagraphs [a] = [a]
toParagraphs [] = []


