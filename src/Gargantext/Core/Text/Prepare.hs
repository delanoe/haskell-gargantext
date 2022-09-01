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

import Data.Text (Text)
import Gargantext.Core.Text (sentences)
import Gargantext.Prelude
import qualified Data.List as List
import qualified Data.Text as Text


---------------------------------------------------------------------
prepareText :: Paragraph -> Text -> [Text]
prepareText p txt = groupText p
                  $ List.filter (/= "")
                  $ toParagraphs
                  $ Text.lines
                  $ Text.replace "_" " "  -- some texts seem to be underlined
                  $ Text.replace "--" ""  -- removing bullets like of dialogs
                  $ Text.replace "\xd" "" txt

---------------------------------------------------------------------

groupText :: Paragraph -> [Text] -> [Text]
groupText (Uniform g s) = groupUniform g s
groupText AuthorLike    = groupLines

---------------------------------------------------------------------
data Paragraph = Uniform Grain Step | AuthorLike
-- Uniform does not preserve the paragraphs of the author but length of paragraphs is uniform
-- Author Like preserve the paragraphs of the Author but length of paragraphs is not uniform

-- Grain: number of Sentences by block of Text
-- Step : overlap of sentence between connex block of Text
groupUniform :: Grain -> Step -> [Text] -> [Text]
groupUniform g s ts = map (Text.intercalate " ")
                    $ chunkAlong g s
                    $ sentences
                    $ Text.concat ts

groupLines :: [Text] -> [Text]
groupLines xxx@(a:b:xs) = 
  if Text.length a > moyenne
     then [a] <> (groupLines (b:xs))
     else let ab = a <> " " <> b in
              if Text.length ab > moyenne
                then [ab] <> (groupLines xs)
                else groupLines ([ab] <> xs)
  where
    moyenne = round
            $ mean
            $ (map (fromIntegral . Text.length) xxx :: [Double])
groupLines [a] = [a]
groupLines [] = []

groupLines_test :: [Text]
groupLines_test = groupLines theData
  where
    theData = ["abxxxx", "bc", "cxxx", "d"]

---------------------------------------------------------------------
toParagraphs :: [Text] -> [Text]
toParagraphs (a:x:xs) =
  if a == ""
     then [a] <> toParagraphs (x:xs)
     else if x == ""
            then [a] <> toParagraphs (x:xs)
            else toParagraphs $ [a <> " " <> x ] <> xs
toParagraphs [a] = [a]
toParagraphs [] = []

-- Tests

-- TODO for internships: Property tests
toParagraphs_test :: Bool
toParagraphs_test = 
  toParagraphs ["a","b","","c","d","d","","e","f","","g","h",""]
     == [ "a b", "", "c d d", "", "e f", "", "g h", ""]





