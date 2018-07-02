{-|
Module      : Gargantext.Text.Ngrams.Lists
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.List
  where

import Data.Text (Text)
import qualified Data.Text as DT
import Gargantext.Prelude

-- | TODO normalize text

-- | TODO Order the seperators in probability of apparition
separators :: [Text]
separators = [" ", ",", ".", "?", "!", "\""]

isIn :: Text -> Text -> Bool
isIn term context = any (\x   -> DT.isInfixOf x context)
                        $ map (\sep -> term <> sep) separators

------------------------------------------------------------------------
--graph :: [Ngrams] -> [Ngrams]
--graph ngs = filter (\ng -> _ngramsListName ng == Just Graph) ngs
--
--candidates :: [Ngrams] -> [Ngrams]
--candidates ngs = filter (\ng -> _ngramsListName ng == Just Candidate) ngs
--
--stop :: [Ngrams] -> [Ngrams]
--stop ngs = filter (\ng -> _ngramsListName ng == Just Stop) ngs
------------------------------------------------------------------------
-- | Attoparsec solution to index test 
--import Data.Attoparsec.ByteString (Parser, parseOnly, try, string
--                                  , takeTill, take
--                                  , manyTill, many1)
--import Data.Attoparsec.ByteString.Char8 (anyChar, isEndOfLine)
--import Data.ByteString (ByteString, concat)
--import Data.ByteString.Char8 (pack)
--import Control.Applicative
-- | Attoparsec version
--indexParser :: (ByteString -> b) -> ByteString -> Parser b
--indexParser form2label x = do
--    _  <- manyTill anyChar (string x)
--    pure $ form2label x

--doIndex :: Applicative f => ByteString -> ByteString -> f (Either String [ByteString]
--doIndex f x txt = pure $ parseOnly (many $ indexParser f x) txt
------------------------------------------------------------------------


