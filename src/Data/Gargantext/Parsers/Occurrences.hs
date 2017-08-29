{-# LANGUAGE OverloadedStrings #-}

module Data.Gargantext.Parsers.Occurrences where

import Data.Attoparsec.Text
import Data.Text (Text)


import Data.Either.Extra(Either(..))
import qualified Data.Text as T
import Control.Applicative

occurrenceParser :: Text -> Parser Bool
occurrenceParser txt = manyTill anyChar (string txt) >> pure True

occurrencesParser :: Text -> Parser Int
occurrencesParser txt = case txt of
                    "" -> pure 0
                    _  -> many (occurrenceParser txt') >>= \matches -> pure (length matches)
    where
        txt' = T.toLower txt

parseOccurrences :: Text -> Text -> Either String Int
parseOccurrences x = parseOnly (occurrencesParser x)
