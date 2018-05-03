{-|
Module      : Gargantext.Text.Occurrences
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

module Gargantext.Text.Occurrences where

import Gargantext.Prelude

import Control.Monad ((>>),(>>=))
import Data.String (String())
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
