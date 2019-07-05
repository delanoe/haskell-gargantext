{-|
Module      : Gargantext.Text.Corpus.Parsers.Date
Description : Some utils to parse dates
Copyright   : (c) CNRS 2017-present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Corpus.Parsers.Date.Parsec
  where

import Control.Monad ((=<<))
import Data.Either (Either)
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable (length)
import Data.String (String)
import Data.Text (Text, unpack)
import Data.Time (ZonedTime(..), LocalTime(..), TimeZone(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)
import Gargantext.Prelude
import Prelude (toInteger, div, otherwise, (++))
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Stream, ParsecT)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (many1, noneOf, anyChar, char, oneOf)
import Text.XML.HXT.DOM.Util (decimalStringToInt)
import qualified Text.ParserCombinators.Parsec (parse)

-- | Permit to transform a String to an Int in a monadic context
wrapDST :: Monad m => String -> m Int
wrapDST = return . decimalStringToInt

-- | Generic parser which take at least one element not given in argument
many1NoneOf :: Stream s m Char => [Char] -> ParsecT s u m [Char]
many1NoneOf = (many1 . noneOf)

getMultiplicator :: Int -> Int
getMultiplicator a
  | 0 >= a = 1
  | otherwise = 10 * (getMultiplicator $ div a 10)

-- | Parser for date format y-m-d
parseGregorian :: Parser Day
parseGregorian  = do
        y <- wrapDST =<< many1NoneOf ['-']
        _ <- char '-'
        m <- wrapDST =<< many1NoneOf ['-']
        _ <- char '-'
        d <- wrapDST =<< many1NoneOf ['T']
        _ <- char 'T'
        return $ fromGregorian (toInteger y) m d

---- | Parser for time format h:m:s
parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = do
        h <- wrapDST =<< many1NoneOf [':']
        _ <- char ':'
        m <- wrapDST =<< many1NoneOf [':']
        _ <- char ':'
        r <- many1NoneOf ['.']
        _ <- char '.'
        dec <- many1NoneOf ['+', '-']
        let (nb, l) = (decimalStringToInt $ r ++ dec, length dec)
            seconds = nb * 10^(12-l)
        return $ TimeOfDay h m (MkFixed . toInteger $ seconds)


-- | Parser for timezone format +hh:mm
parseTimeZone :: Parser TimeZone
parseTimeZone = do
        sign <- oneOf ['+', '-']
        h <- wrapDST =<< many1NoneOf [':']
        _ <- char ':'
        m <- wrapDST =<< (many1 $ anyChar)
        let timeInMinute = if sign == '+' then h * 60 + m else -h * 60 - m
         in return $ TimeZone timeInMinute False "CET"

---- | Parser which use parseGregorian, parseTimeOfDay and parseTimeZone to create a ZonedTime
parseZonedTime :: Parser ZonedTime
parseZonedTime= do
        d <- parseGregorian
        tod <- parseTimeOfDay
        tz <- parseTimeZone
        return $ ZonedTime (LocalTime d (tod)) tz

---- | Opposite of toRFC3339
fromRFC3339 :: Text -> Either ParseError ZonedTime
fromRFC3339 t = Text.ParserCombinators.Parsec.parse parseZonedTime "ERROR: Couldn't parse zoned time." input
        where input = unpack t
