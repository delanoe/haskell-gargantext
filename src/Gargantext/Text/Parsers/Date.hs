{-|
Module      : Gargantext.Text.Parsers.Date
Description : Some utils to parse dates
Copyright   : (c) CNRS 2017-present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

According to the language of the text, parseDateRaw returns date as Text:

TODO : Add some tests
import Gargantext.Text.Parsers.Date as DGP
DGP.parseDateRaw DGP.FR "12 avril 2010" == "2010-04-12T00:00:00.000+00:00"
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Parsers.Date (parseDate, parseDateRaw, parseGregorian, wrapDST) where

import Data.HashMap.Strict as HM hiding (map)
import Data.Text (Text, unpack, splitOn, pack)
import Data.Time (parseTimeOrError, defaultTimeLocale)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (utc)
import Data.Time.LocalTime.TimeZone.Series (zonedTimeToZoneSeriesTime)
import Duckling.Api (analyze)
import Duckling.Core (makeLocale, Some(This), Dimension(Time))
import Duckling.Resolve (fromUTC, Context(Context, referenceTime, locale), DucklingTime(DucklingTime))
import Duckling.Types (ResolvedToken)
import Duckling.Types (jsonValue)
import Gargantext.Core (Lang(FR,EN))
import Gargantext.Prelude
import qualified Data.Aeson   as Json
import qualified Data.HashSet as HashSet
import qualified Duckling.Core as DC

-- | Unused import (to parse Date Format, keeping it for maybe next steps)
import Control.Monad ((=<<))
import Data.Either (Either)
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable (length)
import Data.String (String)
import Data.Time (ZonedTime(..), LocalTime(..), TimeZone(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)
import Duckling.Debug as DB
import Duckling.Engine (parseAndResolve)
import Duckling.Rules (rulesFor)
import Prelude (toInteger, div, otherwise, (++))
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Stream, ParsecT)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (many1, noneOf, anyChar, char, oneOf)
import Text.XML.HXT.DOM.Util (decimalStringToInt)
import qualified Text.ParserCombinators.Parsec (parse)

------------------------------------------------------------------------
-- | Date Parser
-- Parses dates mentions in full text given the language.
-- >>> parseDate FR (pack "10 avril 1979 à 19H")
-- 1979-04-10 19:00:00 UTC
-- >>> parseDate EN (pack "April 10 1979")
-- 1979-04-10 00:00:00 UTC
parseDate :: Lang -> Text -> IO UTCTime
parseDate lang s = parseDate' "%Y-%m-%dT%T" "0-0-0T0:0:0" lang s

type DateFormat = Text
type DateNull   = Text

parseDate' :: DateFormat -> DateNull -> Lang -> Text -> IO UTCTime
parseDate' format def lang s = do
  dateStr' <- parseDateRaw lang s
  let dateStr = unpack $ maybe def identity
                       $ head $ splitOn "." dateStr'
  pure $ parseTimeOrError True defaultTimeLocale (unpack format) dateStr


-- TODO add Paris at Duckling.Locale Region datatype
-- | To get Homogeinity of the languages
--   TODO : put this in a more generic place in the source code
parserLang :: Lang -> DC.Lang
parserLang FR = DC.FR
parserLang EN = DC.EN
-- parserLang _  = panic "not implemented"

-- | Final Date parser API
-- IO can be avoided here:
-- currentContext :: Lang -> IO Context
-- currentContext lang = localContext lang <$> utcToDucklingTime <$> getCurrentTime
-- parseDateRaw :: Context -> Text -> SomeErrorHandling Text

-- TODO error handling
parseDateRaw :: Lang -> Text -> IO (Text)
parseDateRaw lang text = do
    maybeJson <- map jsonValue <$> parseDateWithDuckling lang text
    case headMay maybeJson of
      Just (Json.Object object) -> case HM.lookup "value" object of
                                     Just (Json.String date) -> pure date
                                     Just _                  -> panic "ParseDateRaw ERROR: should be a json String"
                                     Nothing                 -> panic $ "ParseDateRaw ERROR: no date found" <> (pack . show) lang <> " " <> text

      _                         -> panic $ "ParseDateRaw ERROR: type error" <> (pack . show) lang <> " " <> text


-- | Current Time in DucklingTime format
-- TODO : get local Time in a more generic way
utcToDucklingTime :: UTCTime -> DucklingTime
utcToDucklingTime time = DucklingTime . zonedTimeToZoneSeriesTime $ fromUTC time utc

-- | Local Context which depends on Lang and Time
localContext :: Lang -> DucklingTime -> Context
localContext lang dt = Context {referenceTime = dt, locale = makeLocale (parserLang lang) Nothing}

-- | Date parser with Duckling
parseDateWithDuckling :: Lang -> Text -> IO [ResolvedToken]
parseDateWithDuckling lang input = do
    contxt <- localContext lang <$> utcToDucklingTime <$> getCurrentTime
    --pure $ parseAndResolve (rulesFor (locale ctx) (HashSet.fromList [(This Time)])) input ctx
    pure $ analyze input contxt $ HashSet.fromList [(This Time)]

-- | Permit to transform a String to an Int in a monadic context
wrapDST :: Monad m => String -> m Int
wrapDST = return . decimalStringToInt

-- | Generic parser which take at least one element not given in argument
many1NoneOf :: Stream s m Char => [Char] -> ParsecT s u m [Char]
many1NoneOf = (many1 . noneOf)

--getMultiplicator :: Int -> Int
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
