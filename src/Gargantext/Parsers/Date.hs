{-|
Module      : Gargantext.Parsers.Date
Description : Some utils to parse dates
Copyright   : (c) CNRS 2017-present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

According to the language of the text, parseDate1 returns date as Text:

TODO : Add some tests
import Gargantext.Parsers.Date as DGP
DGP.parseDate1 DGP.FR "12 avril 2010" == "2010-04-12T00:00:00.000+00:00"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Parsers.Date (parseDate1, Lang(FR, EN), parseDate, fromRFC3339) where

import Gargantext.Prelude
import Prelude (toInteger)
--import Gargantext.Types.Main as G

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime.TimeZone.Series (zonedTimeToZoneSeriesTime)
import Data.Time.LocalTime (utc)
import Duckling.Resolve (fromUTC, Context(Context, referenceTime, locale)
                        , DucklingTime(DucklingTime)
                        )
import Duckling.Core (makeLocale, Lang(FR,EN), Some(This), Dimension(Time))
import Duckling.Types (jsonValue, Entity)

import Duckling.Api (analyze, parse)
import qualified Data.HashSet as HashSet
import qualified Data.Aeson   as Json
import Data.Time (ZonedTime(..), LocalTime(..), TimeZone(..), TimeOfDay(..), getCurrentTimeZone)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Fixed (Fixed (MkFixed))
import Data.HashMap.Strict as HM hiding (map)

import Control.Monad ((=<<))
import Data.Either (Either)
import Data.String (String)
import Data.Text (Text, unpack)
-- import Duckling.Engine (parseAndResolve)
-- import Duckling.Rules (rulesFor)
-- import Duckling.Debug as DB

import Duckling.Types (ResolvedToken)
import Safe (headMay)
import System.IO.Unsafe (unsafePerformIO)

import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (Stream, ParsecT)
import qualified Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec (many1, noneOf, anyChar, char, oneOf)

import Text.XML.HXT.DOM.Util (decimalStringToInt)
-- TODO add Paris at Duckling.Locale Region datatype
-- | To get Homogeinity of the languages
--   TODO : put this in a more generic place in the source code
--parserLang :: G.Language -> Lang
--parserLang G.FR = FR
--parserLang G.EN = EN


-- | Final Date parser API
-- IO can be avoided here:
-- currentContext :: Lang -> IO Context
-- currentContext lang = localContext lang <$> utcToDucklingTime <$> getCurrentTime
-- parseDate1 :: Context -> Text -> SomeErrorHandling Text
parseDate1 :: Lang -> Text -> IO Text
parseDate1 lang text = do
    maybeJson <- map jsonValue <$> parseDateWithDuckling lang text
    case headMay maybeJson of
      Just (Json.Object object) -> case HM.lookup "value" object of
                                     Just (Json.String date) -> pure date
                                     Just _                  -> panic "ParseDate ERROR: should be a json String"
                                     Nothing                 -> panic "ParseDate ERROR: no date found"
      _                         -> panic "ParseDate ERROR: type error"



-- | Current Time in DucklingTime format
-- TODO : get local Time in a more generic way
utcToDucklingTime :: UTCTime -> DucklingTime
utcToDucklingTime time = DucklingTime . zonedTimeToZoneSeriesTime $ fromUTC time utc

-- | Local Context which depends on Lang and Time
localContext :: Lang -> DucklingTime -> Context
localContext lang dt = Context {referenceTime = dt, locale = makeLocale lang Nothing}

-- | Date parser with Duckling
parseDateWithDuckling :: Lang -> Text -> IO [ResolvedToken]
parseDateWithDuckling lang input = do
    contxt <- localContext lang <$> utcToDucklingTime <$> getCurrentTime
    --pure $ parseAndResolve (rulesFor (locale ctx) (HashSet.fromList [(This Time)])) input ctx
    pure $ analyze input contxt $ HashSet.fromList [(This Time)]


parseDate :: Lang -> Text -> IO [Entity]
parseDate lang input = do
    context <- localContext lang <$> utcToDucklingTime <$> getCurrentTime
    pure $ parse input context [(This Time)]

-- | Permit to transform a String to an Int in a monadic context
wrapDST :: Monad m => String -> m Int
wrapDST = (return . decimalStringToInt)

-- | Generic parser which take at least one element not given in argument
many1NoneOf :: Stream s m Char => [Char] -> ParsecT s u m [Char]
many1NoneOf = (many1 . noneOf)

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

-- | Parser for time format h:m:s
parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = do
        h <- wrapDST =<< many1NoneOf [':']
        _ <- char ':'
        m <- wrapDST =<< many1NoneOf [':']
        _ <- char ':'
        s <- wrapDST =<< many1NoneOf ['+', '-']
        return $ TimeOfDay h m (MkFixed $ toInteger s)

-- | Parser for timezone format +hh:mm
parseTimeZone :: Parser TimeZone
parseTimeZone = do
        sign <- oneOf ['+', '-']
        h <- wrapDST =<< many1NoneOf [':']
        _ <- char ':'
        m <- wrapDST =<< (many1 $ anyChar)
        let (TimeZone _ s n) = unsafePerformIO getCurrentTimeZone
        let timeInMinute = if sign == '+' then h * 60 + m else -h * 60 - m 
         in return $ TimeZone timeInMinute s n

-- | Parser which use parseGregorian, parseTimeOfDay and parseTimeZone to create a ZonedTime
parseZonedTime :: Parser ZonedTime
parseZonedTime= do
        d <- parseGregorian 
        tod <- parseTimeOfDay
        tz <- parseTimeZone
        return $ ZonedTime (LocalTime d (tod)) tz

-- | Opposite of toRFC3339
fromRFC3339 :: Text -> Either ParseError ZonedTime
fromRFC3339 t = Text.ParserCombinators.Parsec.parse parseZonedTime "ERROR: Couldn't parse zoned time." input
        where input = unpack t
