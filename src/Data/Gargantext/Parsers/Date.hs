{-|
Module      : Data.Gargantext.Parsers.Date
Description : Some utils to parse dates
Copyright   : (c) CNRS 2017-present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

According to the language of the text, parseDate1 returns date as Text:

TODO : Add some tests
import Data.Gargantext.Parsers.Date as DGP
DGP.parseDate1 DGP.FR "12 avril 2010" == "2010-04-12T00:00:00.000+00:00"
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Gargantext.Parsers.Date (parseDate1, Lang(FR, EN), parseDate) where

import Data.Gargantext.Prelude
--import Data.Gargantext.Types.Main as G

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
import Data.HashMap.Strict as HM

import Data.Text (Text)
-- import Duckling.Engine (parseAndResolve)
-- import Duckling.Rules (rulesFor)
-- import Duckling.Debug as DB

import Duckling.Types (ResolvedToken)
import Safe (headMay)


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
    maybeJson <- pm jsonValue <$> parseDateWithDuckling lang text
    case headMay maybeJson of
      Just (Json.Object object) -> case HM.lookup "value" object of
                                     Just (Json.String date) -> pure date
                                     Just _                  -> error "ParseDate ERROR: should be a json String"
                                     Nothing                 -> error "ParseDate ERROR: no date found"
      _                         -> error "ParseDate ERROR: type error"



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





