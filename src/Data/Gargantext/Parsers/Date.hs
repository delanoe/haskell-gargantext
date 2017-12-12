{-|
Module      : Data.Gargantext.Parsers.Date
Description : Some utils to parse dates
Copyright   : (c) CNRS 2017
License     : AGPL + CECILL v3
Maintainer  : alexandre.delanoe@iscpif.fr
Stability   : experimental
Portability : POSIX

According to the language of the text, parseDate1 returns date as Text:

TODO : Add some tests
import Data.Gargantext.Parsers as DGP
DGP.parseDate1 DGP.FR "12 avril 2010" == "2010-04-12T00:00:00.000+00:00"

-}

module Data.Gargantext.Parsers.Date (parseDate1, Lang(FR, EN)) where

import Data.Gargantext.Prelude
import qualified Data.Gargantext.Types.Main as G

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime.TimeZone.Series (zonedTimeToZoneSeriesTime)
import Data.Time.LocalTime (utc)
import Duckling.Resolve (fromUTC, Context(Context, referenceTime, locale)
                        , DucklingTime(DucklingTime)
                        )
import Duckling.Core (makeLocale, Lang(FR,EN), Some(This), Dimension(Time))
import Duckling.Types (jsonValue)
--import qualified Duckling.Core as DC

import Duckling.Api (analyze)
import qualified Data.HashSet as HashSet
import qualified Data.Aeson   as Json
import Data.HashMap.Strict as HM

import Data.Text (Text)
-- import Duckling.Engine (parseAndResolve)
-- import Duckling.Rules (rulesFor)
-- import Duckling.Debug as DB

import Safe (headMay)

import Duckling.Types (ResolvedToken)



-- TODO add Paris at Duckling.Locale Region datatype
-- | To get Homogeinity of the languages
--   TODO : put this in a more generic place in the source code
parserLang :: G.Language -> Lang
parserLang G.FR = FR
parserLang G.EN = EN



-- | Final Date parser API
parseDate1 :: Lang -> Text -> IO Text
parseDate1 lang text = do
    maybeJson <- pm jsonValue <$> parseDateWithDuckling lang text
    case headMay maybeJson of
      Just (Json.Object object) -> case HM.lookup "value" object of
                                     Just (Json.String date) -> pure date
                                     Just _             -> error "ERROR: should be a json String"
                                     Nothing            -> error "No date found"
      Just _                    -> error "ERROR: should be a json Object"
      Nothing                   -> pure "No date found"



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
    ctx <- localContext lang <$> utcToDucklingTime <$> getCurrentTime
    --pure $ parseAndResolve (rulesFor (locale ctx) (HashSet.fromList [(This Time)])) input ctx
    pure $ analyze input ctx $ HashSet.fromList [(This Time)]

