{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.Date
Description : Some utils to parse dates
Copyright   : (c) CNRS 2017-present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

According to the language of the text, parseDateRaw returns date as Text:

TODO : Add some tests
import Gargantext.Core.Text.Corpus.Parsers.Date as DGP
DGP.parseDateRaw DGP.FR "12 avril 2010" == "2010-04-12T00:00:00.000+00:00"
-}

{-# LANGUAGE TypeFamilies #-}

module Gargantext.Core.Text.Corpus.Parsers.Date
{-(parse, parseRaw, dateSplit, Year, Month, Day)-}
  where

import Data.Aeson (toJSON, Value)
import Data.Either (Either(..))
import Data.HashMap.Strict as HM hiding (map)
import Data.Text (Text, unpack, splitOn)
import Data.Time (parseTimeOrError, defaultTimeLocale, toGregorian)
import qualified Data.Time.Calendar as DTC
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.LocalTime (utc)
import Data.Time.LocalTime.TimeZone.Series (zonedTimeToZoneSeriesTime)
import Duckling.Api (analyze)
import Duckling.Core (makeLocale, Dimension(Time))
import Duckling.Types (Seal(..))
import Duckling.Resolve (fromUTC, Context(Context, referenceTime, locale), DucklingTime(DucklingTime), Options(..))
import Duckling.Types (ResolvedToken(..), ResolvedVal(..))
import Gargantext.Core (Lang(FR,EN))
import Gargantext.Prelude
import qualified Data.Aeson   as Json
import qualified Data.HashSet as HashSet
import qualified Duckling.Core as DC

------------------------------------------------------------------------
-- | Parse date to Ints
-- TODO add hours, minutes and seconds
dateSplit :: Lang -> Maybe Text -> (Maybe UTCTime, (Maybe Year, Maybe Month, Maybe Day))
dateSplit _ Nothing    = (Nothing, (Nothing, Nothing, Nothing))
dateSplit l (Just txt) = do
  let utcTime = parse l txt
  let (y, m, d) = split' utcTime
  (Just utcTime, (Just y, Just m,Just d))

split' :: UTCTime -> (Year, Month, Day)
split' utcTime = (fromIntegral y, m, d)
  where
    (UTCTime day _) = utcTime
    (y,m,d)         = toGregorian day

type Year  = Int
type Month = Int
type Day   = Int
------------------------------------------------------------------------

-- | Date Parser
-- Parses dates mentions in full text given the language.
-- >>> parseDate FR (pack "10 avril 1979 à 19H")
-- 1979-04-10 19:00:00 UTC
-- >>> parseDate EN (pack "April 10 1979")
-- 1979-04-10 00:00:00 UTC
parse :: Lang -> Text -> UTCTime
parse = parseDate' "%Y-%m-%dT%T" "0-0-0T0:0:0"

type DateFormat  = Text
type DateDefault = Text

parseDate' :: DateFormat
           -> DateDefault
           -> Lang
           -> Text
           -> UTCTime
parseDate' format def lang s = do
  let dateStr' = parseRaw lang s
  case dateStr' of
    Left _err -> defaultUTCTime
    Right ""  -> defaultUTCTime
    Right ds  -> do
      let dateStr = unpack
                  $ maybe def identity
                  $ head
                  $ splitOn "." ds
      parseTimeOrError True defaultTimeLocale (unpack format) dateStr


-- TODO add Paris at Duckling.Locale Region datatype
-- | To get Homogeinity of the languages
--   TODO : put this in a more generic place in the source code
parserLang :: Lang -> DC.Lang
parserLang FR = DC.FR
parserLang EN = DC.EN
parserLang _  = panic "not implemented"

-- | Final Date parser API
-- IO can be avoided here:
-- currentContext :: Lang -> IO Context
-- currentContext lang = localContext lang <$> utcToDucklingTime <$> getCurrentTime
-- parseRaw :: Context -> Text -> SomeErrorHandling Text

parseRaw :: Lang -> Text -> Either Text Text
parseRaw lang text = do -- case result
    let maybeResult = extractValue $ getTimeValue
                                   $ parseDateWithDuckling lang text (Options True)
    case maybeResult of
      Just result -> Right result
      Nothing     -> do
        -- printDebug ("[G.C.T.C.P.D.parseRaw] ERROR " <> (cs . show) lang) text
        Left $ "[G.C.T.C.P.D.parseRaw ERROR] " <> (cs . show) lang <> " :: " <> text

getTimeValue :: [ResolvedToken] -> Maybe Value
getTimeValue rt = case head rt of
  Nothing -> do
    Nothing
  Just x  -> case rval x of
    RVal Time t -> Just $ toJSON t
    _  -> do
      Nothing

extractValue :: Maybe Value -> Maybe Text
extractValue (Just (Json.Object object)) =
  case HM.lookup "value" object of
    Just (Json.String date) -> Just date
    _                  -> Nothing
extractValue _ = Nothing

-- | Current Time in DucklingTime format
-- TODO : get local Time in a more generic way
utcToDucklingTime :: UTCTime -> DucklingTime
utcToDucklingTime time = DucklingTime . zonedTimeToZoneSeriesTime $ fromUTC time utc

-- | Local Context which depends on Lang and Time
localContext :: Lang -> DucklingTime -> Context
localContext lang dt = Context { referenceTime = dt
                               , locale = makeLocale (parserLang lang) Nothing }

defaultDay :: DTC.Day
defaultDay = DTC.fromGregorian 1 1 1

defaultUTCTime :: UTCTime
defaultUTCTime = UTCTime { utctDay = defaultDay
                         , utctDayTime = secondsToDiffTime 0 }

-- | Date parser with Duckling
parseDateWithDuckling :: Lang -> Text -> Options -> [ResolvedToken]
parseDateWithDuckling lang input options = do
  let contxt = localContext lang $ utcToDucklingTime defaultUTCTime
  --pure $ parseAndResolve (rulesFor (locale ctx) (HashSet.fromList [(This Time)])) input ctx
  -- TODO check/test Options False or True
  analyze input contxt options $ HashSet.fromList [(Seal Time)]

