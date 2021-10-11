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
import Data.HashMap.Strict as HM hiding (map)
import Data.Text (Text, unpack, splitOn)
import Data.Time (parseTimeOrError, defaultTimeLocale, toGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
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
dateSplit :: Lang -> Maybe Text -> IO (Maybe UTCTime, (Maybe Year, Maybe Month, Maybe Day))
dateSplit _ Nothing    = pure (Nothing, (Nothing, Nothing, Nothing))
dateSplit l (Just txt) = do
  utcTime <- parse l txt
  let (y, m, d) = split' utcTime
  pure (Just utcTime, (Just y, Just m,Just d))

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
parse :: Lang -> Text -> IO UTCTime
parse lang s = parseDate' "%Y-%m-%dT%T" "0-0-0T0:0:0" lang s

type DateFormat  = Text
type DateDefault = Text

parseDate' :: DateFormat
           -> DateDefault
           -> Lang
           -> Text
           -> IO UTCTime
parseDate' format def lang s = do
  dateStr' <- parseRaw lang s
  if dateStr' == ""
    then getCurrentTime
    else do
      let dateStr = unpack
                  $ maybe def identity
                  $ head
                  $ splitOn "." dateStr'
      pure $ parseTimeOrError True defaultTimeLocale (unpack format) dateStr


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

parseRaw :: Lang -> Text -> IO Text
parseRaw lang text = do -- case result
    maybeResult <- extractValue <$> getTimeValue
                                <$> parseDateWithDuckling lang text (Options True)
    case maybeResult of
      Just result -> pure result
      Nothing     -> do
        printDebug ("[G.C.T.C.P.D.parseRaw] ERROR " <> (cs . show) lang)
                   text
        pure ""

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
localContext lang dt = Context {referenceTime = dt, locale = makeLocale (parserLang lang) Nothing}

-- | Date parser with Duckling
parseDateWithDuckling :: Lang -> Text -> Options -> IO [ResolvedToken]
parseDateWithDuckling lang input options = do
    contxt <- localContext lang <$> utcToDucklingTime <$> getCurrentTime
    --pure $ parseAndResolve (rulesFor (locale ctx) (HashSet.fromList [(This Time)])) input ctx
    -- TODO check/test Options False or True
    pure $ analyze input contxt options $ HashSet.fromList [(Seal Time)]



