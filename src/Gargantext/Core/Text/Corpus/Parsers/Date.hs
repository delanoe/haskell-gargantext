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

import System.Environment (getEnv)
import Data.Aeson (toJSON, Value)
import Data.Either (Either(..))
import Data.HashMap.Strict as HM hiding (map)
import Data.Text (Text, unpack, splitOn, replace)
import Data.Time (defaultTimeLocale, iso8601DateFormat, parseTimeM, toGregorian)
import qualified Data.Time.Calendar as DTC
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Clock ( secondsToDiffTime)
import Data.Time.LocalTime (utc)
import Data.Time.LocalTime.TimeZone.Series (zonedTimeToZoneSeriesTime)
import Duckling.Api (analyze)
import Duckling.Core (makeLocale, Dimension(Time))
import Duckling.Types (Seal(..))
import Duckling.Resolve (fromUTC, Context(Context, referenceTime, locale), DucklingTime(DucklingTime), Options(..))
import Duckling.Types (ResolvedToken(..), ResolvedVal(..))
import Gargantext.Core (Lang(FR,EN))
import Gargantext.Core.Types (DebugMode(..), withDebugMode)
import Gargantext.Prelude
--import qualified Control.Exception as CE
import qualified Data.Aeson        as Json
import qualified Data.HashSet      as HashSet
import qualified Duckling.Core     as DC

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
-- >>> parse FR (pack "10 avril 1900 à 19H")
-- 1900-04-10 19:00:00 UTC
-- >>> parse EN (pack "April 10 1900")
-- 1900-04-10 00:00:00 UTC
parse :: Lang -> Text -> IO UTCTime
parse lang s = do
  dateStr' <- parseRawSafe lang s
  case dateFlow dateStr' of
    DateFlowSuccess ok -> pure ok
    _                  -> withDebugMode (DebugMode True)
                                        "[G.C.T.P.T.Date parse]" (lang,s)
                                        $ getCurrentTime


defaultDate :: Text
defaultDate = "0-0-0T0:0:0"

type DateFormat  = Text
type DateDefault = Text


data DateFlow = DucklingSuccess { ds_result  :: Text }
              | DucklingFailure { df_result  :: Text }
              | ReadFailure1    { rf1_result :: Text }
              | ReadFailure2    { rf2_result :: Text }
              | DateFlowSuccess  { success :: UTCTime }
              | DateFlowFailure
  deriving Show

--{-
dateFlow :: DateFlow -> DateFlow
dateFlow (DucklingSuccess res) = case (head $ splitOn "." res)  of
                             Nothing -> dateFlow (ReadFailure1 res)
                             Just re -> case readDate res of
                                Nothing -> dateFlow (ReadFailure1 re)
                                Just ok -> DateFlowSuccess ok
dateFlow (DucklingFailure txt) = case readDate $ replace " " "T" txt of
                             Nothing -> dateFlow (ReadFailure1 txt)
                             Just ok -> DateFlowSuccess ok
dateFlow (ReadFailure1 txt) = case readDate txt of
                          Nothing -> dateFlow $ ReadFailure2 txt
                          Just ok -> DateFlowSuccess ok
dateFlow (ReadFailure2 txt) = case readDate $ replace " " "" txt <> "-01-01T00:00:00" of
                          Nothing -> DateFlowFailure
                          Just ok -> DateFlowSuccess ok
dateFlow _ = DateFlowFailure
--}

readDate :: Text -> Maybe UTCTime
readDate txt = do
  let format = cs $ iso8601DateFormat (Just "%H:%M:%S")
  parseTimeM True defaultTimeLocale (unpack format) (cs txt)


-- TODO add Paris at Duckling.Locale Region datatype
-- | To get Homogeinity of the languages
--   TODO : put this in a more generic place in the source code
parserLang :: Lang -> DC.Lang
parserLang FR = DC.FR
parserLang EN    = DC.EN
parserLang lang  = panic $ "[G.C.T.C.P.Date] Lang not implemented" <> (cs $ show lang)

-- | Final Date parser API
-- IO can be avoided here:
-- currentContext :: Lang -> IO Context
-- currentContext lang = localContext lang <$> utcToDucklingTime <$> getCurrentTime
-- parseRaw :: Context -> Text -> SomeErrorHandling Text


parseRawSafe :: Lang -> Text -> IO DateFlow
parseRawSafe lang text = do
  let triedParseRaw = parseRaw lang text
  dateStr' <- case triedParseRaw of
      --Left (CE.SomeException err) -> do
      Left err -> do
        envLang <- getEnv "LANG"
        printDebug "[G.C.T.C.P.Date] Exception: " (err, envLang, lang, text)
        pure $ DucklingFailure text
      Right res -> pure $ DucklingSuccess res
  pure dateStr'

--tryParseRaw :: CE.Exception e => Lang -> Text -> IO (Either e Text)
--tryParseRaw lang text = CE.try (parseRaw lang text)

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

