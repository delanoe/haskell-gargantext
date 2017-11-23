module Data.Gargantext.Parsers.Date where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime.TimeZone.Series (zonedTimeToZoneSeriesTime)
import Data.Time.LocalTime (utc)
import Duckling.Resolve (fromUTC, Context(Context, referenceTime, locale)
                        , DucklingTime(DucklingTime)
                        )
--import Duckling.Core (makeLocale, Lang(FR,EN), Some(This), Dimension(Time))
import Duckling.Core (makeLocale, Lang(), Some(This), Dimension(Time))

import Duckling.Api (analyze)
import qualified Data.HashSet as HashSet

import Data.Text (Text)
-- import Duckling.Engine (parseAndResolve)
-- import Duckling.Rules (rulesFor)
-- import Duckling.Debug as DB
import Duckling.Types (ResolvedToken)

-- TODO add Paris at Duckling.Locale Region datatype

utcToDucklingTime :: UTCTime -> DucklingTime
utcToDucklingTime time = DucklingTime . zonedTimeToZoneSeriesTime $ fromUTC time utc

localContext :: Lang -> DucklingTime -> Context
localContext lang dt = Context {referenceTime = dt, locale = makeLocale lang Nothing}

parseDate :: Lang -> Text -> IO [ResolvedToken]
parseDate lang input = do
    ctx <- localContext lang <$> utcToDucklingTime <$> getCurrentTime
    --pure $ parseAndResolve (rulesFor (locale ctx) (HashSet.fromList [(This Time)])) input ctx
    pure $ analyze input ctx $ HashSet.fromList [(This Time)]
