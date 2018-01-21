module Data.Gargantext.Utils.Chronos where

import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Time as DT
import qualified Data.UTC as DU

import Data.Time
import Data.Time.Clock.POSIX
import Text.Regex

parseDate :: String -> Maybe [String]
parseDate d = matchRegex (mkRegex "(.*)/(.*)/(.*)") d

getDate' :: Maybe [String] -> (Integer, Int, Int)
getDate' d
    | isJust d == True = toGregorian $ fromGregorian (read year) (read month) (read day)
    | otherwise = toGregorian $ fromGregorian 2015 1 1
      where
        Just [day, month, year] = d

getDate :: String -> (Integer, Int, Int)
getDate = getDate' . parseDate

--getDateDay :: Maybe [String] -> Day
--getDateDay d = fromGregorian (read year) (read month) (read day)
--      where Just [day, month, year] = matchRegex (mkRegex "(.*)/(.*)/(.*)") d

getDateDay' :: Maybe [String] -> Day
getDateDay' d 
    | isJust d == True = fromGregorian (read year) (read month) (read day)
    | otherwise = fromGregorian 2015 1 1
      where Just [day, month, year] = d

getDateDay :: String -> Day
getDateDay = getDateDay' . parseDate

getDateUTC :: String -> String
getDateUTC d = show $ DT.UTCTime (getDateDay d) (DT.timeOfDayToTime $ DT.TimeOfDay 0 0 0)

getYear :: String -> String
getYear date = s where
    (y, m, d) = getDate date
    s = show y

getMonth :: String -> String
getMonth date = s where
    (y, m, d) = getDate date
    s = show m

getDay :: String -> String
getDay date = s where
    (y, m, d) = getDate date
    s = show d

--for Dates exported via xls2csv tool
type MT = Maybe (DU.Local DU.DateTime)
type MS = Maybe String

--getDate'' :: String -> String
--getDate'' gd = d where
--    start = "1900-01-01T00:00:00Z" 
--    da = (DU.parseRfc3339 start :: MT) >>= DU.addDays ( (read gd :: Integer) -2) >>= DU.renderRfc3339 :: MS
--    d = fromJust da
--
--getDate''' :: String -> String
--getDate''' gd = d where
--    start = "1900-01-01T00:00:00Z" 
--    da = (DU.parseRfc3339 start :: MT) >>= DU.addDays ( (read gd :: Integer) -2) >>= DU.renderIso8601CalendarDate :: MS
--    d = fromJust da
--
--date2greg :: String -> 
date2greg date = (y, m, d) where
    (y, m, d) = DT.toGregorian $ DT.addDays ((read date :: Integer) -2) $ DT.utctDay (read "1900-01-01 00:00:00" :: DT.UTCTime)


getYear' :: String -> String
getYear' date = s where
    (y, m, d) = date2greg date
    s = show y


getMonth' :: String -> String
getMonth' date = s where
    (y, m, d) = date2greg date
    s = show m


getDay' :: String -> String
getDay' date = s where
    (y, m, d) = date2greg date
    s = show d



