{-# LANGUAGE NoImplicitPrelude #-}

module Parsers.Date where

import Gargantext.Prelude

import Test.Hspec
import Test.QuickCheck
import Parsers.Types

import Control.Applicative ((<*>))
import Data.Either (Either(..))
import Data.Time (ZonedTime(..))
import Data.Text (pack, Text)

import Text.Parsec.Error (ParseError)
import Duckling.Time.Types (toRFC3339)
import Gargantext.Parsers.Date (fromRFC3339)

fromRFC3339Inv ::  Either ParseError ZonedTime -> Text
fromRFC3339Inv (Right z) = toRFC3339 z
fromRFC3339Inv (Left pe) = panic . pack $ show pe

testFromRFC3339 :: IO ()
testFromRFC3339 = hspec $ do
  describe "Test fromRFC3339: " $ do
    it "is the inverse of Duckling's toRFC3339" $ property $
      ((==) <*> (fromRFC3339 . fromRFC3339Inv)) . Right . looseZonedTimePrecision

      -- \x -> uncurry (==) $ (,) <*> (fromRFC3339 . fromRFC3339Inv) $ Right $ looseZonedTimePrecision x

      -- \x -> let e = Right x :: Either ParseError ZonedTime
      --       in fmap looseZonedTimePrecision e == (fromRFC3339 . fromRFC3339Inv ) (fmap looseZonedTimePrecision e)
