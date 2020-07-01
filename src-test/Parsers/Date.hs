{-|
Module      : Parsers.Date
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}


module Parsers.Date where


import Test.Hspec
import Test.QuickCheck

import Control.Applicative ((<*>))
import Data.Either (Either(..))
import Data.Time (ZonedTime(..))
import Data.Text (pack, Text)

import Text.Parsec.Error (ParseError)
import Duckling.Time.Types (toRFC3339)

-----------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Text.Corpus.Parsers.Date.Parsec (fromRFC3339)
import Parsers.Types
-----------------------------------------------------------

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
