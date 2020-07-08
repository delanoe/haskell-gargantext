{-|
Module      : Parsers.Types
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE StandaloneDeriving   #-}

module Parsers.Types where

import Gargantext.Prelude

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Text.Parsec.Pos
import Text.Parsec.Error (ParseError, Message(..), newErrorMessage)
import Data.Time.LocalTime (ZonedTime (..), TimeZone (..), TimeOfDay(..), LocalTime(..))
import Data.Eq (Eq(..))
import Data.Either (Either(..))

deriving instance Eq ZonedTime

looseTimeOfDayPrecision :: TimeOfDay -> TimeOfDay
looseTimeOfDayPrecision (TimeOfDay h m _) = TimeOfDay h m 0

looseLocalTimePrecision :: LocalTime -> LocalTime
looseLocalTimePrecision (LocalTime ld ltd) = LocalTime ld $ looseTimeOfDayPrecision ltd

looseTimeZonePrecision :: TimeZone -> TimeZone
looseTimeZonePrecision (TimeZone zm _ _) = TimeZone zm False "CET"

looseZonedTimePrecision :: ZonedTime -> ZonedTime
looseZonedTimePrecision (ZonedTime lt tz) = ZonedTime (looseLocalTimePrecision lt) $ looseTimeZonePrecision tz

loosePrecisionEitherPEZT :: Either ParseError ZonedTime -> Either ParseError ZonedTime
loosePrecisionEitherPEZT (Right zt) = Right $ looseZonedTimePrecision zt
loosePrecisionEitherPEZT pe = pe

instance Arbitrary Message where
  arbitrary = do
    msgContent <- arbitrary
    oneof $ return <$> [SysUnExpect msgContent
                       , UnExpect msgContent
                       , Expect msgContent
                       , Message msgContent
                       ]

instance Arbitrary SourcePos where
  arbitrary = do
    sn <- arbitrary
    l <- arbitrary
    c <- arbitrary
    return $ newPos sn l c

instance Arbitrary ParseError where
  arbitrary = do
    sp <- arbitrary
    msg <- arbitrary
    return $ newErrorMessage msg sp
