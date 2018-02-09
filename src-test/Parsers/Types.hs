{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parsers.Types where

import Gargantext.Prelude

import Test.QuickCheck
import Test.QuickCheck.Instances()

import Text.Parsec.Pos
import Text.Parsec.Error (ParseError, Message(..), newErrorMessage)
import Data.Time.LocalTime (ZonedTime (..))
import Data.Eq (Eq(..))
deriving instance Eq ZonedTime

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
