{-# LANGUAGE NoImplicitPrelude #-}

module Parsers.Types where

import Gargantext.Prelude

import Test.QuickCheck

import Text.Parsec.Pos
import Text.Parsec.Error (ParseError(..), Message(..), newErrorMessage)

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
