{-|
Module      : Gargantext.API.Utils
Description : Server API main Types
Copyright   : (c) CNRS, 2017-Present
License     : BSD3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Mainly copied from Servant.Job.Utils (Thanks)

-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Gargantext.API.Utils
  where

import Gargantext.Prelude
import Data.Maybe (Maybe, fromMaybe)
import Prelude (String)
import qualified Data.Text as T

infixr 4 ?|

-- Reverse infix form of "fromMaybe"
(?|) :: Maybe a -> a -> a
(?|) = flip fromMaybe

infixr 4 ?!

-- Reverse infix form of "fromJust" with a custom error message
(?!) :: Maybe a -> String -> a
(?!) ma' msg = ma' ?| panic (T.pack msg)
