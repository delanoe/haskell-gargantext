{-|
Module      : Gargantext.Core.Types.Individu
Description : Short description
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Individu defintions
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

module Gargantext.Core.Types.Individu
  where

import Gargantext.Prelude hiding (reverse)
import Data.Text (Text, pack, reverse)

type Username = Text
type Password = Text

type UsernameMaster = Username
type UsernameSimple = Username


arbitraryUsername :: [Username]
arbitraryUsername = ["gargantua"] <> users
  where
    users = zipWith (\a b -> a <> (pack . show) b) 
                    (repeat "user") ([1..20]::[Int])

arbitraryPassword :: [Password]
arbitraryPassword = map reverse arbitraryUsername



