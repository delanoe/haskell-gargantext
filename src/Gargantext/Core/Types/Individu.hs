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

module Gargantext.Core.Types.Individu
  where

import Data.Text (Text)

type Username = Text
type UsernameMaster = Username
type UsernameSimple = Username

