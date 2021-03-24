{-|
Module      : Gargantext.API.Flow
Description : Main Flow API DataTypes
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

module Gargantext.API.Flow
  where

-- import Gargantext.API.Prelude
import Gargantext.Prelude

data InputFlow = TextsInput
               | NgramsInput
               | ListInput

data Flow = EndFlow
          | Texts  InputFlow [Flow]
          | Ngrams InputFlow [Flow]
          | Lists  InputFlow [Flow]

data OutputFlow

flow :: Flow -> OutputFlow
flow = undefined

