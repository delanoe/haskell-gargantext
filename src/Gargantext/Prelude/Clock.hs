{-|
Module      : Gargantext.Prelude.Clock
Description : Useful Tools near Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

module Gargantext.Prelude.Clock
  where

import Formatting.Clock (timeSpecs)
import Formatting.Internal (Format(..))
import Gargantext.Prelude
import qualified System.Clock          as Clock (getTime, TimeSpec, Clock(..))

---------------------------------------------------------------------------------
getTime :: MonadBase IO m => m Clock.TimeSpec
getTime = liftBase $ Clock.getTime Clock.ProcessCPUTime

hasTime :: Formatting.Internal.Format r (Clock.TimeSpec -> Clock.TimeSpec -> r)
hasTime = timeSpecs


