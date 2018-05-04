{-|
Module      : Gargantext.Core
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Gargantext.Core
  where

------------------------------------------------------------------------
-- | Language of a Text
-- For simplicity, we suppose text has an homogenous language

data Lang = EN | FR

-- | DE | IT | SP
--  EN == english
--  FR == french
--  DE == deutch  (not implemented yet)
--  IT == italian (not implemented yet)
--  SP == spanish (not implemented yet)
--  ... add your language and help us to implement it (:
