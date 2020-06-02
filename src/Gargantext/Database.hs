{-|
Module      : Gargantext.Database
Description : Tools for Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

All Database related stuff here.

Target: just import this module and nothing else to work with
Gargantext's database.

-}


module Gargantext.Database ( module Gargantext.Database.Prelude
                        -- , module Gargantext.Database.Bashql
                           )
    where

import Gargantext.Database.Prelude (connectGargandb)
-- import Gargantext.Database.Bashql
