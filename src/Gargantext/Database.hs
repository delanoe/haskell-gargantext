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

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Database ( module Gargantext.Database.Utils
                        -- , module Gargantext.Database.Bashql
                           )
    where

import Gargantext.Database.Utils (connectGargandb)
-- import Gargantext.Database.Bashql
