{-|
Module      : Gargantext.Database.Query.Table.Node.Contact
Description : Update Node in Database (Postgres)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.Node.Contact
  where

import Gargantext.Database.Admin.Types.Node ( Node)
import Gargantext.Database.Admin.Types.Hyperdata.Contact

------------------------------------------------------------------------

type NodeContact  = Node HyperdataContact


