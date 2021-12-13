{-|
Module      : Gargantext.API.Node.Document.Export
Description : Document export
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.API.Node.Document.Export
  where

import Gargantext.API.Node.Document.Export.Types
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core.Types
import Gargantext.Prelude

--------------------------------------------------
-- | Hashes are ordered by Set
getDocuments :: DocId
             -> GargNoServer [Document]
getDocuments dId = do
  printDebug "[getDocuments] dId" dId
  pure []
