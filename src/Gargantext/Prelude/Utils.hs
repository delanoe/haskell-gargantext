{-|
Module      : Gargantext.Prelude.Utils
Description : Useful Tools near Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO_1: qualitative  tests (human)
TODO_2: quantitative tests (coded)


-}

module Gargantext.Prelude.Utils
  where

import Control.Monad.Random.Class (MonadRandom)
import qualified System.Random.Shuffle as SRS

------------------------------------------------------------------------
-- | Misc Utils
shuffle :: MonadRandom m => [a] -> m [a]
shuffle ns = SRS.shuffleM ns 
--------------------------------------------------------------------------

-- TODO gargDB instance for NodeType
{-
data NodeToHash = NodeToHash { nodeType :: NodeType
                             , nodeId   :: NodeId
                             }
-}
