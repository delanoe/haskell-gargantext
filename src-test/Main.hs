{-|
Module      : Main.hs
Description : Main for Gargantext Tests
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


import Gargantext.Prelude
import Gargantext.Core (Lang(..))

import qualified Core.Utils as Utils
--import qualified Ngrams.Lang.Fr as Fr
--import qualified Ngrams.Lang as Lang
import qualified Ngrams.Lang.Occurrences as Occ
import qualified Ngrams.Metrics          as Metrics
import qualified Parsers.Date            as PD
-- import qualified Graph.Distance          as GD
import qualified Graph.Clustering        as Graph
import qualified Utils.Crypto            as Crypto

main :: IO ()
main = do
  Utils.test
--    Occ.parsersTest
--    Lang.ngramsExtractionTest FR
--    Lang.ngramsExtractionTest EN
--    Metrics.main
  Graph.test
  PD.testFromRFC3339
--    GD.test
  Crypto.test
