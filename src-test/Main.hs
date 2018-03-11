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
import Gargantext.Types.Main (Language(..))
--import qualified Ngrams.Lang.Fr as Fr
import qualified Ngrams.Lang as Lang
import qualified Ngrams.Lang.Occurrences as Occ
import qualified Ngrams.Metrics as Metrics
import qualified Parsers.Date as PD

main :: IO ()
main = do
    Occ.parsersTest
    Lang.ngramsExtractionTest FR
    Lang.ngramsExtractionTest EN
    Metrics.main
    PD.testFromRFC3339
