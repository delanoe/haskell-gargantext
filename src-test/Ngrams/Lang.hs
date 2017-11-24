module Ngrams.Lang where


import Data.Gargantext.Types.Main (Language(..))
import qualified Ngrams.Lang.Fr as Fr
import qualified Ngrams.Lang.En as En

ngramsExtractionTest :: Language -> IO ()
ngramsExtractionTest FR = Fr.ngramsExtractionTest
ngramsExtractionTest EN = En.ngramsExtractionTest

