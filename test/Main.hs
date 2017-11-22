import Data.Gargantext.Types.Main (Language(..))
--import qualified Ngrams.Lang.Fr as Fr
import qualified Ngrams.Lang as Lang
import qualified Ngrams.Lang.Occurrences as Occ
import qualified Ngrams.Metrics as Metrics
main :: IO ()
main = do
    Occ.parsersTest
    Lang.ngramsExtractionTest EN
    Metrics.main
    --Lang.ngramsExtractionTest FR

