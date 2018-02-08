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
