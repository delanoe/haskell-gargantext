import Data.Gargantext.Types.Main (Language(..))
--import qualified Ngrams.Lang.Fr as Fr
import qualified Ngrams.Lang as Lang
import qualified Ngrams.Lang.Occurrences as Occ

main :: IO ()
main = do
    Occ.parsersTest
    Lang.ngramsExtractionTest EN
    --Lang.ngramsExtractionTest FR

