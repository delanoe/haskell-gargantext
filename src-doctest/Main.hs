import System.FilePath.Glob
import Test.DocTest
import Gargantext.Prelude

main :: IO ()
main = glob "src/Gargantext/" >>= doctest

