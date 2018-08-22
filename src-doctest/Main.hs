import System.FilePath.Glob
import Test.DocTest

main :: IO ()
main = glob "src/Gargantext/Text/Metrics.hs" >>= doctest

