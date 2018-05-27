module Main where

import System.Environment (getArgs)
import Data.Louvain
import Data.Utils
import Data.GexfParser


main :: IO ()
main = do
    [file] <- getArgs
    graph <- mkGraph' <$> importGraphFromGexf file
    print $ bestpartition True graph


