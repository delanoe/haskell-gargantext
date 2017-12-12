module Main where

import Gargantext.Parser.Wos (parseWos)

main :: IO ()
main = parseWos "/tmp/DeepNeuralNetworkFull.zip"
