{-|
Module      : Gargantext.Text.Corpus.Parsers.Date.Attoparsec
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}


module Gargantext.Text.Corpus.Parsers.Date.Attoparsec
  where

import Control.Applicative ((<*))
import Data.Attoparsec.ByteString (Parser, take)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Tuple.Extra (first)
import Gargantext.Prelude hiding (takeWhile, take)

-------------------------------------------------------------

parserWith :: Parser ByteString -> Parser [(ByteString, ByteString)]
parserWith sep = do
  day <- take 2 <* sep
  mon <- take 2 <* sep
  yea <- take 4
  pure $ map (first (\x -> "publication_" <> x))
       [ ("day",day)
       , ("month", mon)
       , ("year", yea)
       , ("date", yea <> "-" <> mon <> "-" <> day <> "T0:0:0")
       ]

