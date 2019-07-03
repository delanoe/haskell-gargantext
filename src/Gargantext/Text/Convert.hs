{-|
Module      : Gargantext.Text.Convert
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Format Converter.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.Convert (risPress2csvWrite)
    where

import System.FilePath (FilePath()) -- , takeExtension)
import Gargantext.Prelude
import Gargantext.Text.Corpus.Parsers.CSV (writeDocs2Csv)
import Gargantext.Text.Corpus.Parsers (parseFile, FileFormat(..))


risPress2csvWrite :: FilePath -> IO ()
risPress2csvWrite f = parseFile RisPresse    (f <> ".ris")
               >>= \hs -> writeDocs2Csv (f <> ".csv") hs



