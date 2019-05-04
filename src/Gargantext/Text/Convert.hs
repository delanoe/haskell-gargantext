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

module Gargantext.Text.Convert (risPress2csv)
    where

import System.FilePath (FilePath()) -- , takeExtension)
import Gargantext.Prelude
import Gargantext.Text.Parsers.CSV (writeDocs2Csv)
import Gargantext.Text.Parsers (parseDocs, FileFormat(..))


risPress2csv :: FilePath -> IO ()
risPress2csv f = parseDocs RisPresse    (f <> ".ris")
               >>= \hs -> writeDocs2Csv (f <> ".csv") hs

