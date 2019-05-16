{-|
Module      : Gargantext.API
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Gargantext.Ext.IMT where

import Gargantext.Prelude
import Data.Text (Text, splitOn)
import Data.Map (Map)

import qualified Data.Set    as S
import qualified Data.List   as DL
import qualified Data.Vector as DV
import qualified Data.Map    as M

import Gargantext.Text.Metrics.Freq as F
import Gargantext.Text.Parsers.CSV as CSV

data School = School { school_shortName :: Text
                     , school_longName  :: Text
                     , school_id        :: Text
} deriving (Show, Read, Eq)

schools :: [School]
schools = [ School
            ("Mines Albi-Carmaux")
            ("Mines Albi-Carmaux - École nationale supérieure des Mines d'Albi‐Carmaux")
            ("469216")
          , School
            ("Mines Alès")
            ("EMA - École des Mines d'Alès")
            ("6279")
          , School
            ("Mines Douai")
            ("Mines Douai EMD - École des Mines de Douai")
            ("224096")
          , School
            ("Mines Lille")
            ("Mines Lille - École des Mines de Lille")
            ("144103")
          , School
            ("IMT Lille Douai")
            ("IMT Lille Douai")
            ("497330")
          , School
            ("Mines Nantes")
            ("Mines Nantes - Mines Nantes")
            ("84538")
          , School
            ("Télécom Bretagne")
            ("Télécom Bretagne")
            ("301262")
          , School
            ("IMT Atlantique")
            ("IMT Atlantique - IMT Atlantique Bretagne-Pays de la Loire")
            ("481355")
          , School
            ("Mines Saint-Étienne")
            ("Mines Saint-Étienne MSE - École des Mines de Saint-Étienne")
            ("29212")
          , School
            ("Télécom École de Management")
            ("TEM - Télécom Ecole de Management")
            ("301442")
          , School
            ("IMT Business School")
            ("IMT Business School")
            ("542824")
          , School
            ("Télécom ParisTech")
            ("Télécom ParisTech")
            ("300362")
          , School
            ("Télécom SudParis")
            ("TSP - Télécom SudParis")
            ("352124")
          , School
            ("ARMINES")
            ("ARMINES")
            ("300362")
          , School
            ("Eurecom")
            ("Eurecom")
            ("421532")
          , School
            ("Mines ParisTech")
            ("MINES ParisTech - École nationale supérieure des mines de Paris")
            ("301492")
            ]

mapIdSchool :: Map Text Text
mapIdSchool = M.fromList $ Gargantext.Prelude.map (\(School n _ i) -> (i,n)) schools

hal_data :: IO (DV.Vector CsvHal)
hal_data = snd <$> CSV.readCsvHal "doc/corpus_imt/Gargantext_Corpus.csv"

names :: S.Set Text
names = S.fromList $ Gargantext.Prelude.map (\s -> school_id s) schools

toSchoolName :: Text -> Text
toSchoolName t = case M.lookup t mapIdSchool of
  Nothing -> t
  Just t' -> t'

publisBySchool :: DV.Vector CsvHal -> [(Maybe Text, Int)]
publisBySchool hal_data' = Gargantext.Prelude.map (\(i,n) -> (M.lookup i mapIdSchool, n))
                        $ DL.filter (\i -> S.member  (fst i) names) 
                        $ DL.reverse
                        $ DL.sortOn snd
                        $ M.toList
                        $ F.freq
                        $ DL.concat
                        $ DV.toList
                        $ DV.map (\n -> splitOn ( ", ") (csvHal_instStructId_i n) )
                        $ DV.filter (\n -> csvHal_publication_year n == 2017) hal_data'


