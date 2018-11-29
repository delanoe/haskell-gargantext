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

module Gargantext.Ext.IMT where

import Gargantext.Prelude
import Data.Text (Text, pack, splitOn)
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
            (pack "Mines Albi-Carmaux")
            (pack "Mines Albi-Carmaux - École nationale supérieure des Mines d'Albi‐Carmaux")
            (pack "469216")
          , School
            (pack "Mines Alès")
            (pack "EMA - École des Mines d'Alès")
            (pack "6279")
          
          , School
            (pack "Mines Douai")
            (pack "Mines Douai EMD - École des Mines de Douai")
            (pack "224096")
          
          , School
            (pack "Mines Nantes")
            (pack "Mines Nantes - Mines Nantes")
            (pack "84538")
          
--          , School
--            (pack "Mines ParisTech")
--            (pack "MINES ParisTech - École nationale supérieure des mines de Paris")
--            (pack "301492")
--          
          , School
            (pack "Mines Saint-Étienne")
            (pack "Mines Saint-Étienne MSE - École des Mines de Saint-Étienne")
            (pack "29212")
          
          , School
            (pack "Télécom Bretagne")
            (pack "Télécom Bretagne")
            (pack "301262")
          
          , School
            (pack "Télécom École de Management")
            (pack "TEM - Télécom Ecole de Management")
            (pack "301442")
          
          , School
            (pack "Télécom ParisTech")
            (pack "Télécom ParisTech")
            (pack "300362")
          
          , School
            (pack "Télécom SudParis")
            (pack "TSP - Télécom SudParis")
            (pack "352124")
          
          , School
            (pack "IMT Atlantique")
            (pack "IMT Atlantique - IMT Atlantique Bretagne-Pays de la Loire")
            (pack "481355")
            ]

mapIdSchool :: Map Text Text
mapIdSchool = M.fromList $ Gargantext.Prelude.map (\(School n _ i) -> (i,n)) schools

hal_data :: IO (DV.Vector CsvHal)
hal_data = snd <$> CSV.readHal "doc/corpus_imt/Gargantext_Corpus.csv"

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
                        $ DV.map (\n -> splitOn (pack ", ") (csvHal_instStructId_i n) )
                        $ DV.filter (\n -> csvHal_publication_year n == 2017) hal_data'


