{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Gargantext.API
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}


module Gargantext.Core.Ext.IMT where

import Data.Either (Either(..))
import Data.Map.Strict (Map)
import Data.Text (Text, splitOn)

import qualified Data.Set    as S
import qualified Data.List   as DL
import qualified Data.Vector as DV
import qualified Data.Map.Strict    as M
import qualified Prelude

import Data.Morpheus.Types (GQLType)
import GHC.Generics (Generic)

import Gargantext.Prelude

import Gargantext.Core.Text.Metrics.Utils      as Utils
import Gargantext.Core.Text.Corpus.Parsers.CSV as CSV

data School = School { school_shortName :: Text
                     , school_longName  :: Text
                     , school_id        :: Text
} deriving (Generic, GQLType, Show, Read, Eq)

schools :: [School]
schools =
  [ School
    { school_shortName = "ARMINES"
    , school_longName = "ARMINES"
    , school_id = "300104" }
  , School
    { school_shortName = "Eurecom"
    , school_longName = "Eurecom"
    , school_id = "421532" }
  , School
    { school_shortName = "IMT Atlantique"
    , school_longName = "IMT Atlantique - IMT Atlantique Bretagne-Pays de la Loire"
    , school_id = "481355" }
  , School
    { school_shortName = "IMT Business School"
    , school_longName = "IMT Business School"
    , school_id = "542824" }
  , School
    { school_shortName = "IMT Lille Douai"
    , school_longName = "IMT Lille Douai"
    , school_id = "497330" }
  , School
    { school_shortName = "IP Paris - Institut Polytechnique de Paris"
    , school_longName = "IP Paris - Institut Polytechnique de Paris"
    , school_id = "563936" }
  , School
    { school_shortName = "Mines Albi-Carmaux"
    , school_longName = "Mines Albi-Carmaux - École nationale supérieure des Mines d'Albi‐Carmaux"
    , school_id = "469216" }
  , School
    { school_shortName = "Mines Alès"
    , school_longName = "EMA - École des Mines d'Alès"
    , school_id = "6279" }
  , School
    { school_shortName = "Mines Douai"
    , school_longName = "Mines Douai EMD - École des Mines de Douai"
    , school_id = "224096" }
  , School
    { school_shortName = "Mines Lille"
    , school_longName = "Mines Lille - École des Mines de Lille"
    , school_id = "144103" }
  , School
    { school_shortName = "Mines Nantes"
    , school_longName = "Mines Nantes - Mines Nantes"
    , school_id = "84538" }
  , School
    { school_shortName = "Mines ParisTech"
    , school_longName = "MINES ParisTech - École nationale supérieure des mines de Paris"
    , school_id = "301492" }
  , School
    { school_shortName = "Mines Saint-Étienne"
    , school_longName = "Mines Saint-Étienne MSE - École des Mines de Saint-Étienne"
    , school_id = "29212" }
  , School
    { school_shortName = "Télécom Bretagne"
    , school_longName = "Télécom Bretagne"
    , school_id = "301262" }
  , School
    { school_shortName = "Télécom École de Management"
    , school_longName = "TEM - Télécom Ecole de Management"
    , school_id = "301442" }
  , School
    { school_shortName = "Télécom ParisTech"
    , school_longName = "Télécom ParisTech"
    , school_id = "300362" }
  , School
    { school_shortName = "Télécom SudParis"
    , school_longName = "TSP - Télécom SudParis"
    , school_id = "352124" }
  ]

mapIdSchool :: Map Text Text
mapIdSchool = M.fromList $ Gargantext.Prelude.map
                (\(School { school_shortName, school_id }) -> (school_id, school_shortName)) schools

hal_data :: IO (Either Prelude.String (DV.Vector CsvHal))
hal_data = do
  r <- CSV.readCsvHal "doc/corpus_imt/Gargantext_Corpus.csv"
  pure $ snd <$> r

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
                        $ Utils.freq
                        $ DL.concat
                        $ DV.toList
                        $ DV.map (\n -> splitOn ( ", ") (csvHal_instStructId_i n) )
                        $ DV.filter (\n -> csvHal_publication_year n == 2017) hal_data'
