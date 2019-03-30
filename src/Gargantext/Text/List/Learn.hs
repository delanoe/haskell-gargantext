{-|
Module      : Gargantext.Text.List.Learn
Description : Learn to make lists
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

CSV parser for Gargantext corpus files.

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Text.List.Learn
  where

import Data.Map (Map)
import Data.Maybe (maybe)
import Gargantext.Core.Types.Main (ListType(..), listTypeId, fromListTypeId)
import Gargantext.Prelude
import Gargantext.Prelude.Utils
import Gargantext.Text.Metrics.Count (occurrencesWith)
import qualified Data.IntMap as IntMap
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.SVM as SVM
import qualified Data.Vector as Vec

------------------------------------------------------------------------
train :: Double -> Double -> SVM.Problem -> IO SVM.Model
train x y = (SVM.train (SVM.CSvc x) (SVM.RBF y))

predict :: SVM.Model -> [Vec.Vector Double] -> IO [Double]
predict m vs = mapM (predict' m) vs
  where
    predict' m' vs' = SVM.predict m' (IntMap.fromList $ (zip [1..]) $ Vec.toList vs')

------------------------------------------------------------------------
trainList :: Double -> Double -> Map ListType [Vec.Vector Double] -> IO SVM.Model
trainList x y = (train x y) . trainList'
  where
    trainList' :: Map ListType [Vec.Vector Double] -> SVM.Problem
    trainList' = mapVec2problem . (Map.mapKeys (fromIntegral . listTypeId))

    mapVec2problem :: Map Double [Vec.Vector Double] -> SVM.Problem
    mapVec2problem = List.concat . (map (\(a,as) -> zip (repeat a) as)) . Map.toList . (Map.map vecs2maps)

    vecs2maps :: [Vec.Vector Double] -> [IntMap.IntMap Double]
    vecs2maps = map (IntMap.fromList . (zip [1..]) . Vec.toList)


predictList :: SVM.Model -> [Vec.Vector Double] -> IO [Maybe ListType]
predictList m vs = map (fromListTypeId . round) <$> predict m vs

------------------------------------------------------------------------
data Model = ModelSVM { model :: SVM.Model }

instance SaveFile Model
  where
    saveFile' p (ModelSVM m) = SVM.saveModel m p

instance ReadFile Model
  where
    readFile' fp = do
      m <- SVM.loadModel fp
      pure $ ModelSVM m
------------------------------------------------------------------------
-- | TODO
-- shuffle list
-- split list : train / test
-- grid parameters on best result on test
grid :: Map ListType [Vec.Vector Double] -> IO () -- Map (ListType, Maybe ListType) Int)
grid m = do
  let
    grid' :: Double -> Double
          -> Map ListType [Vec.Vector Double]
          -> IO (Double, (Double,Double))
    grid' x y ls = do
      model' <- trainList x y ls
      fp <- saveFile (ModelSVM model')
      printDebug "file" fp
      let (res, toGuess) = List.unzip $ List.concat 
                                      $ map (\(k,vs) -> zip (repeat k) vs)
                                      $ Map.toList ls
      res' <- predictList model' toGuess
      pure (score'' $ score' $ List.zip res res', (x,y))

    {-
    score :: [(ListType, Maybe ListType)] -> Map (ListType, Maybe ListType) Int
    score = occurrencesWith identity
    -}
    
    score' :: [(ListType, Maybe ListType)] -> Map (Maybe Bool) Int
    score' = occurrencesWith (\(a,b) -> (==) <$> Just a <*> b)

    score'' :: Map (Maybe Bool) Int -> Double
    score'' m'' = maybe 0 (\t -> (fromIntegral t)/total) (Map.lookup (Just True) m'')
      where
        total = fromIntegral $ foldl (+) 0 $ Map.elems m''

  r <- List.take 10 . List.reverse
                    . (List.sortOn fst)
                   <$> mapM (\(x,y) -> grid' x y m)  [(x,y) | x <- [500..510], y <- [500..510]]

  printDebug "GRID SEARCH" r
  -- save best result


