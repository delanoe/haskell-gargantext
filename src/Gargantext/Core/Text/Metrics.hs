{-|
Module      : Gargantext.Core.Text.Metrics
Description : All parsers of Gargantext in one file.
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Mainly reexport functions in @Data.Text.Metrics@

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.Text.Metrics
  where

--import Data.Array.Accelerate ((:.)(..), Z(..))
--import Math.KMeans (kmeans, euclidSq, elements)
import Control.Lens (makeLenses)
import Data.Map (Map)
import Data.Monoid (Monoid, mempty)
import Data.HashMap.Strict (HashMap)
import Data.Semigroup (Semigroup)
import Gargantext.Core.Methods.Distances.Accelerate.SpeGen
import Gargantext.Core.Statistics (pcaReduceTo, Dimension(..))
import Gargantext.Core.Viz.Graph.Index
import Gargantext.Prelude
import qualified Data.Array.Accelerate as DAA
import qualified Data.Array.Accelerate.Interpreter as DAA
import qualified Data.Map  as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as Vec
import qualified Data.HashMap.Strict as HashMap


type MapListSize = Int
type InclusionSize = Int

scored :: Ord t => HashMap (t,t) Int -> V.Vector (Scored t)
scored = map2scored . (pcaReduceTo (Dimension 2)) . scored2map . Map.fromList . HashMap.toList
  where
    scored2map :: Ord t => Map (t,t) Int -> Map t (Vec.Vector Double)
    scored2map m = Map.fromList $ map (\(Scored t i s) -> (t, Vec.fromList [i,s])) $ scored' m

    map2scored :: Ord t => Map t (Vec.Vector Double) -> V.Vector (Scored t)
    map2scored = V.map (\(t, ds) -> Scored t (Vec.head ds) (Vec.last ds)) . V.fromList . Map.toList

-- TODO change type with (x,y)
data Scored ts = Scored
  { _scored_terms  :: !ts
  , _scored_genInc :: !GenericityInclusion
  , _scored_speExc :: !SpecificityExclusion
  } deriving (Show, Eq, Ord)

instance Monoid a => Monoid (Scored a) where
  mempty = Scored mempty mempty mempty

instance Semigroup a => Semigroup (Scored a) where
  (<>) (Scored a  b  c )
       (Scored _a' b' c')
      = Scored (a {-<> a'-})
               (b <> b')
               (c <> c')

localMetrics' :: Ord t => Map (t,t) Int -> Map t (Vec.Vector Double)
localMetrics' m = Map.fromList $ zipWith (\(_,t) (inc,spe) -> (t, Vec.fromList [inc,spe]))
                                         (Map.toList fi)
                                          scores
  where
    (ti, fi) = createIndices m
    (is, ss) = incExcSpeGen $ cooc2mat ti m
    scores   = DAA.toList
             $ DAA.run
             $ DAA.zip (DAA.use is) (DAA.use ss)

-- TODO Code to be removed below
-- TODO in the textflow we end up needing these indices , it might be
-- better to compute them earlier and pass them around.
scored' :: Ord t => Map (t,t) Int -> [Scored t]
scored' m = zipWith (\(_,t) (inc,spe) -> Scored t inc spe) (Map.toList fi) scores
  where
    (ti, fi) = createIndices m
    (is, ss) = incExcSpeGen $ cooc2mat ti m
    scores   = DAA.toList
             $ DAA.run
             $ DAA.zip (DAA.use is) (DAA.use ss)


normalizeGlobal :: [Scored a] -> [Scored a]
normalizeGlobal ss = map (\(Scored t s1 s2)
                     -> Scored t ((s1 - s1min) / s1max)
                                 ((s2 - s2min) / s2max)) ss
  where
    ss1 = map _scored_genInc ss
    ss2 = map _scored_speExc ss

    s1min = minimum ss1
    s1max = maximum ss1

    s2min = minimum ss2
    s2max = maximum ss2



normalizeLocal :: Scored a -> Scored a
normalizeLocal (Scored t s1 s2) = Scored t (log' 5 s1) (log' 2 s2)
  where
    log' n' x = 1 + (if x <= 0 then 0 else log $ (10^(n'::Int)) * x)



-- | Type Instances
makeLenses 'Scored
