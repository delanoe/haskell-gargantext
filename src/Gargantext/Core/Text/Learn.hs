{-|
Module      : Gargantext.Core.Text.Terms.Stop
Description : Mono Terms module
Copyright   : (c) CNRS, 2017 - present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO:
- generalize to byteString
- Stop words and (how to learn it).
- Main type here is String check if Chars on Text would be optimized

-}

{-# LANGUAGE TypeSynonymInstances #-}

module Gargantext.Core.Text.Learn -- (detectLang, detectLangs, stopList)
  where

import Codec.Serialise
import qualified Data.List as DL

import Data.Map.Strict (Map, toList)
import qualified Data.Map.Strict as DM

import GHC.Generics
import Data.String (String)

import Data.Text (Text)
import Data.Text (pack, unpack, toLower)
import Data.Tuple.Extra (both)
import qualified Data.ByteString.Lazy as BSL

import Gargantext.Prelude
import Gargantext.Prelude.GargDB
import Gargantext.Core (Lang(..), allLangs)
import Gargantext.Core.Text.Terms.Mono (words)
import Gargantext.Core.Text.Metrics.Count (occurrencesWith)

import qualified Gargantext.Core.Text.Samples.FR as FR
import qualified Gargantext.Core.Text.Samples.EN as EN
--import qualified Gargantext.Core.Text.Samples.DE as DE
--import qualified Gargantext.Core.Text.Samples.SP as SP
--import qualified Gargantext.Core.Text.Samples.CH as CH

------------------------------------------------------------------------
data Candidate = Candidate { stop :: Double
                           , noStop :: Double
 } deriving (Show)

------------------------------------------------------------------------
-- * Analyze candidate
type StringSize = Int
type TotalFreq  = Int
type Freq       = Int
type Word       = String

data CatWord a = CatWord a Word
type CatProb a = Map     a Double

type Events a = Map a EventBook
------------------------------------------------------------------------
data EventBook = EventBook { events_freq :: Map String     Freq
                           , events_n    :: Map StringSize TotalFreq
                           }
                             deriving (Show, Generic)

instance Serialise EventBook

instance (Serialise a, Ord a) => SaveFile (Events a) where
  saveFile' f d = BSL.writeFile f (serialise d)

instance (Serialise a, Ord a) => ReadFile (Events a) where
  readFile' filepath = deserialise <$> BSL.readFile filepath

------------------------------------------------------------------------
detectStopDefault :: Text -> Maybe Bool
detectStopDefault = undefined

detectBool :: [(Bool, Text)] -> Text -> Maybe Bool
detectBool events = detectDefault False events

detectDefault :: Ord a => a -> [(a, Text)] -> Text -> Maybe a
detectDefault = detectDefaultWith identity

detectDefaultWith :: Ord a => (b -> Text) -> a -> [(a, b)] -> b -> Maybe a
detectDefaultWith f d events = detectDefaultWithPriors f ps
  where
    ps = priorEventsWith f d events

detectDefaultWithPriors :: Ord b => (a -> Text) -> Events b -> a -> Maybe b
detectDefaultWithPriors f priors = detectCat 99 priors . f

priorEventsWith :: Ord a => (t -> Text) -> a -> [(a, t)] -> Events a
priorEventsWith f d e = toEvents d [0..2] 10 es
  where
    es = map (\(a,b) -> CatWord a (unpack $ toLower $ f b)) e


------------------------------------------------------------------------
detectLangDefault :: Text -> Maybe Lang
detectLangDefault = detectCat 99 eventLang
  where
    eventLang :: Events Lang
    eventLang = toEvents FR [0..2] 10 [ langWord l | l <- allLangs ]

    langWord :: Lang -> CatWord Lang
    langWord l = CatWord l (textSample l)

    textSample :: Lang -> String
    textSample EN = EN.textSample
    textSample FR = FR.textSample
    textSample _  = panic "[G.C.T.L:detectLangDefault] not impl yet"
    --textSample DE = DE.textSample
    --textSample SP = SP.textSample
    --textSample CH = CH.textSample
------------------------------------------------------------------------
detectCat :: Ord a => Int -> Events a -> Text -> Maybe a
detectCat n es = head . map fst . (detectCat' n es) . unpack
  where
    detectCat' :: Ord a => Int -> Events a -> String -> [(a, Double)]
    detectCat' n' es' s =  DL.reverse $ DL.sortOn snd
                                $ toList
                                $ detectWith n' es' (wordsToBook [0..2] n' s)


    detectWith :: Ord a => Int -> Events a -> EventBook -> CatProb a
    detectWith n'' el (EventBook mapFreq _) =
      DM.unionsWith (+)
      $ map DM.fromList
      $ map (\(s,m) -> map (\(l,f) -> (l, (fromIntegral m) * f)) $ toPrior n'' s el)
      $ filter (\x -> fst x /= "  ")
      $ DM.toList mapFreq

    -- | TODO: monoids (but proba >= 0)
    toPrior :: Int -> String -> Events a -> [(a, Double)]
    toPrior n'' s el = prior n'' $ pebLang s el
      where
        pebLang :: String -> Events a -> [(a, (Freq,TotalFreq))]
        pebLang st = map (\(l,eb) -> (l, peb st eb)) .  DM.toList

        peb :: String -> EventBook -> (Freq, TotalFreq)
        peb st (EventBook mapFreq mapN) = (fromIntegral a, fromIntegral b)
          where
            a = maybe 0 identity $ DM.lookup st mapFreq
            b = maybe 1 identity $ DM.lookup (length st) mapN


    prior :: Int -> [(a, (Freq, TotalFreq))] -> [(a, Double)]
    prior i ps = zip ls $ zipWith (\x y -> x^i * y) (map (\(a,_) -> part a (sum $ map fst ps')) ps')
                                    (map (\(a,b) -> a / b) ps')
      where
        (ls, ps'') = DL.unzip ps
        ps' = map (both fromIntegral) ps''

    part :: (Eq p, Fractional p) => p -> p -> p
    part 0 _ = 0
    part _ 0 = 0
    part x y = x / y

{-
toProba :: (Eq b, Fractional b, Functor t, Foldable t) =>
                 t (a, b) -> t (a, b)
toProba xs = map (\(a,b) -> (a, part b total)) xs
  where
    total = sum $ map snd xs
-}
-- | TODO: monoids
toEvents :: Ord a => a -> [Int] -> Int -> [CatWord a] -> Events a
toEvents e ns n = foldl' (opEvent (+)) (emptyEvent e ns n) . map (toEvent ns n)
  where
    emptyEvent :: Ord a => a -> [Int] -> Int -> Events a
    emptyEvent e' ns' n'= toEvent ns' n' (CatWord e' "")

    toEvent :: Ord a => [Int] -> Int -> CatWord a -> Events a
    toEvent ns'' n'' (CatWord l txt) = DM.fromList [(l, wordsToBook ns'' n'' txt)]

    opEvent :: Ord a => (Freq -> Freq -> Freq) -> Events a -> Events a -> Events a
    opEvent f = DM.unionWith (op f)

------------------------------------------------------------------------

emptyEventBook :: [Int] -> Int -> EventBook
emptyEventBook ns n = wordToBook ns n " "

wordsToBook :: [Int] -> Int -> String -> EventBook
wordsToBook ns n txt = foldl' (op (+)) (emptyEventBook ns n) eventsBook
  where
    ws = map unpack $ words $ pack txt
    eventsBook = map (wordToBook ns n) ws

wordToBook :: [Int] -> Int -> Word -> EventBook
wordToBook ns n txt = EventBook ef en
  where
    chks = allChunks ns n txt
    en = DM.fromList $ map (\(n',ns') -> (n', length ns')) $ zip ns chks
    ef = foldl' DM.union DM.empty $ map (occurrencesWith identity) chks

op :: (Freq -> Freq -> Freq) -> EventBook -> EventBook -> EventBook
op f (EventBook ef1 en1)
     (EventBook ef2 en2) = EventBook (DM.unionWith f ef1 ef2)
                                     (DM.unionWith f en1 en2)

------------------------------------------------------------------------
------------------------------------------------------------------------
allChunks :: [Int] -> Int -> String -> [[String]]
allChunks ns m st = map (\n -> chunks n m st) ns

-- | Chunks is the same function as splitBy in Context but for Strings,
-- not Text (without pack and unpack operations that are not needed).
chunks :: Int -> Int -> String -> [String]
chunks n m = DL.take m . filter (not . all (== ' '))
                       . chunkAlong (n+1) 1
                       . DL.concat
                       . DL.take 1000
                       . DL.repeat
                       . blanks

-- | String preparation
blanks :: String -> String
blanks [] = []
blanks xs = [' '] <> xs <> [' ']


{-
-- Some previous tests to be removed
--import GHC.Base (Functor)
--import Numeric.Probability.Distribution ((??))
--import qualified Numeric.Probability.Distribution as D

-- | Blocks increase the size of the word to ease computations
-- some border and unexepected effects can happen, need to be tested
blockOf :: Int -> String -> String
blockOf n = DL.concat . DL.take n . DL.repeat

-- * Make the distributions
makeDist :: [String] -> D.T Double String
makeDist = D.uniform . DL.concat . map (DL.concat . allChunks [0,2] 10)

stopDist :: D.T Double String
stopDist = makeDist $ map show ([0..9]::[Int]) <> EN.stopList

candDist :: D.T Double String
candDist = makeDist candList

------------------------------------------------------------------------
sumProba :: Num a => D.T a String -> [Char] -> a
sumProba ds x = sum $ map ((~?) ds) $ DL.concat $ allChunks [0,2] 10 $ map toLower x

-- | Get probability according a distribution
(~?) :: (Num prob, Eq a) => D.T prob a -> a -> prob
(~?) ds x = (==x) ?? ds

------------------------------------------------------------------------
candidate :: [Char] -> Candidate
candidate x = Candidate (sumProba stopDist x) (sumProba candDist x)

------------------------------------------------------------------------
candList :: [String]
candList = [ "france", "alexandre", "mael", "constitution"
           , "etats-unis", "associes", "car", "train", "spam"]

--}
