module Data.Gargantext.Ngrams.TextMining where

import Data.Map (empty, Map, insertWith, toList)
import Data.List (foldl, foldl')
import qualified Data.List as L

sortGT :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
sortGT (a1, b1) (a2, b2)
    | a1 < a2 = GT
    | a1 > a2 = LT
    | a1 == a2 = compare b1 b2
sortGT (_, _) (_, _) = error "What is this case ?"


--histogram :: Ord a => [a] -> [(a, Int)]
--histogram = map (head &&& length) Prelude.. group Prelude.. sort Prelude.. words
--histogram = sortGT Prelude.. $ map (head &&& length) Prelude.. group Prelude.. sort Prelude.. words

countElem :: (Ord k) => Data.Map.Map k Int -> k -> Data.Map.Map k Int
countElem m e = Data.Map.insertWith (\n o -> n + o) e 1 m

freqList :: (Ord k) => [k] -> Data.Map.Map k Int
freqList = foldl countElem Data.Map.empty

getMaxFromMap :: Ord a => Map a1 a -> [a1]
getMaxFromMap m = go [] Nothing (toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

average :: [Double] -> Double
average x = L.sum x / L.genericLength x

average' :: [Int] -> Double
average' x = (L.sum y) / (L.genericLength y) where
    y = map fromIntegral x


countYear :: [Integer] -> Map Integer Integer
countYear []  = empty
countYear (x:xs) = insertWith (+) x 1 (countYear xs)

countYear' :: [Integer] -> Map Integer Integer
countYear' (xs) = foldl' (\x y -> insertWith (+) y 1 x) empty xs


textMiningMain :: IO ()
textMiningMain = do
    print $ merge ["abc"::String] ["bcd" :: String]
