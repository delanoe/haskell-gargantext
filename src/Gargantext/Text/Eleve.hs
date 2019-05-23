{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Implementation of EleVe Python version of papers:


-}
module Gargantext.Text.Eleve where


import Data.Ord (Ord)
import qualified Data.List as L
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Gargantext.Prelude

-- prop (Node c _e f) = c == Map.size f
-- TODO remove Leaf
--   NP: I think Leaf is an optimisation (less data, a tiny bit more code and time)

example :: [[Token]]
example = map token
        $ chunkAlong 3 1
        $ T.words "New York and New York is a big apple"

data Token = NonTerminal Text | Fin
  deriving (Ord, Eq, Show)

isFin :: Token -> Bool
isFin x = case x of
  Fin   -> True
  _     -> False

token :: [Text] -> [Token]
token xs = (NonTerminal <$> xs) <> [Fin]

data Trie k e
  = Node { _node_count    :: Int
         , _node_entropy  :: e
         , _node_children :: Map k (Trie k e)
         }
-- | Leaf { _node_count    :: Int }
  deriving (Show)

-- emptyTrie :: Trie k e
-- emptyTrie = Leaf 0
emptyTrie :: (Ord k, Monoid e) => Trie k e
emptyTrie = Node 0 mempty mempty

mkTrie :: Monoid e => Int -> Map k (Trie k e) -> Trie k e
mkTrie c children
{-| Map.null children = Leaf c
  | otherwise -}        = Node c mempty children

insertTrie :: Ord k => [k] -> Trie k () -> Trie k ()
insertTrie []     n                    = n
-- insertTrie (x:xs) (Leaf c)             = mkTrie (c+1) (Map.singleton x $ insertTrie xs emptyTrie)
insertTrie (x:xs) (Node c _e children) = mkTrie (c+1) $ Map.alter f x children
  where
    f = Just . insertTrie xs . fromMaybe emptyTrie

insertTries :: Ord k => [[k]] -> Trie k ()
insertTries = L.foldr insertTrie emptyTrie

entropyTrie :: (k -> Bool) -> Trie k () -> Trie k Double
-- entropyTrie _    (Leaf c)             = Leaf c
entropyTrie pred (Node c _e children) = Node c e (entropyTrie pred <$> children)
  where
    e = sum $ f <$> Map.toList children
    f (k, child) = if pred k then cfc * log (fromIntegral c) else - cfc * log cfc
      where
        cfc = fromIntegral (_node_count child) / fromIntegral c

normalizeEntropy :: Trie k Double -> Trie k Double
-- normalizeEntropy (Leaf c)            = Leaf c
normalizeEntropy (Node c e children) =
    Node c e $ normalizeLevel m v . normalizeEntropy <$> children
  where
    es = _node_entropy <$> Map.elems children
    m  = mean es
    v  = variance es

normalizeLevel :: Double -> Double -> Trie k Double -> Trie k Double
-- normalizeLevel _ _ (Leaf c)            = Leaf c
normalizeLevel m v (Node c e children) = Node c ((e - m) / v) children

buildTrie :: [[Token]] -> Trie Token Double
buildTrie = normalizeEntropy . entropyTrie isFin . insertTries

subForest :: Trie k e -> [Trie k e]
-- subForest (Leaf _)            = []
subForest (Node _ _ children) = Map.elems children

levels :: Trie k e -> [[Trie k e]]
levels = L.takeWhile (not . L.null) . L.iterate (L.concatMap subForest) . pure

entropyLevels :: Trie k e -> [[e]]
entropyLevels = fmap (fmap _node_entropy) . levels

normalizeEntropy' :: [[Double]] -> Trie k Double -> Trie k Double
normalizeEntropy' [] _ = panic "normalizeEntropy' empty levels"
-- normalizeEntropy' _          (Leaf c)            = Leaf c
normalizeEntropy' (es : ess) (Node c e children) =
    Node c e (normalizeLevel m v . normalizeEntropy' ess <$> children)
  where
    m  = mean es
    v  = variance es
