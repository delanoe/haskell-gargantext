{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Implementation of EleVe Python version of papers:


-}
module Gargantext.Text.Eleve where

import Debug.Trace (trace)
import Debug.SimpleReflect

import Control.Monad (foldM)
import Data.Ord (Ord)
import qualified Data.List as L
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Gargantext.Prelude
import qualified Data.Tree as Tree
import Data.Tree (Tree)
import qualified Prelude as P (putStrLn, logBase, String)

-- prop (Node c _e f) = c == Map.size f
-- TODO maybe add Leaf
--   NP: I think Leaf is an optimisation (less data, a tiny bit more code and time)

--test = split t ts
test n example = do
  let
    ex = toToken n example
    t  = buildTrie $ chunkAlong n 1 ex

  P.putStrLn $ Tree.drawTree
               $ fmap show
               $ toTree (NonTerminal "") t

  pure $ map unToken $ split t t [] ex


example'  =  T.words "New York and New York"
example'' =  map (T.pack . pure) ("abcdefabcdegabcde" :: P.String)


data Token = NonTerminal Text
           | Terminal
  deriving (Ord, Eq, Show)

toToken :: Int -> [Text] -> [Token]
toToken n xs = (NonTerminal <$> xs) <> L.take n (repeat Terminal)

unToken :: [Token] -> [Text]
unToken = map f
  where
    f (NonTerminal x) = x
    f Terminal = ""


data Trie k e
  = Node { _node_count    :: Int
         , _node_entropy  :: e
         , _node_children :: Map k (Trie k e)
         }
 | Leaf { _node_count    :: Int }
  deriving (Show)

toTree :: k -> Trie k e -> Tree (k,Int,e)
toTree k (Node c e cs) = Tree.Node (k, c, e) (map (uncurry toTree) $ Map.toList cs)

-- emptyTrie :: Trie k e
emptyTrie :: (Ord k, Monoid e) => Int -> Trie k e
--emptyTrie n = Node n mempty mempty
emptyTrie   = Leaf

mkTrie :: Monoid e => Int -> Map k (Trie k e) -> Trie k e
mkTrie c children
{-| Map.null children = Leaf c
  | otherwise -}        = Node c mempty children

insertTrie :: Ord k => [k] -> Trie k () -> Trie k ()
insertTrie []     n                    = n { _node_count = _node_count n +1}
-- insertTrie (x:xs) (Leaf c)             = mkTrie (c+1) (Map.singleton x $ insertTrie xs emptyTrie)
insertTrie (x:xs) (Node c _e children) = mkTrie (c+1) $ Map.alter f x children
  where
    f = Just . insertTrie xs . fromMaybe (emptyTrie 0)

insertTries :: Ord k => [[k]] -> Trie k ()
insertTries = L.foldr insertTrie (emptyTrie 1)

entropyTrie :: (Num e, Floating e) => (k -> Bool) -> Trie k () -> Trie k e
-- entropyTrie _    (Leaf c)             = Leaf c
entropyTrie pred (Node c _e children) = Node c e (map (entropyTrie pred) children)
  where
    e = sum $ map f $ Map.toList children
    f (k, child) = if pred k then   cfc * P.logBase 2 (fromIntegral c)
                             else - cfc * P.logBase 2 cfc
      where
        cfc = fromIntegral (_node_count child) / fromIntegral c

normalizeEntropy :: (Fractional e, Floating e, Show e) => Trie k e -> Trie k e
-- normalizeEntropy (Leaf c)            = Leaf c
normalizeEntropy (Node c e children) =
    trace (show $ L.length es) $ Node c e $ map (normalizeLevel m v . normalizeEntropy) children
  where
    es = map _node_entropy $ Map.elems children
    m  = mean      es
    v  = deviation es

normalizeLevel :: (Fractional e, Floating e, Show e) => e -> e -> Trie k e -> Trie k e
-- normalizeLevel _ _ (Leaf c)            = Leaf c
--normalizeLevel m v n = n { _node_entropy = (_node_entropy n - m) }
normalizeLevel m v n = trace (show (_node_entropy n,m,v)) $ n { _node_entropy = (_node_entropy n - m) / v}

buildTrie :: (Floating e, Show e) => [[Token]] -> Trie Token e
buildTrie = normalizeEntropy . entropyTrie (== Terminal) . insertTries

subForest :: Trie k e -> [Trie k e]
-- subForest (Leaf _)            = []
subForest (Node _ _ children) = Map.elems children

levels :: Trie k e -> [[Trie k e]]
levels = L.takeWhile (not . L.null) . L.iterate (L.concatMap subForest) . pure

entropyLevels :: Trie k e -> [[e]]
entropyLevels = fmap (fmap _node_entropy) . levels

normalizeEntropy' :: (Floating e, Show e) => Trie k e -> Trie k e
normalizeEntropy' t = go (entropyLevels t) t
  where
    go :: (Floating e, Show e) => [[e]] -> Trie k e -> Trie k e
    go [] _ = panic "normalizeEntropy' empty levels"
    -- go _          (Leaf c)            = Leaf c
    go (es : ess) (Node c e children) =
        Node c e (normalizeLevel m v . go ess <$> children)
      where
        m  = mean      es
        v  = deviation es

buildTrie' :: (Floating e, Show e) => [[Token]] -> Trie Token e
buildTrie' = normalizeEntropy' . entropyTrie (== Terminal) . insertTries

------------------------------------------------------------------------

autonomie :: Trie Token e -> Token -> e
autonomie trie t = case (Map.lookup t (_node_children trie)) of
  Nothing -> panic $ "Gargantext.Text.Ngrams: autonomie" <> (cs $ show t)
  Just  a -> _node_entropy a

------------------------------------------------------------------------

split :: (Num e, Ord e) => Trie Token e -> Trie Token e -> [Token] -> [Token] -> [[Token]]
split _ _ pref [] = [reverse pref]
split t0 t pref (x:xs) = case Map.lookup x $ _node_children t of
  Nothing -> reverse pref : split t0 t0 [x] xs
  Just a  -> case Map.lookup x $ _node_children t0 of
    Nothing  -> panic "TODO" -- reverse pref : split t0 t0 [] xs
    Just xt0 -> case _node_entropy t + _node_entropy xt0 > _node_entropy a of
      True  -> split t0 a (x:pref) xs
      False -> reverse pref : split t0 xt0 [x] xs
