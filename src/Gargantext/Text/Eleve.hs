{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-

Implementation of EleVe Python version of papers:


NP:
  * The node count is correct and we should not regress on this front.
-}
module Gargantext.Text.Eleve where

import Debug.Trace (trace)
-- import Debug.SimpleReflect

import Control.Lens (Lens, Lens', ASetter, Getting, (^.), (^?), (&), (.~), (%~), view, makeLenses, _Just)
import Control.Monad (foldM)
import Data.Ord (Ord)
import qualified Data.List as L
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as Map
import Gargantext.Prelude hiding (cs)
import qualified Data.Tree as Tree
import Data.Tree (Tree)
import qualified Prelude as P (putStrLn, logBase, String)

-- prop (Node c _e f) = c == Map.size f
-- TODO maybe add Leaf
--   NP: I think Leaf is an optimisation (less data, a tiny bit more code and time)

data I e = I
  { _info_entropy       :: e
  , _info_norm_entropy  :: e
  , _info_norm_entropy' :: e
  }

instance Show e => Show (I e) where
  show (I e n n') = show (e, n, n')

makeLenses ''I

type ModEntropy i o e = (e -> e) -> i -> o

setNormEntropy :: ModEntropy e (I e) e
setNormEntropy f e = I e (f e) e -- (panic "setNormEntropy")

test n example = do
  let
    ex  = toToken n example
    t   = buildTrie $ chunkAlong n 1 ex
    nt  = normalizeEntropy  identity setNormEntropy (t :: Trie Token Double)
    nt' = normalizeEntropy' info_entropy (\f -> info_norm_entropy' %~ f) nt

  P.putStrLn $ Tree.drawTree
               $ fmap show
               $ toTree (NonTerminal "") nt'

  pure $ map unToken $ split info_entropy nt' ex
                          -- NP: here we use the entropy to split
                          -- instead we should use either:
                          --   info_norm_entropy or info_norm_entropy'
                          -- However they should first be fixed.


example0 =  T.words "New York is New York and New York"
example1 =  T.words "to be or not to be"
example2 =  T.words "to be or not to be or"
example3 =  map (T.pack . pure) ("abcdefabcdegabcde" :: P.String)


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

toTree :: k -> Trie k e -> Tree (k,Int,Maybe e)
toTree k (Leaf c)      = Tree.Node (k, c, Nothing) []
toTree k (Node c e cs) = Tree.Node (k, c, Just e)  (map (uncurry toTree) $ Map.toList cs)

-- emptyTrie :: (Ord k, Monoid e) => Trie k e
-- emptyTrie = Node 0 mempty mempty
emptyTrie :: Trie k e
emptyTrie   = Leaf 0

mkTrie :: Monoid e => Int -> Map k (Trie k e) -> Trie k e
mkTrie c children
  | Map.null children = Leaf c
  | otherwise         = Node c mempty children

insertTrie :: Ord k => [k] -> Trie k () -> Trie k ()
insertTrie []     n                    = n { _node_count = _node_count n +1}
insertTrie (x:xs) (Leaf c)             = mkTrie (c+1) $ Map.singleton x $ insertTrie xs emptyTrie
insertTrie (x:xs) (Node c _e children) = mkTrie (c+1) $ Map.alter f x children
  where
    f = Just . insertTrie xs . fromMaybe emptyTrie

insertTries :: Ord k => [[k]] -> Trie k ()
insertTries = L.foldr insertTrie emptyTrie

entropyTrie :: (Num e, Floating e) => (k -> Bool) -> Trie k () -> Trie k e
entropyTrie _    (Leaf c)             = Leaf c
entropyTrie pred (Node c _e children) = Node c e (map (entropyTrie pred) children)
  where
    e = sum $ map f $ Map.toList children
    f (k, child) = if pred k then   cfc * P.logBase 2 (fromIntegral c)
                             else - cfc * P.logBase 2 cfc
      where
        cfc = fromIntegral (_node_count child) / fromIntegral c

normalizeEntropy :: (Fractional e, Floating e, Show e)
                 => Getting e i e -> ModEntropy i o e -> Trie k i -> Trie k o
normalizeEntropy inE modE = go $ modE identity
  where
    go _ (Leaf c) = Leaf c
    go f (Node c i children) | not (Map.null children) =
        -- trace (show $ L.length es) $
        Node c (f i) $ go (modE $ normalizeLevel m v) <$> children
      where
        es = [ i' ^. inE | Node _ i' _ <- Map.elems children ]
        m  = mean      es
        v  = deviation es

normalizeLevel :: (Fractional e, Floating e, Show e)
               => e -> e -> e -> e
normalizeLevel m v e = (e - m) / v

buildTrie :: (Floating e, Show e) => [[Token]] -> Trie Token e
buildTrie = entropyTrie (== Terminal) . insertTries

subForest :: Trie k e -> [Trie k e]
subForest (Leaf _)            = []
subForest (Node _ _ children) = Map.elems children

nodeEntropy :: Trie k e -> Maybe e
nodeEntropy (Node _ e _) = Just e
nodeEntropy (Leaf _)     = Nothing

nodeChildren :: Trie k e -> Map k (Trie k e)
nodeChildren (Node _ _ cs) = cs
nodeChildren (Leaf _)      = Map.empty

nodeChild :: Ord k => k -> Trie k e -> Maybe (Trie k e)
nodeChild k (Node _ _ cs) = Map.lookup k cs
nodeChild _ (Leaf _)      = Nothing

levels :: Trie k e -> [[Trie k e]]
levels = L.takeWhile (not . L.null) . L.iterate (L.concatMap subForest) . pure

entropyLevels :: Getting e i e -> Trie k i -> [[e]]
entropyLevels inE = fmap (fmap (view inE) . catMaybes . fmap nodeEntropy) . levels

--fwd :: Getting a s a -> ASetter s t u3 a -> s -> t
--fwd inE outE s = s & outE .~ (s ^. inE)

normalizeEntropy' :: (Fractional e, Floating e, Show e)
                  => Getting e i e -> ModEntropy i o e -> Trie k i -> Trie k o
normalizeEntropy' inE modE t = go (modE identity) (entropyLevels inE t) t
  where
    go _ []         _                   = panic "normalizeEntropy' empty levels"
    go _ _          (Leaf c)            = Leaf c
    go _ ([] : _)   _                   = panic "normalizeEntropy': empty level"
    go f (es : ess) (Node c i children) =
        Node c (f i) $ go (modE $ normalizeLevel m v) ess <$> children
      where
        m  = mean      es
        v  = deviation es

------------------------------------------------------------------------

split :: (Num e, Ord e, Show e) => Lens' i e -> Trie Token i -> [Token] -> [[Token]]
split inE t0 = go t0 []
  where
    ne d t = fromMaybe d (nodeEntropy t ^? _Just . inE)
    consRev [] xss = xss
    consRev xs xss = reverse xs : xss

    go _ pref [] = [reverse pref]
    go t pref (x:xs) = case nodeChild x t of
      Nothing -> consRev pref $ go t0 [x] xs
      Just xt -> case nodeChild x t0 of
        Nothing  -> panic "TODO"
        Just xt0 ->
          let et = ne (panic "t") t
              ext0 = ne (panic "xt0") xt0
              ext = ne 0 xt
          in
          -- trace (show ((reverse pref, et, ext0), (reverse (x : pref), ext))) $
          case et {-+ ext0-} < ext of
            -- NP: here we must take ext0 in account howover currently it
            -- makes it worse.
            -- For instance it currently works well to 2-grams but not more.
            -- PASS: test 4 example1
            -- FAIL: test 4 example2
            True  -> go xt (x:pref) xs
            False -> consRev pref $ go xt0 [x] xs
