{-|
Module      : Gargantext.Text.Eleve
Description : Unsupervized Word segmentation
Copyright   : (c) CNRS, 2019-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

# Implementation of Unsupervized Word Segmentation

References:
- EleVe Python implementation and discussions with Korantin August and Bruno Gaume
  [git repo](https://github.com/kodexlab/eleve.git)

- Unsupervized Word Segmentation:the case for Mandarin Chinese Pierre
  Magistry, Benoît Sagot, Alpage, INRIA & Univ. Paris 7, Proceedings of
  the 50th Annual Meeting of the Association for Computational Linguistics
  , pages 383–387. [PDF](https://www.aclweb.org/anthology/P12-2075)

Notes for current implementation:
- The node count is correct; TODO AD add tests to keep track of it
- NP fix normalization
- NP extract longer ngrams (see paper above, viterbi algo can be used)
- TODO AD TEST: prop (Node c _e f) = c == Map.size f

- AD: Real ngrams extraction test
  from Gargantext.Text.Terms import extractTermsUnsupervised
  docs <- runCmdRepl $ selectDocs 1004
  extractTermsUnsupervised 3 $ DT.intercalate " "
                        $ catMaybes
                        $ Gargantext.map _hyperdataDocument_abstract docs


-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Text.Eleve where

-- import Debug.Trace (trace)
-- import Debug.SimpleReflect

import Control.Lens (Lens', Getting, (^.), (^?), (%~), view, makeLenses, _Just)
import Control.Monad (foldM, mapM_, forM_)
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
import qualified Prelude as P (putStrLn, logBase)

------------------------------------------------------------------------
-- | Example and tests for development
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

data Token = NonTerminal Text
           | Terminal
  deriving (Ord, Eq, Show)

toToken :: Int -> [Text] -> [Token]
toToken n xs = (NonTerminal <$> xs) <> L.take n (repeat Terminal)

unToken :: [Token] -> [Text]
unToken = map f
  where
    f (NonTerminal x) = x
    f Terminal        = ""

------------------------------------------------------------------------

data Trie k e
  = Node { _node_count    :: Int
         , _node_entropy  :: e
         , _node_children :: Map k (Trie k e)
         }
 | Leaf { _node_count    :: Int }
  deriving (Show)

makeLenses ''Trie

insertTries :: Ord k => [[k]] -> Trie k ()
insertTries = L.foldr insertTrie emptyTrie

insertTrie :: Ord k => [k] -> Trie k () -> Trie k ()
insertTrie []     n                    = n { _node_count = _node_count n +1}
insertTrie (x:xs) (Leaf c)             = mkTrie (c+1) $ Map.singleton x $ insertTrie xs emptyTrie
insertTrie (x:xs) (Node c _e children) = mkTrie (c+1) $ Map.alter f x children
  where
    f = Just . insertTrie xs . fromMaybe emptyTrie

-- emptyTrie :: (Ord k, Monoid e) => Trie k e
-- emptyTrie = Node 0 mempty mempty
emptyTrie :: Trie k e
emptyTrie  = Leaf 0

mkTrie :: Monoid e => Int -> Map k (Trie k e) -> Trie k e
mkTrie c children
  | Map.null children = Leaf c
  | otherwise         = Node c mempty children

                        -----------------------------

-- | Trie to Tree since Tree as nice print function
toTree :: k -> Trie k e -> Tree (k,Int,Maybe e)
toTree k (Leaf c)      = Tree.Node (k, c, Nothing) []
toTree k (Node c e cs) = Tree.Node (k, c, Just e)  (map (uncurry toTree) $ Map.toList cs)

------------------------------------------------------------------------
------------------------------------------------------------------------

entropyTrie :: (Num e, Floating e) => (k -> Bool) -> Trie k () -> Trie k e
entropyTrie _    (Leaf c)             = Leaf c
entropyTrie pred (Node c _e children) = Node c e (map (entropyTrie pred) children)
  where
    e = sum $ map f $ Map.toList children
    f (k, child) = if pred k then   chc * P.logBase 2 (fromIntegral c)
                             else - chc * P.logBase 2 chc
      where
        chc = fromIntegral (_node_count child) / fromIntegral c

normalizeEntropy :: (Fractional e, Floating e, Show e)
                 => Getting e i e -> ModEntropy i o e -> Trie k i -> Trie k o
normalizeEntropy inE modE = go $ modE identity
  where
    go _ (Leaf c) = Leaf c
    go f (Node c i children)
      | Map.null children =
          panic "normalizeEntropy: impossible"
      | otherwise         =
          -- trace (show $ L.length es) $
          Node c (f i) $ go (modE $ normalizeLevel m v) <$> children
        where
          es = [ i' ^. inE | Node _ i' _ <- Map.elems children ]
          m  = mean      es
          v  = deviation es
------------------------------------------------------------------------

normalizeLevel :: (Fractional e, Floating e, Show e)
               => e -> e -> e -> e
normalizeLevel m v e = (e - m) / v

buildTrie :: (Floating e, Show e) => [[Token]] -> Trie Token e
buildTrie = entropyTrie (== Terminal) . insertTries

nodeEntropy :: Trie k e -> Maybe e
nodeEntropy (Node _ e _) = Just e
nodeEntropy (Leaf _)     = Nothing

nodeChildren :: Trie k e -> Map k (Trie k e)
nodeChildren (Node _ _ cs) = cs
nodeChildren (Leaf _)      = Map.empty

nodeChild :: Ord k => k -> Trie k e -> Maybe (Trie k e)
nodeChild k (Node _ _ cs) = Map.lookup k cs
nodeChild _ (Leaf _)      = Nothing

findTrie :: Ord k => [k] -> Trie k e -> Maybe (Trie k e)
findTrie ks t = foldM (flip nodeChild) t ks

levels :: Trie k e -> [[Trie k e]]
levels = L.takeWhile (not . L.null) . L.iterate (L.concatMap subForest) . pure
  where
    subForest :: Trie k e -> [Trie k e]
    subForest (Leaf _)            = []
    subForest (Node _ _ children) = Map.elems children

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
------------------------------------------------------------------------
split :: (Num e, Ord e, Show e) => Lens' i e -> Trie Token i -> [Token] -> [[Token]]
split inE t0 = go t0 []
  where
    consRev [] xss = xss
    consRev xs xss = reverse xs : xss

    go _ pref []           = [reverse pref]
    go _ pref (Terminal:_) = [reverse pref]
    go t pref (x:xs) = case nodeChild x t of
      Nothing -> consRev pref $ go t0 [x] xs
      Just xt -> case nodeChild x t0 of
        Nothing  -> panic $ "TODO"
        Just xt0 ->
          let et   = ne (panic "t")   t
          --  ^ entropy of the current prefix
              ext0 = ne (panic "xt0") xt0
          --  ^ entropy of [x]
              ext  = ne 0 xt
          --  ^ entropy of the current prefix plus x
          in
          -- trace (show ((reverse pref, et, ext0), (reverse (x : pref), ext))) $
          if ext + ext0 > et
            then go xt (x:pref) xs
            else consRev pref $ go xt0 [x] xs

    ne d t = fromMaybe d (nodeEntropy t ^? _Just . inE)

------------------------------------------------------------------------
------------------------------------------------------------------------

mainEleve :: Int -> [[Text]] -> [[[Text]]]
mainEleve n input = map unToken . split identity (t :: Trie Token Double) <$> inp
  where
    inp = toToken (n - 1) <$> input
    t   = buildTrie $ L.concat $ chunkAlong n 1 <$> inp
                          -- NP: here we use the entropy to split
                          -- instead we should use either:
                          --   info_norm_entropy or info_norm_entropy'
                          -- However they should first be fixed.

testEleve :: Bool -> Int -> [Text] -> IO Bool
testEleve debug n output = do
  let
    out = T.words <$> output
    expected = fmap (T.splitOn "-") <$> out
    input = (T.splitOn "-" =<<) <$> out
    inp = toToken (n - 1) <$> input
    t   = buildTrie $ L.concat $ chunkAlong n 1 <$> inp
    nt  = normalizeEntropy  identity setNormEntropy (t :: Trie Token Double)
    nt' = normalizeEntropy' info_entropy (\f -> info_norm_entropy' %~ f) nt
    pss = [ (ps, findTrie ps t ^? _Just . node_entropy) -- . info_entropy)
          | ps <- L.nub $ [ c
                          | m <- [1..n]
                          , cs <- chunkAlong m 1 <$> inp
                          , c <- cs
                          ]
          ]
    res = map unToken . split identity t <$> inp
  when debug $ do
    P.putStrLn (show input)
    mapM_ (P.putStrLn . show) pss
    P.putStrLn $ Tree.drawTree
              $ fmap show
              $ toTree (NonTerminal "") nt'
    P.putStrLn $ show res
  pure $ expected == res

-- | TODO real data is a list of tokenized sentences
example0, example1, example2, example3, example4, example5 :: [Text]
example0 =  ["New-York is New-York and New-York"]
example1 =  ["to-be or not to-be"]
example2 =  ["to-be-or not to-be-or NOT to-be and"]
example3 =  example0 <> example0
       -- > TEST: Should not have York New in the trie
example4 =  ["a-b-c-d e a-b-c-d f"]
example5 =  ["a-b-c-d-e f a-b-c-d-e g a-b-c-d-e"]

runTests :: IO ()
runTests =
  forM_
    [("example0", 2, example0)
    ,("example1", 2, example1)
    ,("example2", 3, example2)
    ,("example3", 2, example3)
    ,("example4", 4, example4)
    ,("example5", 5, example5)
    ]
    (\(name, n, ex) -> do
      b <- testEleve False n ex
      P.putStrLn $ name <> " " <> show n <> " " <> if b then "PASS" else "FAIL"
    )
