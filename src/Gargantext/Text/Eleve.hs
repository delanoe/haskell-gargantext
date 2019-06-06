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

- Python implementation (Korantin August, Emmanuel Navarro):
  [EleVe](https://github.com/kodexlab/eleve.git)

- Unsupervized Word Segmentation:the case for Mandarin Chinese Pierre
  Magistry, Benoît Sagot, Alpage, INRIA & Univ. Paris 7, Proceedings of
  the 50th Annual Meeting of the Association for Computational Linguistics
  , pages 383–387. [PDF](https://www.aclweb.org/anthology/P12-2075)

Notes for current implementation:
- TODO fix normalization
- TODO extract longer ngrams (see paper above, viterbi algo can be used)
- TODO AD TEST: prop (Node c _e f) = c == Map.size f

- AD: Real ngrams extraction test
  from Gargantext.Text.Terms import extractTermsUnsupervised
  docs <- runCmdRepl $ selectDocs 1004
  extractTermsUnsupervised 3 $ DT.intercalate " "
                        $ catMaybes
                        $ Gargantext.map _hyperdataDocument_abstract docs

-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Gargantext.Text.Eleve where

import Debug.Trace (trace)
-- import Debug.SimpleReflect

import Control.Lens (Lens', Getting, (^.), (^?), view, makeLenses, _Just)
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
import qualified Prelude as P (putStrLn, logBase, isNaN, RealFloat)

type Entropy e =
  ( Fractional e
  , Floating e
  , P.RealFloat e
  , Show e
  -- ^ TODO: only used for debugging
  )
------------------------------------------------------------------------
-- | Example and tests for development
data I e = I
  { _info_entropy  :: e
  , _info_autonomy :: e
  }

instance Show e => Show (I e) where
  show (I e n) = show (e, n)

makeLenses ''I

type ModEntropy i o e = (e -> e) -> i -> o

set_autonomy :: ModEntropy e (I e) e
set_autonomy f e = I e (f e)

data StartStop = Start | Stop
  deriving (Ord, Eq, Show)

data Token = NonTerminal Text
           | Terminal StartStop
  deriving (Ord, Eq, Show)

isTerminal :: Token -> Bool
isTerminal (Terminal    _) = True
isTerminal (NonTerminal _) = False

parseToken :: Text -> Token
parseToken "<start>" = Terminal Start
parseToken "<stop>"  = Terminal Stop
parseToken t         = NonTerminal t

toToken :: [Text] -> [Token]
toToken xs = Terminal Start : (NonTerminal <$> xs) <> [Terminal Stop]

printToken :: Token -> Text
printToken = f
  where
    f (NonTerminal x)  = x
    f (Terminal Start) = "<start>"
    f (Terminal Stop)  = "<stop>"

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

nan :: Floating e => e
nan = 0 / 0

noNaNs :: P.RealFloat e => [e] -> [e]
noNaNs = filter (not . P.isNaN)

updateIfDefined :: P.RealFloat e => e -> e -> e
updateIfDefined e0 e | P.isNaN e = e0
                     | otherwise = e

entropyTrie :: Floating e => (k -> Bool) -> Trie k () -> Trie k e
entropyTrie _    (Leaf c)             = Leaf c
entropyTrie pred (Node c () children) = Node c e (map (entropyTrie pred) children)
  where
    e = sum $ map f $ Map.toList children
    f (k, child) = if pred k then   chc * P.logBase 2 (fromIntegral c)
                             else - chc * P.logBase 2 chc
      where
        chc = fromIntegral (_node_count child) / fromIntegral c
------------------------------------------------------------------------

normalizeLevel :: Entropy e => [e] -> e -> e
normalizeLevel = checkDiff (go . noNaNs)

  where
    -- checkDiff f es e = let e' = f es e in if e == e' then e' else trace ("normalizeLevel: diff " <> show e <> " " <> show e') e'
    checkDiff = identity
    go []  = panic "normalizeLevel: impossible"
                        -- trace "normalizeLevel"
--    go [_] = identity
    go es  = \e -> (e - m) / v
{-
                              in if P.isNaN e'
                                  then trace ("normalizeLevel " <> show (e,m,v,es))
                                      e
                                  else e'
-}
      where
        m  = mean      es
        v  = deviation es

{- Unused

nodeChildren :: Trie k e -> Map k (Trie k e)
nodeChildren (Node _ _ cs) = cs
nodeChildren (Leaf _)      = Map.empty

-}

class IsTrie trie where
  buildTrie :: Floating e => [[Token]] -> trie Token e
  nodeEntropy :: Entropy e => Getting e i e -> trie k i -> e
  nodeChild :: Ord k => k -> trie k e -> trie k e
  findTrie :: Ord k => [k] -> trie k e -> trie k e
  normalizeEntropy :: Entropy e
                   => Getting e i e -> ModEntropy i o e
                   -> trie k i -> trie k o

  nodeAutonomy :: (Ord k, Entropy e) => Getting e i e -> trie k i -> [k] -> e
  nodeAutonomy inE t ks = nodeEntropy inE $ findTrie ks t

instance IsTrie Trie where
  buildTrie = entropyTrie isTerminal . insertTries

  nodeEntropy inE (Node _ e _) = e ^. inE
  nodeEntropy _   (Leaf _)     = -- trace "nodeEntropy of Leaf" $
                                 nan

  nodeChild k (Node _ _ cs) = fromMaybe emptyTrie (Map.lookup k cs)
  nodeChild _ (Leaf _)      = emptyTrie

  findTrie ks t = L.foldl (flip nodeChild) t ks

  normalizeEntropy inE modE t = go (modE identity) (entropyLevels inE t) t
    where
      go _ []         _                   = panic "normalizeEntropy' empty levels"
      go _ _          (Leaf c)            = Leaf c
      go _ ([] : _)   _                   = panic "normalizeEntropy': empty level"
      go f (es : ess) (Node c i children) =
          Node c (f i) $ go (modE $ normalizeLevel es) ess <$> children


  {-
  This is only normalizing a node with respect to its brothers (unlike all the
  nodes of the same level).

  normalizeEntropy inE modE = go $ modE identity
    where
      go _ (Leaf c) = Leaf c
      go f (Node c i children)
        | Map.null children =
            panic "normalizeEntropy: impossible"
        | otherwise         =
            Node c (f i) $ go (modE $ normalizeLevel es) <$> children
          where
            es = [ i' ^. inE | Node _ i' _ <- Map.elems children ]
  -}
------------------------------------------------------------------------

levels :: Trie k e -> [[Trie k e]]
levels = L.takeWhile (not . L.null) . L.iterate (L.concatMap subForest) . pure
  where
    subForest :: Trie k e -> [Trie k e]
    subForest (Leaf _)            = []
    subForest (Node _ _ children) = Map.elems children

entropyLevels :: Entropy e => Getting e i e -> Trie k i -> [[e]]
entropyLevels inE = fmap (noNaNs . map (nodeEntropy inE)) . levels

------------------------------------------------------------------------

data Tries k e = Tries
  { _fwd :: Trie k e
  , _bwd :: Trie k e
  }

instance IsTrie Tries where
  buildTrie tts = Tries { _fwd = buildTrie tts
                        , _bwd = buildTrie (reverse <$> tts)
                        }

  nodeEntropy inE (Tries fwd bwd) =
    mean $ noNaNs [nodeEntropy inE fwd, nodeEntropy inE bwd]

  findTrie ks (Tries fwd bwd) = Tries (findTrie ks fwd) (findTrie (reverse ks) bwd)

  nodeChild k (Tries fwd bwd) = Tries (nodeChild k fwd) (nodeChild k bwd)

  normalizeEntropy inE modE = onTries (normalizeEntropy inE modE)

onTries :: (Trie k i -> Trie k o) -> Tries k i -> Tries k o
onTries f (Tries fwd bwd) = Tries (f fwd) (f bwd)

------------------------------------------------------------------------
split :: (IsTrie trie, Entropy e) => Lens' i e -> trie Token i -> [Token] -> [[Token]]
split _   _  [] = []
split inE t0 (Terminal Start:xs0) = split inE (nodeChild (Terminal Start) t0) xs0
split inE t0 (x0:xs0) = go (nodeChild x0 t0) [x0] xs0
  where
    consRev [] xss = xss
    consRev xs xss = reverse xs : xss

    go _ pref []                  = [reverse pref]
    go _ pref (Terminal Stop:_)   = [reverse pref]
    go t pref (Terminal Start:xs) = go t pref xs
    go t pref (x:xs) =
        -- trace (show (if acc then "ACC" else "CUT", (reverse (x : pref), ext), if acc then ">" else "<=", ((reverse pref, et), "+", ([x], ext0)))) $
        if acc
          then go xt (x:pref) xs
          else consRev pref $ go xt0 [x] xs
      where
        xt   = nodeChild x t
        xt0  = nodeChild x t0
        et   = ne 0 t
    --  ^ entropy of the current prefix
        ext0 = ne 0 xt0
    --  ^ entropy of [x]
        ext  = ne 0 xt
    --  ^ entropy of the current prefix plus x
        acc  = ext > et + ext0
        -- aut(["in","this","paper"]) > aut(["in","this"]) + aut(["paper"])

    ne d t = if P.isNaN e then d else e
      where e = nodeEntropy inE t

{-
split :: Entropy e => Lens' i e -> Tries Token i -> [Token] -> [[Token]]
split inE t0 ts =
  maximumWith (sum . map $ nodeAutonomy inE t0) (all the splits of ts)
-}

------------------------------------------------------------------------
------------------------------------------------------------------------

mainEleve :: Int -> [[Text]] -> [[[Text]]]
mainEleve _ _ = []
{-
mainEleve n input = map (map printToken) . split identity (t :: Trie Token Double) <$> inp
  where
    inp = toToken <$> input
    t   = buildTrie $ L.concat $ chunkAlong n 1 <$> inp
-}

sim :: Entropy e => e -> e -> Bool
sim x y = x == y || (P.isNaN x && P.isNaN y)

chunkAlongEleve :: Int -> [a] -> [[a]]
chunkAlongEleve n xs = L.take n <$> L.tails xs

testEleve :: e ~ Double => Bool -> Int -> [Text] -> [(Text, Int, e, e, e, e, e)] -> IO Bool
testEleve debug n output checks = do
  let
    {-
    pss = [ (ps, findTrie ps fwd ^? _Just . node_entropy) -- . info_entropy)
          | ps <- L.nub $ [ c
                          | m <- [1..n]
                          , cs <- chunkAlong m 1 <$> inp
                          , c <- cs
                          ]
          ]
    -}
  --res = map (map printToken) . split identity fwd <$> inp
  --res = map (map printToken) . split info_norm_entropy' nt' <$> inp
    res = map (map printToken) . split info_autonomy nt <$> inp
  when debug $ do
    P.putStrLn (show input)
    -- mapM_ (P.putStrLn . show) pss
    P.putStrLn ""
--    printTrie nt
    printTrie (_fwd nt)
    printTrie (_bwd nt)
    P.putStrLn $ show res
  forM_ checks checker
  pure $ expected == res

  where
    out = T.words <$> output
    expected = fmap (T.splitOn "-") <$> out
    input = (T.splitOn "-" =<<) <$> out
    inp = toToken <$> input
    t = buildTrie $ L.concat $ chunkAlongEleve (n + 2) <$> inp
    -- nt = normalizeEntropy  identity set_autonomy (fwd :: Trie Token Double)
    -- nt = normalizeEntropy' info_entropy (\f -> info_norm_entropy' %~ f) nt
    nt = normalizeEntropy identity set_autonomy t

    check f msg x y =
      if f x y
        then P.putStrLn $ "    PASS " <> msg <> " " <> show x <> " ~= " <> show y
        else P.putStrLn $ "    FAIL " <> msg <> " " <> show x <> " /= " <> show y

    checker (ngram, count, entropy, _ev, autonomy, bwd_entropy, fwd_entropy) = do
      let ns = parseToken <$> T.words ngram
          t' = findTrie ns nt
      P.putStrLn $ "  " <> T.unpack ngram <> ":"
      check (==) "count"       count       (_node_count (_fwd t'))
      check sim  "entropy"     entropy     (nodeEntropy info_entropy t')
      check sim  "autonomy"    autonomy    (nodeEntropy info_autonomy t')
      check sim  "fwd_entropy" fwd_entropy (nodeEntropy info_entropy (_fwd t'))
      check sim  "bwd_entropy" bwd_entropy (nodeEntropy info_entropy (findTrie ns (_bwd nt)))

    printTrie =
      P.putStrLn . Tree.drawTree
                 . fmap show
                 . toTree (NonTerminal "")

-- | TODO real data is a list of tokenized sentences
example0, example1, example2, example3, example4, example5, example6 :: [Text]
example0 =  ["New-York is New-York and New-York"]
example1 =  ["to-be or not to-be"]
example2 =  ["to-be-or not to-be-or NOT to-be and"]
example3 =  example0 <> example0
       -- > TEST: Should not have York New in the trie
example4 =  ["a-b-c-d e a-b-c-d f"]
example5 =  ["a-b-c-d-e f a-b-c-d-e g a-b-c-d-e"]
example6 =  ["le-petit chat"
            ,"le-petit chien"
            ,"le-petit rat"
            ,"le gros rat"
            ]

checks0, checks2 :: [(Text, Int, Double, Double, Double, Double, Double)]

checks0 =
  [("<start> New", 1, nan, nan, nan, nan, 0.0)
  ,("New York", 3, 1.584962500721156, 1.584962500721156, 1.414213562373095, nan, 1.584962500721156)
  ,("York is", 1, 0.0, nan, nan, nan, 0.0)
  ,("is New", 1, 0.0, nan, nan, nan, 0.0)
  ,("New York", 3, 1.584962500721156, 1.584962500721156, 1.414213562373095, nan, 1.584962500721156)
  ,("York and", 1, 0.0, nan, nan, nan, 0.0)
  ,("and New", 1, 0.0, nan, nan, nan, 0.0)
  ,("New York", 3, 1.584962500721156, 1.584962500721156, 1.414213562373095, nan, 1.584962500721156)
  ,("York <stop>", 1, nan, nan, nan, nan, nan)
  ]

checks2 =
  [("to be",  3, 1.2516291673878228, 1.2516291673878228, 1.5535694744293167, nan, 0.9182958340544896)
  ,("be or",  2, 0.5, nan, nan, nan, 1.0)
  ,("or not", 1, 0.0, nan, nan, nan, 0.0)
  ,("not to", 1, 0.0, nan, nan, nan, 0.0)
  ,("or NOT", 1, 0.0, nan, nan, nan, 0.0)
  ,("NOT to", 1, 0.0, nan, nan, nan, 0.0)
  ,("be and", 1, 0.0, nan, nan, nan, 0.0)
  ]


runTests :: IO ()
runTests =
  forM_
    [("example0", 2, example0, checks0)
    ,("example1", 2, example1, [])
    ,("example2", 3, example2, checks2)
    ,("example3", 2, example3, [])
    ,("example4", 4, example4, [])
    ,("example5", 5, example5, [])
    ]
    (\(name, n, ex, checks) -> do
      P.putStrLn $ name <> " " <> show n
      b <- testEleve False n ex checks
      P.putStrLn $ "  splitting: " <> if b then "PASS" else "FAIL"
    )
