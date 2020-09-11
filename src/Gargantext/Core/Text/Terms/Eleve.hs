{-|
Module      : Gargantext.Core.Text.Terms.Eleve
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
- TODO extract longer ngrams (see paper above, viterbi algo can be used)
- TODO AD TEST: prop (Node c _e f) = c == Map.size f

- AD: Real ngrams extraction test
  from Gargantext.Core.Text.Terms import extractTermsUnsupervised
  docs <- runCmdRepl $ selectDocs 1004
  extractTermsUnsupervised 3 $ DT.intercalate " "
                        $ catMaybes
                        $ Gargantext.map _hyperdataDocument_abstract docs

-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Gargantext.Core.Text.Terms.Eleve where

-- import Debug.Trace (trace)
-- import Debug.SimpleReflect

import Control.Lens hiding (levels, children)
import Control.Monad (forM_)
import Data.Ord (Ord)
import qualified Data.List as L
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Gargantext.Prelude hiding (cs)
import qualified Data.Tree as Tree
import Data.Tree (Tree)
import qualified Prelude as P (putStrLn, logBase, isNaN, RealFloat)

nan :: Floating e => e
nan = 0 / 0

noNaNs :: P.RealFloat e => [e] -> [e]
noNaNs = filter (not . P.isNaN)

updateIfDefined :: P.RealFloat e => e -> e -> e
updateIfDefined e0 e | P.isNaN e = e0
                     | otherwise = e

sim :: Entropy e => e -> e -> Bool
sim x y = x == y || (P.isNaN x && P.isNaN y)

subst :: Entropy e => (e, e) -> e -> e
subst (src, dst) x | sim src x = dst
                   | otherwise = x
------------------------------------------------------------------------

-- | TODO: Show Instance only used for debugging
type Entropy e =
  ( Fractional e
  , Floating e
  , P.RealFloat e
  , Show e
  )
------------------------------------------------------------------------
-- | Example and tests for development
data I e = I
  { _info_entropy    :: e
  , _info_entropy_var :: e
  , _info_autonomy   :: e
  }

instance Show e => Show (I e) where
  show (I e ev a) = show (e, ev, a)

makeLenses ''I

type ModEntropy i o e = (e -> e) -> i -> o

set_autonomy :: Entropy e => ModEntropy (I e) (I e) e
set_autonomy fe i = i & info_autonomy .~ fe (i ^. info_entropy_var)

set_entropy_var :: Entropy e => Setter e (I e) e e
set_entropy_var f e = (\ev -> I e ev nan) <$> f e

data StartStop = Start | Stop
  deriving (Ord, Eq, Show)

data Token = NonTerminal Text
           | Terminal StartStop
  deriving (Ord, Eq, Show)

isTerminal :: Token -> Bool
isTerminal (Terminal    _) = True
isTerminal (NonTerminal _) = False

nonTerminals :: [Token] -> [Text]
nonTerminals ts = [nt | NonTerminal nt <- ts]

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
normalizeLevel :: Entropy e => e -> e -> e -> e
normalizeLevel m v e = (e - m) / v

{- Unused

nodeChildren :: Trie k e -> Map k (Trie k e)
nodeChildren (Node _ _ cs) = cs
nodeChildren (Leaf _)      = Map.empty

-}

chunkAlongEleve :: Int -> [a] -> [[a]]
chunkAlongEleve n xs = L.take n <$> L.tails xs

data Direction = Backward | Forward

buildTrie :: Direction -> Int -> [[Token]] -> Trie Token ()
buildTrie d n sentences
  = L.foldr insertTrie emptyTrie
  . L.concat
  $ ( filter (/= [Terminal (term d)])
    . chunkAlongEleve (n + 1)
    . order d
    )
 <$> sentences
  where
    order Forward  = identity
    order Backward = reverse
    term  Forward  = Stop
    term  Backward = Start

class IsTrie trie where
  entropyTrie :: Entropy e => (k -> Bool) -> trie k () -> trie k e
  nodeEntropy :: Entropy e => Getting e i e -> trie k i -> e
  nodeChild   :: Ord k =>  k  -> trie k e -> trie k e
  findTrie    :: Ord k => [k] -> trie k e -> trie k e
  printTrie   :: (Show i, Entropy e) => Getting e i e -> trie Token i -> IO ()
  evTrie      :: Entropy e => Getting e i e -> Setter i o e e -> trie k i -> trie k o
  normalizeEntropy :: Entropy e
                   => Getting e i e -> ModEntropy i o e
                   -> trie k i -> trie k o

instance IsTrie Trie where

  entropyTrie _    (Leaf c)             = Leaf c
  entropyTrie pred (Node c () children) = Node c e (map (entropyTrie pred) children)
    where
      children' = Map.toList children
      sum_count = sum $ _node_count . snd <$> children'
      e | sum_count == 0 = nan
        | otherwise      = sum $ f <$> children'
      f (k, child) = if pred k then   chc * P.logBase 2 (fromIntegral c)
                              else - chc * P.logBase 2 chc
        where
          chc = fromIntegral (_node_count child) / fromIntegral c

  nodeEntropy inE (Node _ e _) = e ^. inE
  nodeEntropy _   (Leaf _)     = nan

  nodeChild k (Node _ _ cs) = fromMaybe emptyTrie (Map.lookup k cs)
  nodeChild _ (Leaf _)      = emptyTrie

  findTrie ks t = L.foldl (flip nodeChild) t ks

  printTrie inE t = do
    P.putStrLn . Tree.drawTree
                . fmap show
                $ toTree (NonTerminal "") t
    P.putStrLn "  Levels:"
    forM_ (normalizationLevels inE t) $ \level ->
      P.putStrLn $ "    " <> show level

  evTrie inE setEV = go nan
    where
      go _  (Leaf c)            = Leaf c
      go e0 (Node c i children) = Node c (i & setEV .~ ev e0 e1) $ go e1 <$> children
        where e1 = i ^. inE

      ev 0  0  = nan
      ev i0 i1 = i1 - i0

  normalizeEntropy inE modE t = go (modE identity) (normalizationLevels inE t) t
    where
      go _ _                 (Leaf c)            = Leaf c
      go _ []                _                   = panic "normalizeEntropy' empty levels"
      go f ((m, v, _) : ess) (Node c i children)
        = Node c (f i) $ go (modE $ normalizeLevel m v) ess <$> children
------------------------------------------------------------------------

levels :: Trie k e -> [[Trie k e]]
levels = L.takeWhile (not . L.null) . L.iterate (L.concatMap subForest) . pure
  where
    subForest :: Trie k e -> [Trie k e]
    subForest (Leaf _)            = []
    subForest (Node _ _ children) = Map.elems children

entropyLevels :: Entropy e => Getting e i e -> Trie k i -> [[e]]
entropyLevels inE = fmap (noNaNs . map (nodeEntropy inE)) . L.tail . levels

normalizationLevels :: Entropy e => Getting e i e -> Trie k i -> [(e, e, Int)]
normalizationLevels inE = fmap f . entropyLevels inE
  where
    f es = (mean es, deviation es, length es)

------------------------------------------------------------------------

data Tries k e = Tries
  { _fwd :: Trie k e
  , _bwd :: Trie k e
  }

makeLenses ''Tries

buildTries :: Int -> [[Token]] -> Tries Token ()
buildTries n sentences = Tries
  { _fwd = buildTrie Forward  n sentences
  , _bwd = buildTrie Backward n sentences
  }

instance IsTrie Tries where

  nodeEntropy inE (Tries f b) = mean [nodeEntropy inE f, nodeEntropy inE b]

  findTrie ks (Tries f b) = Tries (findTrie ks f) (findTrie (reverse ks) b)

  nodeChild = onTries . nodeChild

  entropyTrie = onTries . entropyTrie

  evTrie inE setEV = onTries $ evTrie inE setEV

  normalizeEntropy inE = onTries . normalizeEntropy inE

  printTrie inE (Tries f b) = do
    P.putStrLn "Forward:"
    printTrie inE f
    P.putStrLn ""
    P.putStrLn "Backward:"
    printTrie inE b

onTries :: (Trie k i -> Trie k o) -> Tries k i -> Tries k o
onTries h (Tries f b) = Tries (h f) (h b)

------------------------------------------------------------------------
mayCons :: [a] -> [[a]] -> [[a]]
mayCons [] xss = xss
mayCons xs xss = xs : xss

{-
split :: (IsTrie trie, Entropy e) => Lens' i e -> trie Token i -> [Token] -> [[Token]]
split _   _ [] = []
split inE t (Terminal Start:xs) = split inE t xs
split inE t (x0:xs0) = go [x0] xs0
  where
    go pref []                  = [pref]
    go pref (Terminal Stop:_)   = [pref]
    go _    (Terminal Start:_)  = panic "split impossible"
    go pref (x:xs) =
        -- trace (show (if acc then "ACC" else "CUT", (prefx, epxt), if acc then ">" else "<=", ((pref, ept), "+", ([x], ext)))) $
        if acc
          then go prefx xs
          else mayCons pref $ go [x] xs
      where
        prefx = pref <> [x]
        pt   = findTrie pref t
        pxt  = findTrie prefx t
        xt   = findTrie [x] t
        ept  = ne pt
    --  ^ entropy of the current prefix
        ext  = ne xt
    --  ^ entropy of [x]
        epxt = ne pxt
    --  ^ entropy of the current prefix plus x
        acc  = P.isNaN ept || P.isNaN ext || not (P.isNaN epxt) -- && (epxt > mean [ept, ext])

        -- aut(["in","this","paper"]) > aut(["in","this"]) + aut(["paper"])

    ne = nodeEntropy inE
-}

split :: Entropy e => Int -> Lens' i e -> Tries Token i -> [Token] -> [[Text]]
split _ _   _ []  = []
split _ _   _ [t] = pure <$> nonTerminals [t]
split n inE t ts  = nonTerminals pref `mayCons` split n inE t (drop (length pref) ts)
  where
    pref = maximumWith (\ks -> nodeEntropy inE $ findTrie ks t)
                       (L.tail . L.inits . take n $ ts)


{-
split :: Entropy e => Lens' i e -> Tries Token i -> [Token] -> [[Token]]
split inE t0 ts =
  maximumWith (sum . map $ nodeAutonomy inE t0) (all the splits of ts)
-}

------------------------------------------------------------------------

mainEleve :: Int -> [[Text]] -> [[[Text]]]
mainEleve n x = mainEleve' n x x

mainEleve' :: Int -> [[Text]] -> [[Text]] -> [[[Text]]]
mainEleve' n x y = mainEleveWith x' n y
  where
    x' = buildTries n (fmap toToken x)
  -- (fmap toToken i) is computed twice, since mainEleveWith is computing it too

-- | This function should take the longest possible chain of:
-- mainEleve'' n x y = maxChainSizeOf [ mainEleve' n x y
--                                    , mainEleve' n x x
--                                    , mainEleve' n y y
--                                    ]
mainEleve'' :: Int -> [[Text]] -> [[Text]] -> [[[Text]]]
mainEleve'' = undefined

mainEleveWith :: Tries Token () -> Int -> [[Text]] -> [[[Text]]]
mainEleveWith m n i = fmap (split n info_autonomy t) (fmap toToken i)
  where
    t :: Tries Token (I Double)
    t = normalizeEntropy info_entropy_var set_autonomy
      $ evTrie identity set_entropy_var
      $ entropyTrie isTerminal m

------------------------------------------------------------------------

type Checks e = [(Text, Int, e, e, e, e, e, e, e, e, e)]

testEleve :: e ~ Double => Bool -> Int -> [Text] -> Checks e -> IO Bool
testEleve debug n output checks = do
  let
    res = split (1 + n) info_autonomy nt <$> input
  when debug $ do
    P.putStrLn . show $ (printToken <$>) <$> input
    P.putStrLn ""
    printTrie info_entropy nt
    P.putStrLn ""
    P.putStrLn "Splitting:"
    P.putStrLn $ show res
  forM_ checks checker
  pure $ expected == res

  where
    out      = T.words <$> output
    expected = fmap (T.splitOn "-") <$> out
    input    = toToken . (T.splitOn "-" =<<) <$> out

    nt :: Tries Token (I Double)
    nt = normalizeEntropy info_entropy_var set_autonomy
       . evTrie identity set_entropy_var
       . entropyTrie isTerminal
       $ buildTries n input

    check f msg ref my =
      if f ref my
        then P.putStrLn $ "    \ESC[32mPASS\ESC[m " <> msg <> " " <> show ref
        else P.putStrLn $ "    \ESC[31mFAIL\ESC[m " <> msg <> " ref=" <> show ref <> " my=" <> show my

    checker (ngram, count, entropy, ev, autonomy, fwd_entropy, fwd_ev, fwd_autonomy, bwd_entropy, bwd_ev, bwd_autonomy) = do
      let ns  = parseToken <$> T.words ngram
          nt' = findTrie ns nt

      P.putStrLn $ "  " <> T.unpack ngram <> ":"
      check (==) "count"        count        (_node_count                  (_fwd nt'))

      check sim  "entropy"      entropy      (nodeEntropy info_entropy           nt' )
      check sim  "ev"           ev           (nodeEntropy info_entropy_var       nt' )
      check sim  "autonomy"     autonomy     (nodeEntropy info_autonomy          nt' )

      check sim  "fwd_entropy"  fwd_entropy  (nodeEntropy info_entropy     (_fwd nt'))
      check sim  "fwd_ev"       fwd_ev       (nodeEntropy info_entropy_var (_fwd nt'))
      check sim  "fwd_autonomy" fwd_autonomy (nodeEntropy info_autonomy    (_fwd nt'))

      check sim  "bwd_entropy"  bwd_entropy  (nodeEntropy info_entropy     (_bwd nt'))
      check sim  "bwd_ev"       bwd_ev       (nodeEntropy info_entropy_var (_bwd nt'))
      check sim  "bwd_autonomy" bwd_autonomy (nodeEntropy info_autonomy    (_bwd nt'))

-- | TODO real data is a list of tokenized sentences
example0, example1, example2, example3, example4, example5, example6, example7, example8, example9 :: [Text]
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
example7 =  ["a-b d", "a-c e", "a-c", "a-b", "a-b", "a-c", "a-c", "a-b"]
-- example8 =  ["z f", "z", "z", "z"] <> example7
example8 =  ["z", "z", "z", "z"] <> example7 <> example7 <> example7
example9 =  (T.replace "z" "a") <$> example8
--example8 =  ["a-b d", "a-c e", "a f", "a-c g", "a-b h", "a i", "a j", "a-b k", "a-c l", "a-c m", "a n", "a-b o"]

checks0, checks2, checks7, checks8, checks9 :: Checks Double

checks0 =
-- [(token, count, entropy, ev, autonomy, fwd_entropy, fwd_ev, fwd_autonomy, bwd_entropy, bwd_ev, bwd_autonomy)]
  [ ("<start>", 1, nan, nan, nan, 0.0, -2.113283334294875, -0.5000000000000002, nan, nan, nan)
  , ("New", 3, 0.792481250360578, -1.3208020839342969, 0.7499999999999999, 0.0, -2.113283334294875, -0.5000000000000002, 1.584962500721156, -0.5283208335737188, 2.0)
  , ("York", 3, 0.792481250360578, -1.3208020839342969, 0.7499999999999999, 1.584962500721156, -0.5283208335737188, 2.0, 0.0, -2.113283334294875, -0.5000000000000002)
  , ("is", 1, 0, -2.113283334294875, -0.5000000000000002, 0.0, -2.113283334294875, -0.5000000000000002, 0.0, -2.113283334294875, -0.5000000000000002)
  , ("and", 1, 0, -2.113283334294875, -0.5000000000000002, 0.0, -2.113283334294875, -0.5000000000000002, 0.0, -2.113283334294875, -0.5000000000000002)
  , ("<stop>", 0, nan, nan, nan, nan, nan, nan, 0.0, -2.113283334294875, -0.5000000000000002)
  , ("<start> New", 1, nan, nan, nan, 0.0, nan, nan, nan, nan, nan)
  , ("New York", 3, 1.584962500721156, 1.584962500721156, 1.414213562373095, 1.584962500721156, 1.584962500721156, 1.4142135623730947, 1.584962500721156, 1.584962500721156, 1.4142135623730951)
  , ("York is", 1, 0, nan, nan, 0.0, -1.584962500721156, -0.7071067811865476, 0.0, nan, nan)
  , ("is New", 1, 0, nan, nan, 0.0, nan, nan, 0.0, -1.584962500721156, -0.7071067811865474)
  , ("York and", 1, 0, nan, nan, 0.0, -1.584962500721156, -0.7071067811865476, 0.0, nan, nan)
  , ("and New", 1, 0, nan, nan, 0.0, nan, nan, 0.0, -1.584962500721156, -0.7071067811865474)
  , ("York <stop>", 1, nan, nan, nan, nan, nan, nan, 0.0, nan, nan)
  , ("<start> New York", 1, nan, nan, nan, 0.0, nan, nan, nan, nan, nan)
  , ("New York is", 1, 0, nan, nan, 0.0, -1.584962500721156, nan, 0.0, nan, nan)
  , ("York is New", 1, 0, nan, nan, 0.0, nan, nan, 0.0, nan, nan)
  , ("is New York", 1, 0, nan, nan, 0.0, nan, nan, 0.0, -1.584962500721156, nan)
  , ("New York and", 1, 0, nan, nan, 0.0, -1.584962500721156, nan, 0.0, nan, nan)
  , ("York and New", 1, 0, nan, nan, 0.0, nan, nan, 0.0, nan, nan)
  , ("and New York", 1, 0, nan, nan, 0.0, nan, nan, 0.0, -1.584962500721156, nan)
  , ("New York <stop>", 1, nan, nan, nan, nan, nan, nan, 0.0, nan, nan)
  ]

checks2 = []
{-
  [("to be",  3, 1.2516291673878228, 1.2516291673878228, 1.5535694744293167, nan, 0.9182958340544896)
  ,("be or",  2, 0.5, nan, nan, nan, 1.0)
  ,("or not", 1, 0.0, nan, nan, nan, 0.0)
  ,("not to", 1, 0.0, nan, nan, nan, 0.0)
  ,("or NOT", 1, 0.0, nan, nan, nan, 0.0)
  ,("NOT to", 1, 0.0, nan, nan, nan, 0.0)
  ,("be and", 1, 0.0, nan, nan, nan, 0.0)
  ]
-}

checks7 =
  [ ("a b", 4, 2, 1.5, 1.0106455960380136, 2, 1, 0.7302967433402215, 2, 2, 1.2909944487358056)
  , ("a c", 4, 2, 1.5, 1.0106455960380136, 2, 1, 0.7302967433402215, 2, 2, 1.2909944487358056)
  , ("a", 8, 2, -0.7139421727208477, 0.9315597394596105, 1, -1.7139421727208477, 0.1695158759052029, 3, 0.2860578272791523, 1.693603603014018)
  ]

checks8 =
  [ ("a b", 4, 2, 1.5, 1.2384061243840367, 2, 1, 0.9190418024406298, 2, 2, 1.5577704463274435)
  , ("a c", 4, 2, 1.5, 1.2384061243840367, 2, 1, 0.9190418024406298, 2, 2, 1.5577704463274435)
  , ("a", 8, 2, -1.1151193576322829, 0.8012882295122719, 1, -2.115119357632283, 1.1025957503820932e-2, 3, -0.11511935763228287, 1.5915505015207227)
  , ("z", 4, 2, -1.1151193576322829, 0.9576679529201777, 2, -1.1151193576322829, 1.0906240295212841, 2, -1.1151193576322829, 0.8247118763190712)
  ]

checks9 =
  [ ("a b", 4, 2, 0.8741854163060885, 0.9234576822288185, 2, -0.25162916738782304, 0.2891449181301934, 2, 2, 1.5577704463274435)
  , ("a c", 4, 2, 0.8741854163060885, 0.9234576822288185, 2, -0.25162916738782304, 0.2891449181301934, 2, 2, 1.5577704463274435)
  , ("a", 12, 2.91829583405449, 3.763498724462999e-2, 1.518835832034022, 2.251629167387823, -0.6290316794220367, 1.2162041043595873, 3.5849625007211565, 0.7043016539112967, 1.8214675597084569)
  ]

runTestsEleve :: Bool -> IO ()
runTestsEleve doChecks =
  forM_
    [("example0", 3, example0, checks0)
    ,("example0", 2, example0, [])
    ,("example1", 2, example1, [])
    ,("example2", 3, example2, checks2)
    ,("example3", 2, example3, [])
    ,("example4", 4, example4, [])
    ,("example5", 5, example5, [])
    ,("example6", 2, example6, [])
    ,("example7", 2, example7, checks7)
    ,("example8", 2, example8, checks8)
    ,("example9", 2, example9, checks9)
    ]
    (\(name, n, ex, checks) -> do
      P.putStrLn $ name <> " " <> show n
      b <- testEleve False n ex (if doChecks then checks else [])
      P.putStrLn $ "  splitting: " <> if b then "PASS" else "FAIL"
    )
