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

-- import Debug.Trace (trace)
-- import Debug.SimpleReflect

import Data.Functor.Reverse
import Control.Lens (Lens', Getting, (^.), (^?), view, makeLenses, _Just, under, reversed, at, (.~), to, set)
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
import qualified Gargantext.Prelude as GP
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
  { _info_entropy    :: e
  , _info_entropy_var :: e
  , _info_autonomy   :: e
  }

instance Show e => Show (I e) where
  show (I e v n) = show (e, v, n)

makeLenses ''I

type ModEntropy i o e = (e -> e) -> i -> o

set_autonomy :: ModEntropy e (I e) e
set_autonomy f e = I e e (f e)

set_entropy_var :: ModEntropy e (I e) e
set_entropy_var f e = I e (f e) e


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

-- |
-- >>> reverseTokens [Terminal Start, NonTerminal "new", NonTerminal "york", Terminal Stop]
-- [Terminal Start,NonTerminal "york",NonTerminal "new",Terminal Stop]
reverseTokens :: [Token] -> [Token]
reverseTokens xs = case lastMay xs of
  Nothing -> []
  Just (Terminal Stop) -> reverseTokens' xs <> [Terminal Stop]
  _                    -> reverseTokens' xs

reverseTokens' :: [Token] -> [Token]
reverseTokens' [] = []
reverseTokens' [Terminal Stop] = []
reverseTokens' [x] = [x]
reverseTokens' (x:xs) = case x of
              Terminal Start -> [Terminal Start] <> reverseTokens' xs
              _              -> reverseTokens' xs <> [x]


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

sim :: Entropy e => e -> e -> Bool
sim x y = x == y || (P.isNaN x && P.isNaN y)

subst :: Entropy e => (e, e) -> e -> e
subst (src, dst) x | sim src x = dst
                   | otherwise = x

entropyTrie :: Entropy e => (k -> Bool) -> Trie k () -> Trie k e
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
--  go []  = panic "normalizeLevel: impossible"
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
  buildTrie   :: Entropy e => (Int -> [[Text]] -> [[Token]]) -> Int -> [[Text]] -> trie Token e
  nodeEntropy :: Entropy e => Getting e i e -> trie k i -> e
  nodeChild   :: Ord k =>  k  -> trie k e -> trie k e
  findTrie    :: Ord k => [k] -> trie k e -> trie k e

-- UNUSED
--nodeAutonomy :: (Ord k, Entropy e) => Getting e i e -> trie k i -> [k] -> e
--nodeAutonomy inE t ks = nodeEntropy inE $ findTrie ks t

instance IsTrie Trie where
  buildTrie to n ts = entropyTrie isTerminal $ insertTries $ to n ts

  nodeEntropy inE (Node _ e _) = e ^. inE
  nodeEntropy _   (Leaf _)     = nan

  nodeChild k (Node _ _ cs) = fromMaybe emptyTrie (Map.lookup k cs)
  nodeChild _ (Leaf _)      = emptyTrie

  findTrie ks t = L.foldl (flip nodeChild) t ks

normalizeEntropy :: Entropy e
                 => Getting e i e -> ModEntropy i o e
                 -> Trie k i -> Trie k o
normalizeEntropy inE modE t = go (modE identity) level t
  where
    level = (entropyLevels inE t)
    go _ []         _                   = panic "normalizeEntropy' empty levels"
    go _ _          (Leaf c)            = Leaf c
--      go _ ([] : _)   _                   = panic "normalizeEntropy': empty level"
    go f (es : ess) (Node c i children)
  --  | any (sim (i ^. inE)) es
      = Node c (f i) $ go (modE $ normalizeLevel es) ess <$> children
  --  | otherwise
  --  = panic "NOT an elem"


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

makeLenses ''Tries



instance IsTrie Tries where
  buildTrie to n tts = Tries { _fwd = buildTrie to n tts
                             , _bwd = buildTrie to n (map reverse $ tts)
                             }

  nodeEntropy inE (Tries fwd bwd) =
    -- VETODO reverse the query for bwd here
    -- mean $ noNaNs [nodeEntropy inE fwd, nodeEntropy inE bwd . under reversed]
    mean $ noNaNs [nodeEntropy inE fwd, nodeEntropy inE bwd]

  findTrie ks (Tries fwd bwd) = Tries (findTrie ks fwd) (findTrie ks bwd)
  --                                                              ^^
  -- TODO: here this is tempting to reverse but this is not always what we
  -- want. See also nodeAutonomy.
  -- AD: I also tried to reverse here and I confirm getting unexpected results (whereas VETODO FIX below is ok)
  -- since recursivity of the function makes the reverse multiple times (I guess)

  nodeChild k (Tries fwd bwd) = Tries (nodeChild k fwd) (nodeChild k bwd)

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

chunkAlongEleve :: Int -> [a] -> [[a]]
chunkAlongEleve n xs = L.take n <$> L.tails xs

toToken' :: Int -> [[Text]] -> [[Token]]
toToken' n input = L.concat $ (filter (/= [Terminal Stop]) . chunkAlongEleve (n + 2)) <$> toToken <$> input

---------------------------------------------
set_entropy_vars :: Entropy e  => Getting e i e -> (e -> i -> o) -> Tries Token i -> Trie Token o
set_entropy_vars inE modE tries@(Tries fwd _bwd) =
  mapTree (\k -> modE $ entropy_var'' inE tries k) [] fwd

mapTree :: ([Token] -> t -> e) -> [Token] -> Trie Token t -> Trie Token e
mapTree f k t = go f k t
  where
    go _ _ (Leaf c)            = Leaf c
    go f k (Node c i children) = Node c (f k i) (Map.mapWithKey (\k'-> go f (k <> [k'])) children)

entropy_var'' :: Entropy e => Getting e i e -> Tries Token i -> [Token] -> e
entropy_var'' inE tries ng = mean $ noNaNs [fwd, bwd]
  where
    fwd = (nodeEntropy inE (_fwd $ findTrie                ng  tries))
    bwd = (nodeEntropy inE (_bwd $ findTrie (reverseTokens ng) tries))

---------------------------------------------
-- | TODO remove function below after following bug fixed
-- | TODO entropy_var' /= entropy_var on "<start> token.."
entropy_var' :: Entropy e => Tries Token (I e) -> [Token] -> e
entropy_var' tries ng = (mean $ noNaNs [ (nodeEntropy info_entropy (_fwd $ findTrie ng tries))
                             , (nodeEntropy info_entropy (_bwd $ findTrie (reverseTokens ng) tries))
                             ]
                        )

entropy_var :: Entropy e => [Text] -> Tries Token (I e) -> e
entropy_var ng trie = (mean [ (nodeEntropy info_entropy (_fwd $ findTrie ntf trie))
                            , (nodeEntropy info_entropy (_bwd $ findTrie ntb trie))
                            ]
                        )
  where
    ntf = parseToken <$> ng
    ntb = parseToken <$> reverse ng

---------------------------------------------

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
    -- forM_ pss (P.putStrLn . show)
    P.putStrLn ""
    P.putStrLn "Forward:"
    printTrie (_fwd t)
    P.putStrLn ""
    P.putStrLn "Backward:"
    printTrie (_bwd t)
    P.putStrLn ""
    P.putStrLn "Levels:"
    forM_ (entropyLevels identity t'') $ \level ->
      P.putStrLn $ "  " <> show level
    P.putStrLn ""
    P.putStrLn "Normalized:"
    printTrie nt
    P.putStrLn ""
    P.putStrLn "Splitting:"
    P.putStrLn $ show res
  forM_ checks checker
  pure $ expected == res

  where
    out      = T.words <$> output
    expected = fmap (T.splitOn "-") <$> out
    input    = (T.splitOn "-" =<<) <$> out
    inp      = toToken <$> input

    t :: Tries Token Double
    t        = buildTrie toToken' n input
             & bwd . node_children . at (Terminal Start) . _Just . node_entropy .~ nan
             -- TODO NP: this is a hack to set the bwd entropy of Start at NaN.

    t'' :: Trie Token Double
    t'' = set_entropy_vars identity (\e _i -> e) t

    nt :: Trie Token (I Double)
    nt = normalizeEntropy identity set_autonomy t''

    -- nt = normalizeEntropy  identity set_autonomy (fwd :: Trie Token Double)
    -- nt = normalizeEntropy' info_entropy (\f -> info_norm_entropy' %~ f) nt

    check f msg ref my =
      if f ref my
        then P.putStrLn $ "    PASS " <> msg <> " " <> show ref
        else P.putStrLn $ "    FAIL " <> msg <> " ref=" <> show ref <> " my=" <> show my

    checker (ngram, count, entropy, _ev, autonomy, bwd_entropy, fwd_entropy) = do
      let ns  = parseToken <$> T.words ngram
          nsb = parseToken <$> (reverse $ T.words ngram)
          t'  = findTrie ns t
          tvar  = findTrie ns  t''
          nt' = findTrie ns nt

      P.putStrLn $ "  " <> T.unpack ngram <> ":"
      check (==) "count"       count       (_node_count tvar)
      check sim  "entropy_var"      entropy (nodeEntropy identity tvar)
      --check sim  ("entropy_varOK")  entropy (entropy_var (T.words ngram) nt)
      --check sim  "entropy"     entropy  (entropy_var' nt (parseToken <$> T.words ngram))
      {- ^ FIXME 2 fun above should have same results (error in reverseToken):
        <start> New York:
        PASS count 1
        FAIL entropy ref=NaN my=0.0
      -}

      check sim  "autonomy"    autonomy    (nodeEntropy info_autonomy nt')
      check sim  "fwd_entropy" fwd_entropy (nodeEntropy identity (_fwd t'))
      check sim  "bwd_entropy" bwd_entropy (nodeEntropy identity (_bwd t'))

    printTrie :: Show e => Trie Token e -> IO ()
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
  [("<start>", 1, nan, nan, nan, nan, 0.0)
  ,("New", 3, 0.792481250360578, -1.3208020839342969, 0.7499999999999999, 1.584962500721156, 0.0)
  ,("York", 3, 0.792481250360578, -1.3208020839342969, 0.7499999999999999, 0.0, 1.584962500721156)
  ,("is", 1, 0.0, -2.113283334294875, -0.5000000000000002, 0.0, 0.0)
  ,("and", 1, 0.0, -2.113283334294875, -0.5000000000000002, 0.0, 0.0)
--,("<stop>", 0, nan, nan, nan, 0.0, nan) Since it is not in the trie it no,
-- need to count it.

--{-
  ,("<start> New", 1, nan, nan, nan, nan, 0.0)
  ,("New York", 3, 1.584962500721156, 1.584962500721156, 1.4142135623730951, nan, 1.584962500721156)
  ,("York is", 1, 0.0, nan, nan, nan, 0.0)
  ,("is New", 1, 0.0, nan, nan, nan, 0.0)
  ,("York and", 1, 0.0, nan, nan, nan, 0.0)
  ,("and New", 1, 0.0, nan, nan, nan, 0.0)
  ,("York <stop>", 1, nan, nan, nan, nan, nan)

  ,("<start> New York", 1, nan, nan, nan, nan, 0.0)
  ,("New York is", 1, 0.0, nan, nan, nan, 0.0)
  ,("York is New", 1, 0.0, nan, nan, nan, 0.0)
  ,("is New York", 1, 0.0, nan, nan, nan, 0.0)
  ,("New York and", 1, 0.0, nan, nan, nan, 0.0)
  ,("York and New", 1, 0.0, nan, nan, nan, 0.0)
  ,("and New York", 1, 0.0, nan, nan, nan, 0.0)
  ,("New York <stop>", 1, nan, nan, nan, nan, nan)
--}
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
