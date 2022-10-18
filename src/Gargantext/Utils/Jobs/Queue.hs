{-# LANGUAGE ConstraintKinds, TypeFamilies, ScopedTypeVariables #-}
module Gargantext.Utils.Jobs.Queue where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Function
import Data.List
import Data.Ord
import Data.Maybe
import Prelude
import System.IO

import qualified Data.Map as Map
import qualified Data.Vector as Vector

type EnumBounded t = (Ord t, Enum t, Bounded t)

data Q a = Q [a] [a] !Int

emptyQ :: Q a
emptyQ = Q [] [] 0

singletonQ :: a -> Q a
singletonQ a = Q [a] [] 1

snocQ :: a -> Q a -> Q a
snocQ a (Q xs ys sz) = Q xs (a:ys) (sz+1)

normalizeQ :: Q a -> Q a
normalizeQ (Q [] ys sz) = Q (reverse ys) [] sz
normalizeQ q            = q

deleteQ :: Eq a => a -> Q a -> Q a
deleteQ x (Q xs ys sz) = Q xs' ys' sz'
  where (xs_num_x, xs') = go xs (0, [])
        (ys_num_x, ys') = go ys (0, [])
        sz'             = sz - xs_num_x - ys_num_x

        go [] (n, bs) = (n, reverse bs)
        go (a:as) (n, bs)
          | a == x = go as (n+1, bs)
          | otherwise = go as (n, a:bs)

popQ :: Q a -> Maybe (a, Q a)
popQ q@(Q as bs sz) = case as of
  x:xs -> Just (x, Q xs bs (sz-1))
  _    -> case normalizeQ q of
    Q (x:xs) ys sz' -> Just (x, Q xs ys (sz'-1))
    _               -> Nothing

sizeQ :: Q a -> Int
sizeQ (Q _ _ sz) = sz

peekQ :: Q a -> Maybe a
peekQ (Q _ _ 0) = Nothing
peekQ q = case normalizeQ q of
  Q (x:_) _ _ -> Just x
  _           -> Nothing

dropQ :: Q a -> Q a
dropQ (Q [] [] _) = Q [] [] 0
dropQ (Q (_x:xs) ys sz) = Q xs ys (sz-1)
dropQ q@(Q [] _ _) = dropQ (normalizeQ q)

-- | A priority is just a number. The greater, the earlier the job will get picked.
type Prio = Int

applyPrios
  :: Ord t
  => [(t, Prio)] -> Map.Map t Prio -> Map.Map t Prio
applyPrios changes prios = foldl' (\m (t, p) -> Map.insert t p m) prios changes

-- | A queue with different kinds of values, described by @t@, where each
--   kind can have a higher or lower priority than other kinds, as described
--   by the 'queuePrios' field.
data Queue t a = Queue
  { queueData    :: Vector.Vector (TVar (Q a))
  , queueIndices :: Map.Map t Int -- indices into queueData
  , queuePrios   :: Map.Map t Prio
  }

-- | Default priorities for the enumeration of job types @t@: everyone at 0.
defaultPrios :: EnumBounded t => Map.Map t Prio
defaultPrios = Map.fromList [ (t, 0) | t <- [minBound..maxBound] ]

-- | Create a new queue that'll apply the given priorities
newQueue :: EnumBounded t => Map.Map t Prio -> IO (Queue t a)
newQueue prios = do
  let allTs = [ minBound .. maxBound ]
      indices = Map.fromList (zip allTs [0..])
      n = Map.size indices
  vars <- Vector.replicateM n (newTVarIO emptyQ)
  return $ Queue vars indices prios

-- | Add a new element to the queue, with the given kind.
addQueue :: Ord t => t -> a -> Queue t a -> IO ()
addQueue jobkind a q = case Map.lookup jobkind (queueIndices q) of
  Just i -> atomically $ modifyTVar (queueData q Vector.! i) (snocQ a)
  Nothing -> error "addQueue: couldn't find queue for given job kind"

deleteQueue :: (Eq a, Ord t) => t -> a -> Queue t a -> STM ()
deleteQueue jobkind a q = case Map.lookup jobkind (queueIndices q) of
  Just i -> modifyTVar (queueData q Vector.! i) (deleteQ a)
  Nothing -> error "deleteQueue: queue type not found?!"


type Picker a = [(a, STM ())] -> STM (a, STM ())

-- | Figure out the candidates for being popped from the various queues.
--   We always look at highest priority queues first, and will pick between
--   equal priority items of different queues (candidates, elements of the
--   returned lists) by choosing the one that was queued first.
popQueue :: forall a t. Ord t => Picker a -> Queue t a -> IO (Maybe a)
popQueue picker q = atomically $ select prioLevels

  where -- TODO: cache this in the 'Queue' data structure?
        prioLevels :: [[(t, Prio)]]
        prioLevels = groupBy ((==) `on` snd) . sortOn (Down . snd) $
          Map.toList (queuePrios q)

        select :: [[(t, Prio)]] -> STM (Maybe a)
        select [] = return Nothing
        select (level:levels) = do
          mres <- selectLevel level
          case mres of
            Nothing  -> select levels
            Just res -> return (Just res)

        selectLevel :: [(t, Prio)] -> STM (Maybe a)
        selectLevel xs = do
          let indices = catMaybes $ map (flip Map.lookup (queueIndices q) . fst) xs
              queues  = map (queueData q Vector.!) indices
              go qvar = readTVar qvar >>= \qu ->
                return (peekQ qu, modifyTVar' qvar dropQ)
          mtopItems <- catMaybesFst <$> traverse go queues
          case mtopItems of
            Nothing -> return Nothing
            Just [] -> return Nothing
            Just topItems -> do
              (earliestItem, popItem) <- picker topItems
              popItem
              return (Just earliestItem)

        catMaybesFst ((Nothing, _b) : xs) = catMaybesFst xs
        catMaybesFst ((Just a, b) : xs) = ((a, b) :) <$> catMaybesFst xs
        catMaybesFst [] = Just []

-- | A ready-to-use runner that pops the highest priority item off the queue
--   and processes it using the given function.
queueRunner :: Ord t => Picker a -> (a -> IO ()) -> Queue t a -> IO ()
queueRunner picker f q = go

  where go = do
          mres <- popQueue picker q
          case mres of
            Just a -> f a `catch` exc
            Nothing -> return ()
          threadDelay 5000 -- 5ms
          go

        exc :: SomeException -> IO ()
        exc e = hPutStrLn stderr ("Queue runner exception: " ++ show e)

-- | Create a queue and @n@ runner actions for it, with the given priorities
--   for the runners to apply when picking a new item.
newQueueWithRunners
  :: EnumBounded t
  => Int -- ^ number of runners
  -> Map.Map t Prio -- ^ priorities
  -> Picker a  -- ^ how to pick between equal priority items
  -> (a -> IO ()) -- ^ what to do with each item
  -> IO (Queue t a, [IO ()])
newQueueWithRunners n prios picker f = do
  q <- newQueue prios
  let runners = replicate n (queueRunner picker f q)
  return (q, runners)
