{-# LANGUAGE ConstraintKinds #-}
module Gargantext.Utils.Jobs.Queue where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
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

-- | Try to pop the highest priority item off of the queue, per the priorities
--   defined by the @'Map.Map' t 'Prio'@ argument to 'newQueue'.
popQueue :: Ord t => Queue t a -> IO (Maybe a)
popQueue q = go queues

  where prios = sortOn (Down . snd) $ Map.toList (queuePrios q)
        indices = flip map prios $ \(t, _prio) ->
          case Map.lookup t (queueIndices q) of
            Just i -> i
            Nothing -> error "popQueue: couldn't find queue index for given job kind"
        queues = [ queueData q Vector.! i | i <- indices ]
        go [] = return Nothing
        go (q1:qs) = do
          mitem <- atomically $ do
            qa <- readTVar q1
            case popQ qa of
              Just (a, qa') -> writeTVar q1 qa' >> return (Just a)
              Nothing       -> return Nothing
          case mitem of
            Nothing -> go qs
            a  -> return a

-- | A ready-to-use runner that pops the highest priority item off the queue
--   and processes it using the given function.
queueRunner :: Ord t => (a -> IO ()) -> Queue t a -> IO ()
queueRunner f q = go

  where go = do
          mres <- popQueue q
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
  -> (a -> IO ()) -- ^ what to do with each item
  -> IO (Queue t a, [IO ()])
newQueueWithRunners n prios f = do
  q <- newQueue prios
  let runners = replicate n (queueRunner f q)
  return (q, runners)
