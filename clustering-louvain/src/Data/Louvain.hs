module Data.Louvain where

import Data.List (maximumBy,nub, intersect, scanl', foldl')
import Data.Graph.Inductive

------------------------------------------------------------------------
-- | Definitions
------------------------------------------------------------------------
type Modularity = Double
type Community = [Node] 
-- type Community' = Community { nodes :: [Node], modularity :: Maybe Modularity}
-- type Partition = [Community]

type Reverse = Bool

------------------------------------------------------------------------
-- | Partitionning the graph
------------------------------------------------------------------------
bestpartition :: (Eq b, DynGraph gr) => Reverse -> gr a b -> [[Node]]
bestpartition r gr = converge gr (start gr r)

converge :: (Eq b, DynGraph gr) => gr a1 b -> [[Node]] -> [[Node]]
converge gr ns = case stepscom gr (length ns) ns of
            ns' | ns == ns' -> ns
                | otherwise -> stepscom gr (length ns') ns'
------------------------------------------------------------------------

dendogram :: (Eq b, DynGraph gr) => gr a b -> Int -> Reverse -> [[Node]]
dendogram gr n r = stepscom gr n (start gr r)

start :: DynGraph gr => gr a b -> Reverse -> [[Node]]
start gr r = order $ Prelude.map (\x -> [] ++ [x]) ( nodes gr )
    where
        order = case r of
                  True  -> reverse
                  False -> id

------------------------------------------------------------------------
------------------------------------------------------------------------
stepscom :: (DynGraph gr, Eq b) => gr a1 b -> Int -> [[Node]] -> [[Node]]
stepscom gr n ns = foldl' (\xs _ -> stepcom gr' (smallCom xs) xs) ns [1..n]
    where
        gr' = undir gr
        smallCom xs = head $ filter (\x -> length x == minimum (Prelude.map length xs)) (reverse xs)

stepscom' :: (DynGraph gr, Eq b) => gr a1 b -> Int -> [[Node]] -> [[Node]]
stepscom' gr n ns = foldl' (\xs _ -> stepcom' gr' (smallCom xs) xs) ns [1..n]
    where
        gr' = undir gr
        smallCom xs = head $ filter (\x -> length x == minimum (Prelude.map length xs)) (reverse xs)


------------------------------------------------------------------------

stepcom' :: DynGraph gr => gr a b -> [Node] -> [[Node]] -> [[Node]]
stepcom' gr n ns = bestModularities gr $ Prelude.map (\x -> x ++ neighout) (addcom n neighin)
    where
        -- | First remove the node (n) of the current partition (ns)
        ns' = filter (/= n) ns
        neighin    = filter (\c ->  (intersect (neighcom gr n) c) /= [] ) ns'
        neighout   = filter (\c ->  (intersect (neighcom gr n) c) == [] ) ns'


stepcom :: DynGraph gr => gr a b -> [Node] -> [[Node]] -> [[Node]]
stepcom gr n ns = bestModularities gr $ [ns] ++ Prelude.map (\x -> x ++ neighout) (addcom n neighin)
    where
        -- | First remove the node (n) of the current partition (ns)
        ns' = filter (/= n) ns
        neighin    = filter (\c ->  (intersect (neighcom gr n) c) /= [] ) ns'
        neighout   = filter (\c ->  (intersect (neighcom gr n) c) == [] ) ns'

addcom :: [a] -> [[a]] -> [[[a]]]
addcom com coms = Prelude.map (\n -> addcom' com (rotate n coms)) ns
    where
        ns = [0.. fromIntegral (length coms) ]
        addcom' c cs = [com'] ++ coms''
            where
                com'   = concat $ [c] ++ (take 1 cs)
                coms'' = drop 1 cs

neighcom :: DynGraph gr => gr a b -> [Node] -> [Node]
neighcom gr ns = ( nub . filter (not . (`elem` ns)) . concat ) ns'
    where ns' = Prelude.map (neighbors gr) ns

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

------------------------------------------------------------------------
-- | Computing modularity of the partition
------------------------------------------------------------------------

modularities :: DynGraph gr => gr a b -> [[Node]] -> Double
modularities gr xs = sum $ Prelude.map (\y -> modularity gr y) xs

compareModularities :: DynGraph gr => gr a b -> [[Node]] -> [[Node]] -> Ordering
compareModularities gr xs ys
    | modularities gr xs < modularities gr ys = LT
    | modularities gr xs > modularities gr ys = GT
    | otherwise = EQ

bestModularities :: DynGraph gr => gr a b -> [[[Node]]] -> [[Node]]
bestModularities gr ns = maximumBy (compareModularities gr) ns

modularity :: DynGraph gr => gr a b -> [Node] -> Double
modularity gr ns = coverage - edgeDensity
    where
        coverage :: Double
        coverage  = sizeSubGraph / sizeAllGraph 
            where
                sizeSubGraph :: Double
                sizeSubGraph = fromIntegral ( size $ subgraph ns gr )

                sizeAllGraph :: Double
                sizeAllGraph = fromIntegral (size gr)

        edgeDensity :: Double
        edgeDensity = (sum (Prelude.map (\node -> (degree node) / links ) ns)) ** 2
            where
                degree :: Node -> Double
                degree node = fromIntegral (deg gr node)

                links :: Double
                links       = fromIntegral (2 * (size gr))

----------------------------------------------------------
-- | Discover what NP complete means:
----------------------------------------------------------

takeDrop :: Int -> [a] -> [[a]]
takeDrop n xs = [ (take n xs), drop n xs]

-- http://stackoverflow.com/questions/35388734/list-partitioning-implemented-recursively
separate :: [a] -> [[[a]]]
separate [] = [[]]
separate (x:xs) = let recur = separate xs
                      split = do
                        partition <- recur
                        return $ [x] : partition
                      noSplit = do
                        (y:ys) <- recur
                        return $ (x:y):ys
                  in split ++ noSplit


-- separate' :: forall a. [a] -> [[[a]]]
-- separate' xs = [ takeDrop t (rotate r xs) 
--                | t <- [1.. fromIntegral (length xs) - 1 ]
--                , r <- [0.. fromIntegral (length xs)     ]
--                ]


gpartition :: DynGraph gr => gr a b -> [[[Node]]]
gpartition gr = separate (nodes gr)

bestPartition' :: DynGraph gr => gr a b -> [[Node]]
bestPartition' gr = maximumBy (compareModularities gr) $ gpartition gr
----------------------------------------------------------


