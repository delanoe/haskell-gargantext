module Data.Utils where

import Data.Maybe 
import Data.Graph.Inductive
import Data.List (nub)


label' :: (Graph gr) => gr a b -> Edge -> Maybe b
label' gr (u,v) = lookup v (lsuc gr u)

shortest_path :: (Real b, Graph gr) => gr a b -> Node -> Node -> Maybe Path
shortest_path graph node_1 node_2= sp node_1 node_2 graph


mkGraph' :: [LEdge b] -> Gr () b
mkGraph' es = mkGraph ns es
    where
        ns :: [LNode ()]
        ns = zip [1.. (fromIntegral . length) ns'] (repeat ())
            where ns' = nub $ concat (Prelude.map edge2nodes es)

edge2nodes :: LEdge b -> [Node]
edge2nodes (a,b,_) = [a,b]



