{-|
Module      : Gargantext.Core.Viz.Graph
Description : Graph utils
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell      #-}

module Gargantext.Core.Viz.Graph
  where

import Data.ByteString.Lazy as DBL (readFile, writeFile)
import Data.HashMap.Strict (HashMap, lookup)
import GHC.IO (FilePath)
import Gargantext.API.Ngrams.Types (NgramsTerm(..), NgramsRepoElement(..), mSetToList)
import Gargantext.Core.Viz.Graph.Types
import Gargantext.Database.Admin.Types.Hyperdata.Prelude
import Gargantext.Database.Schema.Ngrams (NgramsType(..))
import Gargantext.Prelude
import qualified Data.Aeson   as DA
import qualified Data.Text    as Text
import qualified Text.Read    as Text

-----------------------------------------------------------
graphV3ToGraph :: GraphV3 -> Graph
graphV3ToGraph (GraphV3 links nodes) = Graph { _graph_nodes = map nodeV32node nodes
                                             , _graph_edges = zipWith linkV32edge [1..] links
                                             , _graph_metadata = Nothing }
  where
    nodeV32node :: NodeV3 -> Node
    nodeV32node (NodeV3 no_id' (AttributesV3 cl') no_s' no_lb')
                = Node { node_size = no_s'
                       , node_type = NgramsTerms
                       , node_id = cs $ show no_id'
                       , node_label = no_lb'
                       , node_x_coord = 0
                       , node_y_coord = 0
                       , node_attributes = Attributes cl'
                       , node_children = []
                       }

    linkV32edge :: Int -> EdgeV3 -> Edge
    linkV32edge n (EdgeV3 eo_s' eo_t' eo_w') =
      Edge { edge_source = cs $ show eo_s'
           , edge_hidden = Just False
           , edge_target = cs $ show eo_t'
           , edge_weight = (Text.read $ Text.unpack eo_w') :: Double
           , edge_confluence = 0.5
           , edge_id = cs $ show n }


graphV3ToGraphWithFiles :: FilePath -> FilePath -> IO ()
graphV3ToGraphWithFiles g1 g2 = do
  -- GraphV3 <- IO Fichier
  graph <- DBL.readFile g1
  let newGraph = case DA.decode graph :: Maybe GraphV3 of
        Nothing -> panic (Text.pack "no graph")
        Just new -> new

  DBL.writeFile g2 (DA.encode $ graphV3ToGraph newGraph)

readGraphFromJson :: MonadBase IO m => FilePath -> m (Maybe Graph)
readGraphFromJson fp = do
  graph <- liftBase $ DBL.readFile fp
  pure $ DA.decode graph


-----------------------------------------------------------
mergeGraphNgrams :: Graph -> Maybe (HashMap NgramsTerm NgramsRepoElement) -> Graph
mergeGraphNgrams g Nothing = g
mergeGraphNgrams graph@(Graph { _graph_nodes }) (Just listNgrams) = set graph_nodes newNodes graph
  where
    newNodes = insertChildren <$> _graph_nodes
    insertChildren (Node { node_label, .. }) = Node { node_children = children', .. }
      where
        -- lookup (NgramsTerm node_label) in listNgrams, then fetch (NgramsRepoElement _nre_children)
        children' = case (lookup (NgramsTerm node_label) listNgrams) of
          Nothing  -> []
          Just (NgramsRepoElement { _nre_children }) -> unNgramsTerm <$> mSetToList _nre_children
