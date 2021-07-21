{-|
Module      : Gargantext.Core.NodeStory
Description : Node API generation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Core.NodeStory where

import Data.Maybe (fromMaybe)
import Codec.Serialise (Serialise())
import System.FileLock (FileLock)
import Control.Concurrent (MVar())
import Control.Lens (makeLenses, makePrisms, Getter, Iso', iso, from, (.~), (?=), (#), to, folded, {-withIndex, ifolded,-} view, use, (^.), (^?), (%~), (.~), (%=), at, _Just, Each(..), itraverse_, both, forOf_, (?~))
import Data.Aeson hiding ((.=))
import Data.IntMap (IntMap)
import Data.IntMap as Bibliotheque
import qualified Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Monoid
import GHC.Generics (Generic)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types (ListType(..), ListId, NodeId)
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixUntagged, unPrefixSwagger, wellNamedSchema)
import Gargantext.Prelude
import qualified Data.IntMap as Dict
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams
import qualified Data.Map.Strict.Patch.Internal as Patch


------------------------------------------------------------------------
-- TODO : repo Migration TODO TESTS
repoMigration :: NgramsRepo -> NodeListStory
repoMigration (Repo _v s h) = NodeStory $ Map.fromList ns
  where
    s' = ngramsState_migration      s
    h' = ngramsStatePatch_migration h
    ns = List.map (\(n,ns)
                    -> (n, let hs = fromMaybe [] (Map.lookup n h') in
                               Archive (List.length hs) ns hs
                       )
                  ) s'

ngramsState_migration :: NgramsState
                      -> [(NodeId,NgramsState')]
ngramsState_migration ns =
    [ (nid, Map.singleton nt table)
    | (nt, nTable) <- Map.toList ns
    , (nid, table) <- Map.toList nTable
    ]

ngramsStatePatch_migration :: [NgramsStatePatch]
                           -> Map NodeId [NgramsStatePatch']
ngramsStatePatch_migration np' = Map.fromListWith (<>)
    [ (nid, [fst $ Patch.singleton nt table])
    | np <- np'
    , (nt, nTable) <- Patch.toList np
    , (nid, table) <- Patch.toList nTable
    ]
------------------------------------------------------------------------


{- | Node Story for each NodeType where the Key of the Map is NodeId
  TODO : generalize for any NodeType, let's start with NodeList which
  is implemented already
-}
data NodeStory s p = NodeStory { unNodeStory :: Map NodeId (Archive s p) }
  deriving (Generic, Show)

instance (FromJSON s, FromJSON p) => FromJSON (NodeStory s p)
instance (ToJSON s, ToJSON p) => ToJSON (NodeStory s p)
instance (Serialise s, Serialise p) => Serialise (NodeStory s p)

data Archive s p = Archive
  { _a_version :: !Version
  , _a_state   :: !s
  , _a_history :: ![p]
    -- first patch in the list is the most recent
  }
  deriving (Generic, Show)

instance (Serialise s, Serialise p) => Serialise (Archive s p)

-- TODO Semigroup instance for unions

type NodeListStory     = NodeStory NgramsState' NgramsStatePatch'

type NgramsState'      = Map       TableNgrams.NgramsType NgramsTableMap
type NgramsStatePatch' = PatchMap  TableNgrams.NgramsType NgramsTablePatch

instance (FromJSON s, FromJSON p) => FromJSON (Archive s p) where
  parseJSON = genericParseJSON $ unPrefix "_a_"

instance (ToJSON s, ToJSON p) => ToJSON (Archive s p) where
  toJSON     = genericToJSON     $ unPrefix "_a_"
  toEncoding = genericToEncoding $ unPrefix "_a_"

------------------------------------------------------------------------
initNodeStory :: Monoid s => NodeStory s p
initNodeStory = NodeStory $ Map.singleton 0 initArchive

initArchive :: Monoid s => Archive s p
initArchive = Archive 0 mempty []

initNodeListStoryMock :: NodeListStory
initNodeListStoryMock = NodeStory $ Map.singleton nodeListId archive
  where
    nodeListId = 10
    archive        = Archive 0 ngramsTableMap []
    ngramsTableMap = Map.singleton TableNgrams.NgramsTerms
                   $ Map.fromList
                   [ (n ^. ne_ngrams, ngramsElementToRepo n)
                   | n <- mockTable ^. _NgramsTable
                   ]

------------------------------------------------------------------------
data NodeStoryEnv = NodeStoryEnv
  { _nse_var :: !(IO (MVar NodeListStory))
  , _nse_saver :: !(IO ())
  , _nse_lock  :: !FileLock
  }
  deriving (Generic)

makeLenses ''NodeStoryEnv

class HasNodeStoryEnv env where
  nodeStoryEnv :: env -> IO (MVar NodeListStory)

instance HasNodeStoryEnv (MVar NodeListStory) where
  nodeStoryEnv = pure


class HasNodeStorySaver env where
  nodeStorySaver :: Getter env (IO ())


