{-|
Module      : Gargantext.Core.NodeStory
Description : Node API generation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.NodeStory where


import Codec.Serialise (Serialise())
import Control.Lens (makeLenses, makePrisms, Getter, Iso', iso, from, (.~), (?=), (#), to, folded, {-withIndex, ifolded,-} view, use, (^.), (^?), (%~), (.~), (%=), at, _Just, Each(..), itraverse_, both, forOf_, (?~))
import Data.Aeson hiding ((.=))
import Data.IntMap (IntMap)
import Data.IntMap as Bibliotheque
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

-- TODO : repo Migration
repoMigration :: (s -> s') -> (p -> p') -> Repo s p -> NodeStory s' p'
repoMigration = undefined

-- Key is NodeId
-- | Node Story for each NodeType
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
initNodeStory = NodeStory $ Map.singleton 1 initArchive

initArchive :: Monoid s => Archive s p
initArchive = Archive 1 mempty []

initNodeListStoryMock :: NodeListStory
initNodeListStoryMock = NodeStory $ Map.singleton nodeListId archive
  where
    nodeListId = 10
    archive        = Archive 1 ngramsTableMap []
    ngramsTableMap = Map.singleton TableNgrams.NgramsTerms
                   $ Map.fromList
                   [ (n ^. ne_ngrams, ngramsElementToRepo n)
                   | n <- mockTable ^. _NgramsTable
                   ]

------------------------------------------------------------------------
{-
data NodeStoryEnv = NodeStoryEnv
  { _nse_var :: !(MVar
-}

