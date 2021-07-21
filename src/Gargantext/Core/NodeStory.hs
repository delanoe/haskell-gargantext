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

import Data.IntMap (IntMap)
import qualified Data.IntMap as Dict
import Data.Map (Map)
import Data.Map as Map
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types (ListType(..), ListId, NodeId)
import Data.IntMap as Bibliotheque
import qualified Gargantext.Database.Query.Table.Ngrams as TableNgrams
import Gargantext.Prelude
import GHC.Generics (Generic)
import Data.Aeson hiding ((.=))
import Gargantext.Core.Utils.Prefix (unPrefix, unPrefixUntagged, unPrefixSwagger, wellNamedSchema)


-- TODO : repo Migration
repoMigration :: (s -> s') -> (p -> p') -> Repo s p -> NodeStory s' p'
repoMigration = undefined

-- Key is NodeId
-- | Node Story for each NodeType
data NodeStory s p = NodeStory { unNodeStory :: Map NodeId (Archive s p) }
  deriving (Generic, Show)

instance (FromJSON s, FromJSON p) => FromJSON (NodeStory s p)

data Archive s p = Archive
  { _a_version :: !Version
  , _a_state   :: !s
  , _a_history :: ![p]
    -- first patch in the list is the most recent
  }
  deriving (Generic, Show)

-- TODO Semigroup instance for unions

type NodeListStory     = NodeStory NgramsState' NgramsStatePatch'
type NgramsState'      = Map       TableNgrams.NgramsType NgramsTableMap
type NgramsStatePatch' = PatchMap  TableNgrams.NgramsType NgramsTablePatch

instance (FromJSON s, FromJSON p) => FromJSON (Archive s p) where
  parseJSON = genericParseJSON $ unPrefix "_a_"

instance (ToJSON s, ToJSON p) => ToJSON (Archive s p) where
  toJSON     = genericToJSON     $ unPrefix "_a_"
  toEncoding = genericToEncoding $ unPrefix "_a_"
