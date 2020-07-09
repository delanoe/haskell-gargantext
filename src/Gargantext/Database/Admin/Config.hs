{-|
Module      : Gargantext.Database
Description : Tools for Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Target: just import this module and nothing else to work with
Gargantext's database.

TODO: configure nodes table in Haskell (Config typenames etc.)
-}


module Gargantext.Database.Admin.Config
    where

import Control.Lens     (view)
import Data.List        (lookup)
import Data.Maybe       (fromMaybe)
import Data.Text        (Text,pack)
import Data.Tuple.Extra (swap)
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Schema.Node
import Gargantext.Prelude

-- TODO put this in config.ini file
corpusMasterName :: Text
corpusMasterName = "Main"

userMaster :: Text
userMaster = "gargantua"

userArbitrary :: Text
userArbitrary = "user1"

nodeTypeId :: NodeType -> NodeTypeId
nodeTypeId n =
  case n of
    NodeUser      -> 1
    NodeFolder    -> 2
    NodeFolderPrivate -> 20
    NodeFolderShared  -> 21
    NodeTeam          -> 210
    NodeFolderPublic  -> 22
    NodeCorpusV3  -> 3
    NodeCorpus    -> 30
    NodeAnnuaire  -> 31
    NodeTexts     -> 40
    NodeDocument  -> 4
    NodeContact   -> 41
  --NodeSwap   -> 19

----  Lists
    NodeList      -> 5
    NodeListCooc  -> 50
    NodeListModel -> 52

----  Scores
--    NodeOccurrences -> 10
    NodeGraph       -> 9
    NodePhylo       -> 90
    NodeChart       -> 7
    NodeDashboard   -> 71
    NodeNoteBook    -> 88

    NodeFrameWrite  -> 991
    NodeFrameCalc   -> 992

--  Cooccurrences -> 9
--
--  Specclusion  -> 11
--  Genclusion   -> 18
--  Cvalue       -> 12
--
--  TfidfCorpus  -> 13
--  TfidfGlobal  -> 14
--
--  TirankLocal  -> 16
--  TirankGlobal -> 17

--  Node management
--  NodeFavorites    -> 15

hasNodeType :: forall a. Node a -> NodeType -> Bool
hasNodeType n nt = (view node_typename n) == (nodeTypeId nt)

isInNodeTypes :: forall a. Node a -> [NodeType] -> Bool
isInNodeTypes n ts = elem (view node_typename n) (map nodeTypeId ts)

-- | Nodes are typed in the database according to a specific ID
--
nodeTypeInv :: [(NodeTypeId, NodeType)]
nodeTypeInv = map swap nodeTypes

nodeTypes :: [(NodeType, NodeTypeId)]
nodeTypes = [ (n, nodeTypeId n) | n <- allNodeTypes ]

fromNodeTypeId :: NodeTypeId -> NodeType
fromNodeTypeId tId = fromMaybe (panic $ pack $ "Type Id " <> show tId <> " does not exist")
                               (lookup tId nodeTypeInv)

