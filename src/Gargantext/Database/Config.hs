{-|
Module      : Gargantext.Database
Description : Tools for Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

All Database related stuff here.

Target: just import this module and nothing else to work with
Gargantext's database.

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Gargantext.Database.Config
    where


import Data.Text        (pack)
import Data.Tuple.Extra (swap)
import Data.Maybe       (fromMaybe)
import Data.List        (lookup)

import Gargantext.Database.Types.Node
import Gargantext.Prelude

nodeTypeId :: NodeType -> NodeTypeId
nodeTypeId n =
  case n of
    NodeUser      -> 1
    NodeFolder    -> 2
    NodeCorpusV3  -> 3
    NodeCorpus    -> 30
    NodeAnnuaire  -> 31
    NodeDocument  -> 4
    NodeContact   -> 41
  --NodeSwap   -> 19

----  Lists
--  StopList   -> 5
--  GroupList  -> 6
--  MainList   -> 7
--  MapList    -> 8

----  Scores
--    NodeOccurrences -> 10
    NodeGraph       -> 9
    NodeDashboard   -> 5
    NodeChart       -> 51

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


--
-- | Nodes are typed in the database according to a specific ID
--
nodeTypeInv :: [(NodeTypeId, NodeType)]
nodeTypeInv = map swap nodeTypes

nodeTypes :: [(NodeType, NodeTypeId)]
nodeTypes = [ (n, nodeTypeId n) | n <- allNodeTypes ]

typeId2node :: NodeTypeId -> NodeType
typeId2node tId = fromMaybe (panic $ pack $ "Type Id " <> show tId <> " does not exist")
                            (lookup tId nodeTypeInv)
