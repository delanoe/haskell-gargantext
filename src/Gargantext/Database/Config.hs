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

import Data.Text  (pack)
import Data.Maybe (fromMaybe)
import Data.List (lookup)

import Gargantext.Database.Types.Node
import Gargantext.Prelude

-- | Nodes are typed in the database according to a specific ID
--
nodeTypes :: [(NodeType, NodeTypeId)]
nodeTypes = [ (NodeUser      ,  1)
            , (Folder        ,  2)
            , (NodeCorpus    ,  30)
            , (Annuaire      ,  31)
            , (Document      ,  4)
            , (UserPage      ,  41)
            --, (NodeSwap      , 19)
------  Lists
--            , (StopList      ,  5)
--            , (GroupList     ,  6)
--            , (MainList      ,  7)
--            , (MapList       ,  8)
----  Scores
            , (Occurrences   , 10)
--            , (Cooccurrences ,  9)
--
--            , (Specclusion   , 11)
--            , (Genclusion    , 18)
--            , (Cvalue       , 12)
--
--            , (TfidfCorpus  , 13)
--            , (TfidfGlobal  , 14)
--
--            , (TirankLocal  , 16)
--            , (TirankGlobal , 17)
--
----  Node management
            , (Favorites     , 15)
--
            ]
--
nodeTypeId :: NodeType -> NodeTypeId
nodeTypeId tn = fromMaybe (panic $ pack $ "Typename " <> show tn <> " does not exist")
                          (lookup tn nodeTypes)



