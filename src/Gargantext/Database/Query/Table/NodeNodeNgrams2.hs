{-|
Module      : Gargantext.Database.Schema.NodeNodeNgrams
Description : TODO: remove this module and table in database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.NodeNodeNgrams2
  ( module Gargantext.Database.Schema.NodeNodeNgrams2
  , insertNodeNodeNgrams2
  )
  where

import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Schema.NodeNodeNgrams2
import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Prelude (Cmd, mkCmd)
import Prelude


_queryNodeNodeNgrams2Table :: Query NodeNodeNgrams2Read
_queryNodeNodeNgrams2Table = selectTable nodeNodeNgrams2Table

-- | Insert utils
insertNodeNodeNgrams2 :: [NodeNodeNgrams2] -> Cmd err Int
insertNodeNodeNgrams2 = insertNodeNodeNgrams2W
                     . map (\(NodeNodeNgrams2 n1 n2 w) ->
                              NodeNodeNgrams2 (pgNodeId n1)
                                              (sqlInt4   n2)
                                              (sqlDouble w)
                           )

insertNodeNodeNgrams2W :: [NodeNodeNgrams2Write] -> Cmd err Int
insertNodeNodeNgrams2W nnnw =
  mkCmd $ \c -> fromIntegral <$> runInsert_ c insertNothing
    where
      insertNothing = Insert { iTable = nodeNodeNgrams2Table
                              , iRows  = nnnw
                              , iReturning = rCount
                              , iOnConflict = (Just DoNothing)
                              }
