{-|
Module      : Gargantext.Database.Schema.NodeNodeNgrams
Description : TODO: remove this module and table in database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-#  OPTIONS_GHC -fno-warn-orphans  #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Query.Table.NodeNodeNgrams
  ( module Gargantext.Database.Schema.NodeNodeNgrams
  , queryNodeNodeNgramsTable
  , insertNodeNodeNgrams
  )
  where

import Gargantext.Database.Admin.Types.Node (pgNodeId)
import Gargantext.Database.Prelude (Cmd, mkCmd)
import Gargantext.Database.Schema.Ngrams (pgNgramsTypeId)
import Gargantext.Database.Schema.NodeNodeNgrams
import Gargantext.Database.Schema.Prelude
import Prelude


queryNodeNodeNgramsTable :: Query NodeNodeNgramsRead
queryNodeNodeNgramsTable = selectTable nodeNodeNgramsTable

-- | Insert utils
insertNodeNodeNgrams :: [NodeNodeNgrams] -> Cmd err Int
insertNodeNodeNgrams = insertNodeNodeNgramsW
                     . map (\(NodeNodeNgrams n1 n2 ng nt w) ->
                              NodeNodeNgrams (pgNodeId n1)
                                             (pgNodeId n2)
                                             (sqlInt4   ng)
                                             (pgNgramsTypeId nt)
                                             (sqlDouble w)
                                                  )

insertNodeNodeNgramsW :: [NodeNodeNgramsWrite] -> Cmd err Int
insertNodeNodeNgramsW nnnw =
  mkCmd $ \c -> fromIntegral <$> runInsert_ c insertNothing
    where
      insertNothing = (Insert { iTable = nodeNodeNgramsTable
                              , iRows  = nnnw
                              , iReturning = rCount
                              , iOnConflict = (Just DoNothing)
      })

