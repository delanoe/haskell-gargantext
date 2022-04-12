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

module Gargantext.Database.Query.Table.ContextNodeNgrams
  ( module Gargantext.Database.Schema.ContextNodeNgrams
  , queryContextNodeNgramsTable
  , insertContextNodeNgrams
  )
  where

import Gargantext.Database.Admin.Types.Node (pgNodeId, pgContextId)
import Gargantext.Database.Prelude (Cmd, mkCmd)
import Gargantext.Database.Schema.Ngrams (pgNgramsTypeId)
import Gargantext.Database.Schema.ContextNodeNgrams
import Gargantext.Database.Schema.Prelude
import Prelude


queryContextNodeNgramsTable :: Query ContextNodeNgramsRead
queryContextNodeNgramsTable = selectTable contextNodeNgramsTable

-- | Insert utils
insertContextNodeNgrams :: [ContextNodeNgrams] -> Cmd err Int
insertContextNodeNgrams = insertContextNodeNgramsW
                     . map (\(ContextNodeNgrams c n ng nt w) ->
                              ContextNodeNgrams (pgContextId c)
                                                (pgNodeId n)
                                                (sqlInt4  ng)
                                                (pgNgramsTypeId nt)
                                                (sqlDouble w)
                            )

insertContextNodeNgramsW :: [ContextNodeNgramsWrite] -> Cmd err Int
insertContextNodeNgramsW nnnw =
  mkCmd $ \c -> fromIntegral <$> runInsert_ c insertNothing
    where
      insertNothing = Insert { iTable = contextNodeNgramsTable
                             , iRows  = nnnw
                             , iReturning = rCount
                             , iOnConflict = (Just DoNothing)
                             }
