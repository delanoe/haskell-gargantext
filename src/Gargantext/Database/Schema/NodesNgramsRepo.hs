{-|
Module      : Gargantext.Database.Schema.NodesNgramsRepo
Description : NodeNgram for Ngram indexation or Lists
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX


-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}


module Gargantext.Database.Schema.NodesNgramsRepo
  where

{-
import Data.Map.Strict.Patch (PatchMap)

import Gargantext.Database.Schema.Prelude
import Gargantext.API.Ngrams.Types (NgramsTablePatch)
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Prelude


data RepoDbPoly version patches
   = RepoDbNgrams { _rdp_version :: !version
                  , _rdp_patches :: !patches
                  } deriving (Show)

type RepoDbWrite
  = RepoDbPoly (Column SqlInt4)
             (Column SqlJsonb)
type RepoDbRead
  = RepoDbPoly (Column SqlInt4)
             (Column SqlJsonb)

type RepoDbNgrams = RepoDbPoly Int NgramsStatePatch
$(makeAdaptorAndInstance "pRepoDbNgrams" ''RepoDbPoly)
makeLenses ''RepoDbPoly

instance DefaultFromField SqlJsonb
                          (PatchMap NgramsType
                          (PatchMap NodeId NgramsTablePatch))
  where
    defaultFromField = fromPGSFromField

repoTable :: Table RepoDbWrite RepoDbRead
repoTable = Table "nodes_ngrams_repo"
    (pRepoDbNgrams RepoDbNgrams
                   { _rdp_version = requiredTableField "version"
                   , _rdp_patches = requiredTableField "patches"
                   }
    )
-}
