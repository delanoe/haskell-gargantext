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

import Data.Map.Strict.Patch (PatchMap)

import Gargantext.Database.Schema.Prelude
import Gargantext.API.Ngrams.Types (NgramsStatePatch, NgramsTablePatch)
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Database.Admin.Types.Node (NodeId)
import Gargantext.Prelude


data RepoDbPoly version patches
   = RepoDbNgrams { _rdp_version :: !version
                  , _rdp_patches :: !patches
                  } deriving (Show)

type RepoDbWrite
  = RepoDbPoly (Column PGInt4)
             (Column PGJsonb)
type RepoDbRead
  = RepoDbPoly (Column PGInt4)
             (Column PGJsonb)

type RepoDbNgrams = RepoDbPoly Int NgramsStatePatch
$(makeAdaptorAndInstance "pRepoDbNgrams" ''RepoDbPoly)
makeLenses ''RepoDbPoly

instance QueryRunnerColumnDefault PGJsonb
                          (PatchMap NgramsType
                          (PatchMap NodeId NgramsTablePatch))
  where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

repoTable :: Table RepoDbWrite RepoDbRead
repoTable = Table "nodes_ngrams_repo"
    (pRepoDbNgrams RepoDbNgrams
                   { _rdp_version = required "version"
                   , _rdp_patches = required "patches"
                   }
    )

