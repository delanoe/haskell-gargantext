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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}


module Gargantext.Database.Admin.Schema.NodesNgramsRepo
  where

import Control.Arrow (returnA)
import Control.Lens.TH (makeLenses)
import Data.Map.Strict.Patch (PatchMap)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Gargantext.API.Ngrams (NgramsStatePatch, NgramsTablePatch)
import Gargantext.Database.Schema.Ngrams (NgramsType)
import Gargantext.Database.Types.Node (NodeId)
import Gargantext.Database.Utils (mkCmd, Cmd, runOpaQuery)
import Gargantext.Prelude
import Opaleye


data RepoDbPoly version patches
   = RepoDbNgrams { _rdp_version :: version
              , _rdp_patches :: patches
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

-- type Re
repoTable :: Table RepoDbWrite RepoDbRead
repoTable = Table "nodes_ngrams_repo"
    (pRepoDbNgrams RepoDbNgrams
                   { _rdp_version = required "version"
                   , _rdp_patches = required "patches"
                   }
    )


selectRepo :: Cmd err [RepoDbNgrams]
selectRepo =  runOpaQuery selectPatches

selectPatches :: Query RepoDbRead
selectPatches = proc () -> do
  repos <- queryTable repoTable -< ()
  returnA -< repos


insertRepos :: [NgramsStatePatch] -> Cmd err Int64
insertRepos ns = mkCmd $ \conn -> runInsert_ conn $ Insert repoTable (toWrite ns) rCount Nothing
  where
    toWrite :: [NgramsStatePatch] -> [RepoDbWrite]
    toWrite = undefined
    --ns' = map (\(RepoDbNgrams v ps) -> RepoDbWrite (pgInt4 v) (pgJSONB ps)) ns

