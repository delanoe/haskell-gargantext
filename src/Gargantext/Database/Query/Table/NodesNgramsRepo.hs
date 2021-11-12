{-|
Module      : Gargantext.Database.Schema.NodesNgramsRepo
Description :
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

module Gargantext.Database.Query.Table.NodesNgramsRepo
  where

{-
import Gargantext.Database.Schema.Prelude
import Gargantext.API.Ngrams (NgramsStatePatch)
import Gargantext.Database.Schema.NodesNgramsRepo
import Gargantext.Database.Prelude (mkCmd, Cmd, runOpaQuery)
import Gargantext.Prelude


selectPatches :: Query RepoDbRead
selectPatches = proc () -> do
  repos <- selectTable repoTable -< ()
  returnA -< repos

_selectRepo :: Cmd err [RepoDbNgrams]
_selectRepo =  runOpaQuery selectPatches

_insertRepos :: [NgramsStatePatch] -> Cmd err Int64
_insertRepos ns = mkCmd $ \conn -> runInsert_ conn $ Insert repoTable (toWrite ns) rCount Nothing
  where
    toWrite :: [NgramsStatePatch] -> [RepoDbWrite]
    toWrite = undefined
    --ns' = map (\(RepoDbNgrams v ps) -> RepoDbWrite (sqlInt4 v) (pgJSONB ps)) ns
-}
