{-|
Module      : Gargantext.Database.Flow.Annuaire
Description : Database Flow Annuaire
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}

module Gargantext.Database.Flow.Annuaire
    where



{-
import Gargantext.Prelude
import Gargantext.Database.Flow

-- | Annuaire

flowAnnuaire :: FlowCmdM env ServantErr m => FilePath -> m ()
flowAnnuaire filePath = do
  contacts <- liftBase $ deserialiseImtUsersFromFile filePath
  ps <- flowInsertAnnuaire "Annuaire"
      $ map (\h-> ToDbContact h)
      $ map addUniqIdsContact contacts
  printDebug "length annuaire" ps
-}

