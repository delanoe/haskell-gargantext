{-|
Module      : Gargantext.Database.Admin.Types.Hyperdata.Folder
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gargantext.Database.Admin.Types.Hyperdata.Folder
  where

import Gargantext.Database.Admin.Types.Hyperdata.Corpus

------------------------------------------------------------------------
type HyperdataFolder = HyperdataCorpus

defaultHyperdataFolder :: HyperdataFolder
defaultHyperdataFolder = defaultHyperdataCorpus
