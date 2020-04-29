{-|
Module      : Gargantext.Database.Tree.Error
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Gargantext.Database.Query.Tree.Error
  where

import Control.Lens (Prism', (#))
import Control.Monad.Error.Class (MonadError(throwError))
import Gargantext.Prelude

------------------------------------------------------------------------
data TreeError = NoRoot
               | EmptyRoot
               | TooManyRoots

instance Show TreeError
  where
    show NoRoot       = "Root node not found"
    show EmptyRoot    = "Root node should not be empty"
    show TooManyRoots = "Too many root nodes"

class HasTreeError e where
  _TreeError :: Prism' e TreeError

treeError :: ( MonadError e m
             , HasTreeError e)
             => TreeError
             -> m a
treeError te = throwError $ _TreeError # te

