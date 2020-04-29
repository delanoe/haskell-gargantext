{-|
Module      : Gargantext.Database.Types.Error
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Gargantext.Database.Query.Table.Node.Error where

import Control.Lens (Prism', (#), (^?))
import Control.Monad.Error.Class (MonadError(..))
import Gargantext.Prelude hiding (sum, head)
import Prelude hiding (null, id, map, sum)

------------------------------------------------------------------------
data NodeError = NoListFound
               | NoRootFound
               | NoCorpusFound
               | NoUserFound
               | MkNode
               | UserNoParent
               | HasParent
               | ManyParents
               | NegativeId
               | NotImplYet
               | ManyNodeUsers

instance Show NodeError
  where
    show NoListFound   = "No list   found"
    show NoRootFound   = "No Root   found"
    show NoCorpusFound = "No Corpus found"
    show NoUserFound   = "No user   found"

    show MkNode        = "Cannot make node"
    show NegativeId    = "Node with negative Id"
    show UserNoParent  = "Should not have parent"
    show HasParent     = "NodeType has parent"
    show NotImplYet    = "Not implemented yet"
    show ManyParents   = "Too many parents"
    show ManyNodeUsers = "Many userNode/user"

class HasNodeError e where
  _NodeError :: Prism' e NodeError

nodeError :: ( MonadError e m
             , HasNodeError e)
          => NodeError -> m a
nodeError ne = throwError $ _NodeError # ne

catchNodeError :: (MonadError e m, HasNodeError e) => m a -> (NodeError -> m a) -> m a
catchNodeError f g = catchError f (\e -> maybe (throwError e) g (e ^? _NodeError))
