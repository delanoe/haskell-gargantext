{-|
Module      : Gargantext.Core.Text.List.Merge
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}

module Gargantext.Core.Text.List.Merge
  where

import Control.Lens (view)
import Data.Map.Strict (Map)
import Gargantext.API.Ngrams
import Gargantext.API.Ngrams.Types
import Gargantext.Prelude
import Data.Map.Strict.Patch hiding (PatchMap)

type List = Map NgramsTerm NgramsRepoElement
type Patch = PatchMap NgramsTerm (Replace (Maybe NgramsRepoElement))

-- Question: which version of Patching increment is using the FrontEnd ?
diffList :: Versioned List -> Versioned List -> Versioned Patch
diffList l1 l2 = Versioned (1 + view v_version l1)
                           (diff (view v_data l1) (view v_data l2))

-- | TODO
{-
commit :: ListId -> NgramsType -> Versioned Patch -> List -> List
commit = undefined
-}
