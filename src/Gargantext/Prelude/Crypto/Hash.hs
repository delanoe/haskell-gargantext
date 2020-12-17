{-|
Module      : Gargantext.Prelude.Crypto.Hash
Description : Useful Tools near Prelude of the project
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Gargantext.Prelude.Crypto.Hash
  where

import Prelude (String)
import Data.Set (Set)
import Data.Text (Text)
import Gargantext.Prelude
import qualified Data.ByteString.Lazy.Char8  as Char
import qualified Data.Digest.Pure.SHA        as SHA (sha256, showDigest)
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text

--------------------------------------------------------------------------
-- | Use this datatype to keep traceability of hashes
-- TODO use newtype
type Hash = Text

-- | Class to make hashes
class IsHashable a where
  hash :: a -> Hash

-- | Main API to hash text
-- using sha256 for now
instance IsHashable Char.ByteString where
  hash = Text.pack
        . SHA.showDigest
        . SHA.sha256

instance {-# OVERLAPPING #-} IsHashable String where
  hash = hash . Char.pack

instance IsHashable Text where
  hash = hash . Text.unpack


instance IsHashable (Set Hash) where
  hash = hash . foldl (<>) "" . Set.toList

instance {-# OVERLAPPABLE #-} IsHashable a => IsHashable [a] where
  hash = hash . Set.fromList . map hash

