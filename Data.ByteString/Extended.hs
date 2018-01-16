{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Data.ByteString.Extended
Description : Short description
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Example showing how to extend existing base libraries.
-}


module Data.ByteString.Extended ( module Data.ByteString
                                , replace
                                ) where
import Data.ByteString

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace = undefined

-- instance (Binary k, Binary v) => Binary (HaskMap k v) where
-- ...
