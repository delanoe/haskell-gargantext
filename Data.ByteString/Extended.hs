{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ByteString.Extended ( module Data.ByteString
                                , replace
                                ) where
import Data.ByteString

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace = undefined

-- instance (Binary k, Binary v) => Binary (HaskMap k v) where
-- ...
