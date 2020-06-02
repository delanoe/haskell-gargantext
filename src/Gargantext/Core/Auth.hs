{-|
Module      : Gargantext.API.Admin.Auth.Check
Description :
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Auth ( createPasswordHash
                            , checkPassword
                            , module Data.Password.Argon2
                            )
      where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Password.Argon2 hiding (checkPassword)
import qualified Data.Password.Argon2 as A


createPasswordHash :: MonadIO m 
                   => Text
                   -> m (PasswordHash Argon2)
createPasswordHash x = hashPassword (mkPassword x)


checkPassword :: Password
              -> PasswordHash Argon2
              -> PasswordCheck
checkPassword = A.checkPassword

{-
-- Notes to implement Raw Password with argon2 lib
-- (now using password library, which does not use salt anymore)
-- import Crypto.Argon2 as Crypto
-- import Data.ByteString.Base64.URL as URL
-- import Data.Either
-- import Data.ByteString (ByteString)
secret_key :: ByteString
secret_key = "WRV5ymit8s~ge6%08dLR7Q!gBcpb1MY%7e67db2206"

type SecretKey = ByteString

hashNode :: SecretKey -> NodeToHash -> ByteString
hashNode sk (NodeToHash nt ni) = case hashResult of
    Left  e -> panic (cs $ show e)
    Right h -> URL.encode h
  where
    hashResult = Crypto.hash Crypto.defaultHashOptions
                  sk
                  (cs $ show nt <> show ni)
-}
