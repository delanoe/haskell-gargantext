{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.Private where

import qualified Database.PostgreSQL.Simple as PGS

-- TODO add a reader Monad here
-- read this in the init file
infoGargandb :: PGS.ConnectInfo
infoGargandb =  PGS.ConnectInfo { PGS.connectHost = "127.0.0.1"
                               , PGS.connectPort = 5432
                               , PGS.connectUser = "gargantua"
                               , PGS.connectPassword = "C8kdcUrAQy66U"
                               , PGS.connectDatabase = "gargandb" }


