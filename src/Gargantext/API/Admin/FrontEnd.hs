{-|
Module      : Gargantext.API.Admin.FrontEnd
Description : Server FrontEnd API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Loads all static file for the front-end.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}

---------------------------------------------------------------------
module Gargantext.API.Admin.FrontEnd where

import Servant
import Servant.Server.StaticFiles (serveDirectoryFileServer)

type FrontEndAPI = Raw

frontEndServer :: Server FrontEndAPI
frontEndServer = serveDirectoryFileServer "./purescript-gargantext/dist"
