{-|
Module      : Gargantext.API.Swagger
Description : Swagger API generation
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

---------------------------------------------------------------------
module Gargantext.API.Swagger where
---------------------------------------------------------------------
import Control.Lens
import Data.Swagger
import Data.Version (showVersion)
import Servant
import Servant.Swagger
import qualified Paths_gargantext as PG -- cabal magic build module

import Gargantext.API.Routes
import Gargantext.Prelude

-- | Swagger Specifications
swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy GargAPI)
  & info.title       .~ "GarganText"
  & info.version     .~ (cs $ showVersion PG.version)
  -- & info.base_url     ?~ (URL "http://gargantext.org/")
  & info.description ?~ "REST API specifications"
  -- & tags             .~ Set.fromList [Tag "Garg" (Just "Main perations") Nothing]
  & applyTagsFor (subOperations (Proxy :: Proxy GargAPI)(Proxy :: Proxy GargAPI)) 
                 ["Gargantext" & description ?~ "Main operations"]
  & info.license     ?~ ("AGPLV3 (English) and CECILL (French)" & url ?~ URL urlLicence )
    where
        urlLicence = "https://gitlab.iscpif.fr/gargantext/haskell-gargantext/blob/master/LICENSE"
