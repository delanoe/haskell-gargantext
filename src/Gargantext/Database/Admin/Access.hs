{-|
Module      : Gargantext.Database.Access
Description : Access to Database
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO-SECURITY review purpose of this module
-}



module Gargantext.Database.Admin.Access where

data Action = Read | Write | Exec
data Roles  = RoleUser | RoleMaster

