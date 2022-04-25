{-# LANGUAGE TemplateHaskell            #-}

module Gargantext.Database.Admin.Types.Hyperdata.Codebook where

import Gargantext.Database.Admin.Types.Hyperdata.Prelude


data HyperdataCodebook =
  HyperdataCodebook { _hco_name :: !Text }

defaultHyperdataCodebook :: HyperdataCodebook
defaultHyperdataCodebook = HyperdataCodebook ""

$(deriveJSON (unPrefix "_hco_") ''HyperdataCodebook)
