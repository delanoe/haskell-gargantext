{-|
Module      : Gargantext.Text.TFICF
Description : TFICF Ngrams tools
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Definition of TFICF

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Gargantext.Text.TFICF where

import GHC.Generics (Generic)

import Data.Maybe (Maybe)
import Data.Text (Text)
import Text.Show (Show())

-- import Gargantext.Types
import Gargantext.Prelude


data Context = Corpus | Document
  deriving (Show, Generic)

data TFICF = TFICF { _tficfTerms    :: Text
                   , _tficfContext1 :: Context
                   , _tficfContext2 :: Context
                   , _tficfScore    :: Maybe Double
                   } deriving (Show, Generic)


--tfidf :: Text -> TFICF
--tfidf txt = TFICF txt Document Corpus score
--    where
--        score = Nothing

