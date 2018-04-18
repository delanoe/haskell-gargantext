{-|
Module      : Gargantext.Databse.Ngram
Description : 
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Gargantext.Database.Ngram where

import Prelude
import Data.Text (Text)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import qualified Database.PostgreSQL.Simple as PGS

import Opaleye

-- Functions only
import Data.List (find)


data NgramPoly id terms n = Ngram { ngram_id    :: id
                                  , ngram_terms :: terms
                                  , ngram_n     :: n
                                  } deriving (Show)

type NgramWrite = NgramPoly (Maybe (Column PGInt4))
                                   (Column PGText)
                                   (Column PGInt4)

type NgramRead  = NgramPoly        (Column PGInt4)
                                   (Column PGText)
                                   (Column PGInt4)

type Ngram = NgramPoly Int Text Int

$(makeAdaptorAndInstance "pNgram"    ''NgramPoly)
$(makeLensesWith abbreviatedFields   ''NgramPoly)

ngramTable :: Table NgramWrite NgramRead
ngramTable = Table "ngrams" (pNgram Ngram { ngram_id    = optional "id"
                                            , ngram_terms = required "terms"
                                            , ngram_n     = required "n"
                                            }
                                )

queryNgramTable :: Query NgramRead
queryNgramTable = queryTable ngramTable


--selectUsers :: Query UserRead
--selectUsers = proc () -> do
--      --user@(i, p, ll, is, un, fn, ln, m, iff, ive, dj) <- queryUserTable -< ()
--      row@(User i p ll is un fn ln m iff ive dj) <- queryUserTable -< ()
--      O.restrict -< i .== 1
--      --returnA -< User i p ll is un fn ln m iff ive dj
--      returnA -< row
--

findWith :: (Eq a1, Foldable t) => (a -> a1) -> a1 -> t a -> Maybe a
findWith f t = find (\x -> f x == t)

--userWithUsername :: Text -> [User] -> Maybe User
--userWithUsername t xs = userWith userUsername t xs
--
--userWithId :: Integer -> [User] -> Maybe User
--userWithId t xs = userWith userUserId t xs

-- | not optimized (get all ngrams without filters)
getNgrams :: PGS.Connection -> IO [Ngram]
getNgrams conn = runQuery conn queryNgramTable
