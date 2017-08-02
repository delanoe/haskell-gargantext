{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Data.Gargantext.Database.Ngram where

import Prelude
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Maybe (Maybe)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Lens.TH (makeLensesWith, abbreviatedFields)
import Control.Arrow (returnA)
import qualified Database.PostgreSQL.Simple as PGS

import qualified Opaleye as O
import Opaleye (Column, PGBool, PGInt4, PGText, PGTimestamptz
               , Table(Table), Query
               , QueryRunnerColumnDefault, queryRunnerColumnDefault 
               , fieldQueryRunnerColumn 
               , (.==), (.>)
               )

import Data.Gargantext.Database.Private (infoGargandb)
import Data.Gargantext.Database.Instances

-- Functions only
import Data.List (find)


data NgramPoly id terms n = Ngram { ngram_id    :: id
                                  , ngram_terms :: terms
                                  , ngram_n     :: n
                                  } deriving (Show)

type NgramWrite = NgramPoly (Maybe (Column PGInt4)) (Column PGText) (Column PGInt4)
type NgramRead  = NgramPoly        (Column PGInt4)  (Column PGText) (Column PGInt4)

type Ngram = NgramPoly Int Text Int

$(makeAdaptorAndInstance "pNgram"    ''NgramPoly)
$(makeLensesWith abbreviatedFields   ''NgramPoly)


ngramTable :: O.Table NgramWrite NgramRead
ngramTable = O.Table "ngrams" (pNgram Ngram { ngram_id    = O.optional "id"
                                            , ngram_terms = O.required "terms"
                                            , ngram_n     = O.required "n"
                                            }
                                )


queryNgramTable :: Query NgramRead
queryNgramTable = O.queryTable ngramTable


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
ngrams :: IO [Ngram]
ngrams = do
    conn <- PGS.connect infoGargandb
    O.runQuery conn queryNgramTable

