{-|
Module      : Gargantext.Database.Query.Table.NgramsPostag
Description : Deal with in Gargantext Database.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gargantext.Database.Query.Table.NgramsPostag
    where

import Control.Lens (view)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Gargantext.Core
import Gargantext.Core.Types
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Types
import Gargantext.Prelude
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.List                  as List
import qualified Database.PostgreSQL.Simple as PGS

data NgramsPostag = NgramsPostag { _np_lang   :: Lang
                                 , _np_algo   :: PosTagAlgo
                                 , _np_postag :: POS
                                 , _np_form   :: Ngrams
                                 , _np_lem    :: Ngrams
                                 }
  deriving (Eq, Ord, Generic, Show)

makeLenses ''NgramsPostag

instance Hashable NgramsPostag

type NgramsPostagInsert = ( Int
                          , Int
                          , Text
                          , Text
                          , Int
                          , Text
                          , Int
                          )

toInsert :: NgramsPostag -> NgramsPostagInsert
toInsert (NgramsPostag l a p form lem) =
  ( toDBid l
  , toDBid a
  , cs $ show p
  , view ngramsTerms form
  , view ngramsSize  form
  , view ngramsTerms lem
  , view ngramsSize  lem
  )

insertNgramsPostag :: [NgramsPostag] -> Cmd err (HashMap Text NgramsId)
insertNgramsPostag ns =
  if List.null ns
     then pure HashMap.empty
     else HashMap.fromList
         <$> map (\(Indexed t i) -> (t,i))
         <$> insertNgramsPostag' (map toInsert ns)

insertNgramsPostag' :: [NgramsPostagInsert] -> Cmd err [Indexed Text Int]
insertNgramsPostag' ns = runPGSQuery queryInsertNgramsPostag (PGS.Only $ Values fields ns)
  where

    fields = map (\t -> QualifiedIdentifier Nothing t) $ snd fields_name

    fields_name :: ( [Text], [Text])
    fields_name = ( ["lang_id", "algo_id", "postag", "form", "form_n", "lem" , "lem_n"]
                  , ["int4"   , "int4"   , "text"  , "text", "int4"  , "text", "int4" ]
                  )

----------------------
queryInsertNgramsPostag :: PGS.Query
queryInsertNgramsPostag = [sql|
  WITH input_rows(lang_id,algo_id,postag,form,form_n, lem, lem_n)
   AS (?)
   -- ((VALUES (1::"int4",2::"int4",'VB'::"text",'dansaient'::"text",1::"int4",'danser'::"text",1::"int4")))
  ------------------------------------------------
  , ins_form AS (INSERT INTO ngrams (terms,n)
    SELECT ir1.form, ir1.form_n
      FROM input_rows as ir1
      UNION ALL
      SELECT ir2.lem, ir2.lem_n
      FROM input_rows as ir2
      ON CONFLICT (terms)
        DO NOTHING
        RETURNING id,terms
      )
  ------------------------------------------------
  , ins_form_ret AS (
      SELECT id, terms
      FROM   ins_form
      UNION  ALL
      SELECT n.id, ir.form
      FROM   input_rows ir
      JOIN   ngrams n ON n.terms = ir.form
    )

  , ins_lem_ret AS (
      SELECT id, terms
      FROM   ins_form
      UNION  ALL
      SELECT n.id, ir.lem
      FROM   input_rows ir
      JOIN   ngrams n ON n.terms = ir.lem
    )
  ------------------------------------------------
  ------------------------------------------------
  , ins_postag AS ( INSERT INTO ngrams_postag (lang_id, algo_id, postag, ngrams_id, lemm_id,score)
    SELECT ir.lang_id, ir.algo_id, ir.postag, form.id, lem.id, 1
    FROM input_rows ir
      JOIN ins_form_ret  form ON form.terms = ir.form
      JOIN ins_lem_ret   lem  ON lem.terms  = ir.lem


      ON CONFLICT (lang_id,algo_id,postag,ngrams_id,lemm_id)
        DO UPDATE SET score = ngrams_postag.score + 1
    )

SELECT terms,id FROM ins_form_ret
 INNER JOIN input_rows ir ON ins_form_ret.terms = ir.form

  |]

