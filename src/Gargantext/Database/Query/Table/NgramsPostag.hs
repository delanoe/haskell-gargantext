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

import Data.Text (Text)
import Gargantext.Database.Prelude (Cmd, runPGSQuery)
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Schema.Prelude
import Gargantext.Database.Types
import Gargantext.Prelude
import qualified Database.PostgreSQL.Simple as PGS




type NgramsPostagInsert = ( Int
                          , Int
                          , Text
                          , Text
                          , Int
                          , Text
                          , Int
                          )


insertNgramsPostag :: [NgramsPostagInsert] -> Cmd err [Indexed Int Text]
insertNgramsPostag ns = runPGSQuery queryInsertNgramsPostag (PGS.Only $ Values fields ns)
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

