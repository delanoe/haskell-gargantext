{-|
Module      : Gargantext.Text.Flow
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

From text to viz, all the flow of texts in Gargantext.

-}

{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}

module Gargantext.Text.Flow
  where

import qualified Data.Text as T
--import Data.Text.IO (readFile)
import Database.PostgreSQL.Simple (Connection)
import GHC.IO (FilePath)
--import Gargantext.Core (Lang)
import Gargantext.Core.Types (CorpusId)

{-
  ____                             _            _
 / ___| __ _ _ __ __ _  __ _ _ __ | |_ _____  _| |_
| |  _ / _` | '__/ _` |/ _` | '_ \| __/ _ \ \/ / __|
| |_| | (_| | | | (_| | (_| | | | | ||  __/>  <| |_
 \____|\__,_|_|  \__, |\__,_|_| |_|\__\___/_/\_\\__|
                 |___/
-}


contextText :: [T.Text]
contextText = ["The dog is an animal."
              ,"The bird is an animal."
              ,"The dog is an animal."
              ,"The animal is a bird or a dog ?"
              ,"The table is an object."
              ,"The pen is an object."
              ,"The object is a pen or a table ?"
              ,"The girl is a human."
              ,"The boy  is a human."
              ,"The boy or the girl are human."
              ]


-- | Control the flow of text
data TextFlow = CSV FilePath
              | FullText FilePath
              | Contexts [T.Text]
              | DBV3 Connection CorpusId
              | Query T.Text

{-
textFlow :: TermType Lang -> TextFlow -> IO Graph
textFlow termType workType = do
  contexts <- case workType of
                FullText path -> splitBy (Sentences 5) <$> readFile path
                CSV      path -> readCsvOn [csv_title, csv_abstract] path
                Contexts ctxt -> pure ctxt
                DBV3 con corpusId -> catMaybes <$> map (\n -> hyperdataDocumentV3_title (_node_hyperdata n)  <> hyperdataDocumentV3_abstract (_node_hyperdata n))<$> runReaderT (getDocumentsV3WithParentId corpusId) con
                _             -> undefined -- TODO Query not supported

  textFlow' termType contexts


textFlow' :: TermType Lang -> [T.Text] -> IO Graph
textFlow' termType contexts = do
  -- Context :: Text -> [Text]
  -- Contexts = Paragraphs n | Sentences n | Chars n

  myterms <- extractTerms termType contexts
  -- TermsType = Mono | Multi | MonoMulti
  -- myterms # filter (\t -> not . elem t stopList)
  --         # groupBy (Stem|GroupList|Ontology)
  --printDebug "terms" myterms
  --printDebug "myterms" (sum $ map length myterms)

  -- Bulding the map list
  -- compute copresences of terms, i.e. cooccurrences of terms in same context of text
  -- Cooc = Map (Term, Term) Int
  let myCooc1 = coocOn (_terms_label) myterms
  --printDebug "myCooc1 size" (M.size myCooc1)

  -- Remove Apax: appears one time only => lighting the matrix
  let myCooc2 = Map.filter (>0) myCooc1
  --printDebug "myCooc2 size" (M.size myCooc2)
  --printDebug "myCooc2" myCooc2
  g <- cooc2graph myCooc2
  pure g
-}

