{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Gargantext.Text.Parsers.Isidore where

import Gargantext.Prelude
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

-- import Data.RDF hiding (triple)
import Data.Text hiding (groupBy)

route = "http://isidore.science/sparql/"


--selectExample :: IO (Maybe [Text])
isidore q = do
  res <- selectQuery route $ simpleSelect q
  pure res

simpleSelect :: Text -> Query SelectQuery
simpleSelect q = do
  isidore <- prefix "isidore" (iriRef "http://www.rechercheisidore.fr/class")
  rdf     <- prefix "rdf"     (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  dcterms <- prefix "dcterms" (iriRef "http://purl.org/dc/terms")
  dc      <- prefix "dc"      (iriRef "http://purl.org/dc")

  doc      <- var
  title    <- var
  date     <- var
  abstract <- var
  source   <- var

  triple doc (rdf .:. "type") (isidore .:. "BibliographicalResource")
  triple doc (dcterms .:. "title")       title
  triple doc (dcterms .:. "date")        date
  triple doc (dcterms .:. "source")      source
  triple doc (dc      .:. "description") abstract

  filterExpr $ (.||.) (contains title q) (contains abstract q)
  groupBy title
  groupBy source

  selectVars [title, date, source, abstract]
