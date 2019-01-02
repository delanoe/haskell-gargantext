{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Gargantext.Text.Parsers.Isidore where

import Gargantext.Prelude
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

-- import Data.RDF hiding (triple)
import Data.Text

route = "http://isidore.science/sparql/"


--selectExample :: IO (Maybe [Text])
selectIsidore = do
  res <- selectQuery route simpleSelect
  pure res

simpleSelect :: Query SelectQuery
simpleSelect = do
  isidore <- prefix "isidore" (iriRef "http://www.rechercheisidore.fr/class")
  rdf     <- prefix "rdf"     (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  dcterms <- prefix "dcterms" (iriRef "http://purl.org/dc/terms")
  dc      <- prefix "dc"      (iriRef "http://purl.org/dc")

  doc      <- var
  resource <- var
  title    <- var
  date     <- var
  abstract <- var
  source   <- var

  triple resource (rdf .:. "type") (isidore .:. "BibliographicalResource")

  triple doc (dcterms .:. "title")       title
  triple doc (dcterms .:. "date")        date
  triple doc (dcterms .:. "source")      source
  triple doc (dc      .:. "description") abstract

  filterExpr $ contains title ("ville" :: Text)

  selectVars [title]
