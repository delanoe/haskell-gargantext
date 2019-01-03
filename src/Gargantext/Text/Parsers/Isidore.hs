{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Gargantext.Text.Parsers.Isidore where

import Gargantext.Prelude
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

-- import Data.RDF hiding (triple)
import Data.Text hiding (groupBy)

import Control.Lens hiding (contains)
import Data.ByteString.Lazy (ByteString)
import Prelude (String)
import Network.Wreq

route :: EndPoint
route = "https://isidore.science/sparql/"

selectQueryRaw' :: String -> String -> IO (Response ByteString)
selectQueryRaw' uri q = getWith opts uri
  where
    opts = defaults & header "Accept"     .~ ["application/sparql-results+xml"]
                    & header "User-Agent" .~ ["gargantext-hsparql-client"]
                    & param  "query"      .~ [Data.Text.pack q]

isidoreGet :: Text -> IO (Maybe [[BindingValue]])
isidoreGet q = do
  let s = createSelectQuery $ isidoreSelect q
  putStrLn s
  r <- selectQueryRaw' route s
  putStrLn $ show $ r ^. responseStatus
  pure $ structureContent $ r ^. responseBody
 -- res <- selectQuery route $ simpleSelect q
 -- pure res

isidoreSelect :: Text -> Query SelectQuery
isidoreSelect q = do
  -- See Predefined Namespace Prefixes:
  -- https://isidore.science/sparql?nsdecl
  isidore <- prefix "isidore" (iriRef "http://www.rechercheisidore.fr/class/")
  rdf     <- prefix "rdf"     (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  dcterms <- prefix "dcterms" (iriRef "http://purl.org/dc/terms/")
  dc      <- prefix "dc"      (iriRef "http://purl.org/dc/elements/1.1/")
  --iso     <- prefix "fra"        (iriRef "http://lexvo.org/id/iso639-3/")
  --ore     <- prefix "ore"    (iriRef "http://www.openarchives.org/ore/terms/")
  --bif     <- prefix "bif"    (iriRef "bif:")

  link     <- var
  title    <- var
  date     <- var
  abstract <- var
  authors  <- var
  source   <- var
  langDoc  <- var
  publisher <- var
  --langFr   <- var
  --agg       <- var

  triple_ link (rdf     .:. "type")     (isidore .:. "BibliographicalResource")
  triple_ link (dcterms .:. "title")    title
  triple_ link (dcterms .:. "date")     date
  triple_ link (dcterms .:. "creator")  authors
  triple_ link (dcterms .:. "language") langDoc
  triple_ link (dc      .:. "description") abstract
  --triple_ link (ore .:. "isAggregatedBy") agg
  --triple_ agg (dcterms .:. "title") title

  optional_ $ triple_ link (dcterms .:. "source")      source
  optional_ $ triple_ link (dcterms .:. "publisher")   publisher

  -- TODO FIX BUG with (.||.) operator
  --filterExpr $ (.||.) (contains title q) (contains title q)
  filterExpr_ (containsWith title q) -- (contains abstract q)
  --filterExpr (containsWith abstract q) -- (contains abstract q)

  -- TODO FIX filter with lang
  --filterExpr $ langMatches title (str ("fra" :: Text))
  --filterExpr $ (.==.) lang (str ("http://lexvo.org/id/iso639-3/fra" :: Text))
  
  orderNextDesc date
  limit_ 10
  distinct_
  selectVars [link, date, langDoc, authors, source, publisher, title, abstract]
