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
    opts = defaults & header "Accept" .~ ["application/sparql-results+xml"]
                    & header "User-Agent" .~ ["gargantext-hsparql-client"]
                    & param "query" .~ [Data.Text.pack q]

isidoreGet :: Text -> IO ByteString
isidoreGet q = do
  let s = createSelectQuery $ simpleSelect q
  putStrLn s
  r <- selectQueryRaw' route s
  putStrLn $ show $ r ^. responseStatus
  pure $ r ^. responseBody
 -- res <- selectQuery route $ simpleSelect q
 -- pure res

simpleSelect :: Text -> Query SelectQuery
simpleSelect q = do
  isidore <- prefix "isidore" (iriRef "http://www.rechercheisidore.fr/class")
  rdf     <- prefix "rdf"     (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  dcterms <- prefix "dcterms" (iriRef "http://purl.org/dc/terms")
  dc      <- prefix "dc"      (iriRef "http://purl.org/dc")

  doc      <- var
  title    <- var
  --date     <- var
  abstract <- var
  --source   <- var

  triple_ doc (rdf .:. "type")      (isidore .:. "BibliographicalResource")
  triple_ doc (dcterms .:. "title")  title
  --triple doc (dcterms .:. "date")        date
  --triple doc (dcterms .:. "source")      source
  triple doc (dc      .:. "description") abstract

  --filterExpr $ (.||.) (contains title q) (contains abstract q)
  filterExpr_ (contains title q) -- (contains abstract q)
  limit_ 3

  selectVars [title]
