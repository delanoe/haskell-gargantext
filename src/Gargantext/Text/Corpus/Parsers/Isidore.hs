{-|
Module      : Gargantext.Text.Corpus.Parsers.Isidore
Description : To query French Humanities publication database
Copyright   : (c) CNRS, 2019-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

TODO:
- put endpoint in configuration file
- more flexible fields of research
- type database name
- use more ontologies to help building corpora
-}


{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Text.Corpus.Parsers.Isidore where

import Control.Lens hiding (contains)
import Data.ByteString.Lazy (ByteString)
import Data.RDF hiding (triple, Query)
import Data.Text hiding (groupBy, map)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import Gargantext.Core (Lang)
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Prelude
import Network.Wreq (getWith, Response, defaults, header, param, responseStatus, responseBody)
import Prelude (String)

route :: EndPoint
route = "https://isidore.science/sparql/"

selectQueryRaw' :: String -> String -> IO (Response ByteString)
selectQueryRaw' uri q = getWith opts uri
  where
    opts = defaults & header "Accept"     .~ ["application/sparql-results+xml"]
                    & header "User-Agent" .~ ["gargantext-hsparql-client"]
                    & param  "query"      .~ [Data.Text.pack q]

isidoreGet :: Lang -> Int -> Text -> IO (Maybe [HyperdataDocument])
isidoreGet la li q = do
  bindingValues <- isidoreGet' li q
  case bindingValues of
    Nothing -> pure Nothing
    Just dv -> pure $ Just $ map (bind2doc la) dv

isidoreGet' :: Int -> Text -> IO (Maybe [[BindingValue]])
isidoreGet' l q = do
  let s = createSelectQuery $ isidoreSelect l q
  putStrLn s
  r <- selectQueryRaw' route s
  putStrLn $ show $ r ^. responseStatus
  pure $ structureContent $ r ^. responseBody
 -- res <- selectQuery route $ simpleSelect q
 -- pure res

isidoreSelect :: Int -> Text -> Query SelectQuery
isidoreSelect lim q = do
  -- See Predefined Namespace Prefixes:
  -- https://isidore.science/sparql?nsdecl
  isidore <- prefix "isidore" (iriRef "http://isidore.science/class/")
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
  --agg       <- var

  triple_ link (rdf     .:. "type")     (isidore .:. "Document")
  triple_ link (dcterms .:. "title")    title
  triple_ link (dcterms .:. "date")     date
  triple_ link (dcterms .:. "creator")  authors
  --triple_ link (dcterms .:. "language") langDoc
  triple_ link (dc      .:. "description") abstract
  --triple_ link (ore .:. "isAggregatedBy") agg
  --triple_ agg (dcterms .:. "title") title

  optional_ $ triple_ link (dcterms .:. "source")      source
  optional_ $ triple_ link (dcterms .:. "publisher")   publisher

  -- TODO FIX BUG with (.||.) operator
  --filterExpr_ $ (.||.) (contains title q) (contains abstract q)
  --filterExpr_ (containsWith authors q) -- (contains abstract q)
  --filterExpr_ (containsWith title q) -- (contains abstract q)
  --filterExpr_ $ (.||.) (containsWith title q) (contains abstract q)
  filterExpr_ (containsWith title q)

  -- TODO FIX filter with lang
  --filterExpr_ $ langMatches title (str ("fra" :: Text))
  --filterExpr_ $ (.==.) langDoc (str ("http://lexvo.org/id/iso639-3/fra" :: Text))

  orderNextDesc date
  limit_ lim
  distinct_
  selectVars [link, date, langDoc, authors, source, publisher, title, abstract]

-- | TODO : check if all cases are taken into account
unbound :: Lang -> BindingValue -> Maybe Text
unbound _ Unbound         = Nothing
unbound _ (Bound (UNode x)) = Just x
unbound _ (Bound (LNode (TypedL x _))) = Just x
unbound _ (Bound (LNode (PlainL x)))   = Just x
unbound l (Bound (LNode (PlainLL x l')))   = if l' == (toLower $ cs $ show l) then Just x else Nothing
unbound _ _ = Nothing

bind2doc :: Lang -> [BindingValue] -> HyperdataDocument
bind2doc l [ link, date, langDoc, authors, _source, publisher, title, abstract ] =
  HyperdataDocument (Just "Isidore")
                      Nothing
                      (unbound l link)
                      Nothing Nothing Nothing
                      (unbound l title)
                      (unbound l authors)
                      Nothing
                      (unbound l publisher)
                      (unbound l abstract)
                      (unbound l date)
                      Nothing Nothing Nothing Nothing Nothing Nothing
                      (unbound l langDoc)

bind2doc _ _  = undefined
