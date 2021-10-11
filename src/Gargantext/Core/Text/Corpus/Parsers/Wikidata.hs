{-|
Module      : Gargantext.Core.Text.Corpus.Parsers.Wikidata
Description : To query Wikidata
Copyright   : (c) CNRS, 2019-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gargantext.Core.Text.Corpus.Parsers.Wikidata where

import Control.Lens (makeLenses, (^.) )
import Data.Maybe (catMaybes)
import Data.Text (Text, concat)
import Database.HSparql.Connection
import Gargantext.Core (Lang(..))
import Gargantext.Core.Text.Corpus.Parsers.Isidore (unbound)
import Gargantext.Database.Admin.Types.Hyperdata.Document (HyperdataDocument(..))
import Gargantext.Prelude
import Gargantext.Core.Text.Corpus.Parsers.Wikidata.Crawler
import Prelude (String)
import qualified Data.List as List
import Gargantext.Core.Text.Corpus.Parsers.Date (dateSplit)



data WikiResult = WikiResult { _wr_cid         :: Maybe Text
                             , _wr_title       :: Maybe Text
                             , _wr_url         :: Maybe Text
                             , _wr_yearStart   :: Maybe Text
                             , _wr_yearEnd     :: Maybe Text
                             , _wr_yearFlorish :: Maybe Text
                             } deriving (Show, Eq)
$(makeLenses ''WikiResult)

type NumberOfSections = Int

wikidataGet :: Int -> NumberOfSections -> IO [HyperdataDocument]
wikidataGet n m = do
  results <- wikidataSelect n
  mapM (wikiPageToDocument m) results


wikiPageToDocument :: NumberOfSections -> WikiResult ->  IO HyperdataDocument
wikiPageToDocument m wr = do

  sections <- case wr ^. wr_url of
    Nothing -> pure []
    Just  u -> crawlPage u

  let bdd    = Just "wikidata"
      doi    = Nothing
      url    = (wr ^. wr_url)
      uniqId    = Nothing
      uniqIdBdd = Nothing
      page      = Nothing
      title     = (wr ^. wr_title)
      authors    = Nothing
      institutes = Nothing
      source     = Nothing
      abstract   = Just $ concat $ take m sections

  (date, (year, month, day))
    <- dateSplit EN $ head
                    $ catMaybes
                    [ wr ^. wr_yearStart
                    , wr ^. wr_yearEnd
                    , wr ^. wr_yearFlorish
                    , head sections
                    ]

  let hour = Nothing
      minute = Nothing
      second = Nothing
      iso2   = Just $ cs $ show EN

  pure $ HyperdataDocument bdd doi url uniqId uniqIdBdd
                           page title authors institutes source
                           abstract ((cs . show) <$> date) year month day hour minute second iso2


wikidataSelect :: Int -> IO [WikiResult]
wikidataSelect n = do
  result <- selectQueryRaw wikidataRoute (wikidataQuery n)
  case result of
    Nothing      -> pure []
    Just result' -> pure $ map toWikiResult $ unbound' EN result'


unbound' :: Lang -> [[BindingValue]] -> [[Maybe Text]]
unbound' l = map (map (unbound l))

toWikiResult :: [Maybe Text] -> WikiResult
toWikiResult (c:t:u:ys:ye:yf:_) = WikiResult c t u ys ye yf
toWikiResult _                  = panic "[G.C.T.C.Parsers.Wikidata.toWikiResult] error"

wikidataRoute :: EndPoint
wikidataRoute = "https://query.wikidata.org/sparql"

wikidataQuery :: Int -> String
wikidataQuery n = List.unlines
      ["     PREFIX wd:       <http://www.wikidata.org/entity/>"
      ,"     PREFIX wdt:      <http://www.wikidata.org/prop/direct/>"
      ,"     PREFIX schema:   <http://schema.org/>"
      ,"     PREFIX wikibase: <http://wikiba.se/ontology#>"
      ,"     SELECT DISTINCT "
      ,"      ?cid"
      ,"      ?title"
      ,"      ?url"
      ,"      (year(xsd:dateTime(?dateStart))   as ?yearStart)"
      ,"      (year(xsd:dateTime(?dateEnd))     as ?yearEnd)"
      ,"      (year(xsd:dateTime(?dateFlorish)) as ?yearFlorish) "
      ,"     WHERE {"
      ,"       ?cid wdt:P31 wd:Q968159 ."
      ,"       ?cid rdfs:label ?title filter (lang(?title) = \"en\") ."
      ,"      "
      ,"       ?url schema:about ?cid ."
      ,"       ?url schema:inLanguage \"en\" ."
      ,"       FILTER (SUBSTR(str(?url), 1, 25) = \"https://en.wikipedia.org/\")"
      ,"       OPTIONAL {?cid (wdt:P580) ?dateStart   .}"
      ,"       OPTIONAL {?cid (wdt:P582) ?dateEnd     .}"
      ,"       OPTIONAL {?cid (wdt:P571) ?dateFlorish .}"
      ,"     }"
      ,"       LIMIT " <> (cs $ show n)
      ]
