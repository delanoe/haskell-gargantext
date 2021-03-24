{-|
Module      : Gargantext.Core.Text.Corpus.API.Pubmed
Description : Pubmed API connection
Copyright   : (c) CNRS, 2017
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}


module Gargantext.Core.Text.Corpus.API.Pubmed
    where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Gargantext.Prelude
import Gargantext.Core (Lang(..))
import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))

import qualified PUBMED as PubMed
import qualified PUBMED.Parser as PubMedDoc


type Query = Text
type Limit = PubMed.Limit


-- | TODO put default pubmed query in gargantext.ini
-- by default: 10K docs
get :: Query -> Maybe Limit -> IO [HyperdataDocument]
get q l = either (\e -> panic $ "CRAWL: PubMed" <> e) (map (toDoc EN))
        <$> PubMed.getMetadataWith q l

toDoc :: Lang -> PubMedDoc.PubMed -> HyperdataDocument
toDoc l (PubMedDoc.PubMed (PubMedDoc.PubMedArticle t j as aus)
                    (PubMedDoc.PubMedDate a y m d)
          ) = HyperdataDocument (Just "PubMed")
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 t
                                 (authors aus)
                                 (institutes aus)
                                 j
                                 (abstract as)
                                 (Just $ Text.pack $ show a)
                                 (Just $ fromIntegral y)
                                 (Just m)
                                 (Just d)
                                 Nothing
                                 Nothing
                                 Nothing
                                 (Just $ (Text.pack . show) l)
      where
        authors :: Maybe [PubMedDoc.Author] -> Maybe Text
        authors aus' = case aus' of
            Nothing -> Nothing
            Just au -> Just $ (Text.intercalate ", ")
                            $ catMaybes
                            $ map (\n -> PubMedDoc.foreName n <> Just " " <> PubMedDoc.lastName n) au

        institutes :: Maybe [PubMedDoc.Author] -> Maybe Text
        institutes aus' = case aus' of
            Nothing -> Nothing
            Just au -> Just $ (Text.intercalate ", ")
                            $ (map (Text.replace ", " " - "))
                            $ catMaybes
                            $ map PubMedDoc.affiliation au


        abstract :: Maybe [Text] -> Maybe Text
        abstract as' = fmap (Text.intercalate ", ") as'

