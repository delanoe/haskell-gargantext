{-|
Module      : Gargantext.Database.Learn
Description : Learn Small Data Analytics with big data connection (DB)
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}

module Gargantext.Database.Learn where

import Data.Text (Text)
import Data.Tuple (snd)
import Data.Maybe
import Gargantext.Database.Facet
import Gargantext.Database.Types.Node
import Gargantext.Prelude
import Gargantext.Text.Learn
import qualified Data.List as List
import qualified Data.Text as Text
import Gargantext.Database.Schema.NodeNode (nodesToFavorite)
import Gargantext.API.Node (delDocs, Documents(..))
import Gargantext.Database.Utils (Cmd)
import Gargantext.Database.Schema.Node (HasNodeError)

text :: FacetDoc -> (NodeId, Text)
text (FacetDoc nId _ _ h _ _)  = (nId, title <> "" <> Text.take 100 abstr)
  where
    title = maybe "" identity (_hyperdataDocument_title    h)
    abstr = maybe "" identity (_hyperdataDocument_abstract h)

--moreLike docs_fav docs_trash docs_test = do
data FavTrash = IsFav | IsTrash
  deriving (Eq)

moreLike :: HasNodeError err => FavTrash -> CorpusId -> Cmd err [(NodeId, Maybe Bool)]
moreLike ft cId = do
  let b = if (==) ft IsFav then True else False
  
  docs_trash <- map text <$> runViewDocuments cId True Nothing Nothing Nothing
  docs_fav   <- map text <$> filter (\(FacetDoc _ _ _ _ f _) -> f == True)  <$> runViewDocuments cId False Nothing Nothing Nothing
  docs_test  <- map text <$> filter (\(FacetDoc _ _ _ _ f _) -> f == False) <$> runViewDocuments cId False Nothing Nothing Nothing
  
  let priors = priorEventsWith snd b (  List.zip (repeat False) docs_fav
                                     <> List.zip (repeat True ) docs_trash
                                     )
  
  pure $ filter ((==) (Just $ not b) . snd) $ map (\x -> (fst x, detectDefaultWithPriors snd priors x)) docs_test

learnModify :: HasNodeError err => FavTrash -> CorpusId -> [NodeId] -> Cmd err [Int]
learnModify favTrash cId ns = case favTrash of
      IsFav   -> nodesToFavorite $ map (\n -> (cId, n, True)) ns
      IsTrash -> delDocs cId (Documents ns)

learnAndApply :: HasNodeError err => FavTrash -> CorpusId -> Cmd err [Int]
learnAndApply ft cId = do
  ids <- map fst <$> moreLike ft cId
  learnModify ft cId ids



