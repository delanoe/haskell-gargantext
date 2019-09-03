{-|
Module      : Gargantext.Database.Learn
Description : Learn Small Data Analytics with big data connection (DB)
opyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}

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
--import Gargantext.Database.Schema.NodeNode (nodeNodesCategory)
import Gargantext.Database.Utils (Cmd)
import Gargantext.Core.Types (Offset, Limit)

data FavOrTrash = IsFav | IsTrash
  deriving (Eq)


moreLike :: CorpusId   -> Maybe Offset -> Maybe Limit -> Maybe OrderBy
         -> FavOrTrash -> Cmd err [FacetDoc]
moreLike cId o l order ft = do
  priors <- getPriors ft cId
  moreLikeWith cId o l order ft priors

---------------------------------------------------------------------------
getPriors :: FavOrTrash -> CorpusId -> Cmd err (Events Bool)
getPriors ft cId = do
  
  docs_fav   <- filter (\(FacetDoc _ _ _ _ f _) -> f == Just 2)
              <$> runViewDocuments cId False Nothing Nothing Nothing
  
  docs_trash <- List.take (List.length docs_fav)
            <$> runViewDocuments cId True Nothing Nothing Nothing
  

  let priors = priorEventsWith text (fav2bool ft) (  List.zip (repeat False) docs_fav
                                      <> List.zip (repeat True ) docs_trash
                                      )
  pure priors


moreLikeWith :: CorpusId   -> Maybe Offset -> Maybe Limit -> Maybe OrderBy
             -> FavOrTrash -> Events Bool  -> Cmd err [FacetDoc]
moreLikeWith cId o l order ft priors = do

  docs_test  <- filter (\(FacetDoc _ _ _ _ f _) -> f == Just 1)
            <$> runViewDocuments cId False o Nothing order

  let results = map fst
       $ filter ((==) (Just $ not $ fav2bool ft) . snd)
       $ map (\f -> (f, detectDefaultWithPriors text priors f)) docs_test

  pure $ List.take (maybe 10 identity l) results

---------------------------------------------------------------------------
fav2bool :: FavOrTrash -> Bool
fav2bool ft = if (==) ft IsFav then True else False


text :: FacetDoc -> Text
text (FacetDoc _ _ _ h _ _)  = title <> "" <> Text.take 100 abstr
  where
    title = maybe "" identity (_hyperdataDocument_title    h)
    abstr = maybe "" identity (_hyperdataDocument_abstract h)

---------------------------------------------------------------------------

{-
apply :: (FlowCmdM env e m) => FavOrTrash -> CorpusId -> [NodeId] -> m [Int]
apply favTrash cId ns = case favTrash of
      IsFav   -> nodeNodesCategory $ map (\n -> (cId, n, 2)) ns
      IsTrash -> nodeNodesCategory $ map (\n -> (cId, n, 0)) ns

moreLikeAndApply :: FlowCmdM DevEnv GargError m => FavOrTrash -> CorpusId -> m [Int]
moreLikeAndApply ft cId = do
  priors <- getPriors ft cId
  moreLikeWithAndApply priors ft cId

moreLikeWithAndApply :: FlowCmdM DevEnv GargError m => Events Bool -> FavOrTrash -> CorpusId -> m [Int]
moreLikeWithAndApply priors ft cId = do
  ids <- map facetDoc_id <$> moreLikeWith cId ft priors
  apply ft cId ids
-}
