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
import Gargantext.Database.Schema.NodeNode (nodesToFavorite)
import Gargantext.API.Node (delDocs, Documents(..))
--import Gargantext.Database.Utils (Cmd)
--import Gargantext.Database.Schema.Node (HasNodeError)
import Gargantext.API
import Gargantext.API.Settings
import Gargantext.Database.Flow (FlowCmdM)

data FavOrTrash = IsFav | IsTrash
  deriving (Eq)



--moreLike :: FlowCmdM env error m => FavOrTrash -> CorpusId -> m (Events Bool, [FacetDoc])


moreLike :: FlowCmdM DevEnv GargError m => FavOrTrash -> CorpusId -> m [FacetDoc]
moreLike ft cId = do
  priors <- getPriors ft cId
  moreLikeWith priors ft cId


---------------------------------------------------------------------------
getPriors :: FlowCmdM DevEnv GargError m => FavOrTrash -> CorpusId -> m (Events Bool)
getPriors ft cId = do
  docs_trash <- runViewDocuments cId True Nothing Nothing Nothing
  
  docs_fav   <- filter (\(FacetDoc _ _ _ _ f _) -> f == True) 
              <$> runViewDocuments cId False Nothing Nothing Nothing
  

  let priors = priorEventsWith text (fav2bool ft) (  List.zip (repeat False) docs_fav
                                      <> List.zip (repeat True ) docs_trash
                                      )
  pure priors


moreLikeWith :: FlowCmdM DevEnv GargError m => Events Bool -> FavOrTrash -> CorpusId -> m [FacetDoc]
moreLikeWith priors ft cId = do
  
  docs_test  <- filter (\(FacetDoc _ _ _ _ f _) -> f == False)
              <$> runViewDocuments cId False Nothing Nothing Nothing

  let results = map fst
       $ filter ((==) (Just $ not $ fav2bool ft) . snd)
       $ map (\f -> (f, detectDefaultWithPriors text priors f)) docs_test

  pure results

---------------------------------------------------------------------------
fav2bool :: FavOrTrash -> Bool
fav2bool ft = if (==) ft IsFav then True else False


text :: FacetDoc -> Text
text (FacetDoc _ _ _ h _ _)  = title <> "" <> Text.take 100 abstr
  where
    title = maybe "" identity (_hyperdataDocument_title    h)
    abstr = maybe "" identity (_hyperdataDocument_abstract h)

---------------------------------------------------------------------------


apply :: (FlowCmdM DevEnv GargError m) => FavOrTrash -> CorpusId -> [NodeId] -> m [Int]
apply favTrash cId ns = case favTrash of
      IsFav   -> nodesToFavorite $ map (\n -> (cId, n, True)) ns
      IsTrash -> delDocs cId (Documents ns)


moreLikeAndApply :: FlowCmdM DevEnv GargError m => FavOrTrash -> CorpusId -> m [Int]
moreLikeAndApply ft cId = do
  priors <- getPriors ft cId
  moreLikeWithAndApply priors ft cId


moreLikeWithAndApply :: FlowCmdM DevEnv GargError m => Events Bool -> FavOrTrash -> CorpusId -> m [Int]
moreLikeWithAndApply priors ft cId = do
  ids <- map facetDoc_id <$> moreLikeWith priors ft cId
  apply ft cId ids


