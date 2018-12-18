{-|
Module      : Gargantext.API.Ngrams
Description : Server API
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

Ngrams API

-- | TODO
-- get data of NgramsTable
-- post :: update NodeNodeNgrams
-- group ngrams

get ngrams filtered by NgramsType
add get 

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS -fno-warn-orphans #-}

module Gargantext.API.Ngrams
  where

import Prelude (round)
-- import Gargantext.Database.Schema.User  (UserId)
import Data.Functor (($>))
import Data.Patch.Class (Replace, replace)
--import qualified Data.Map.Strict.Patch as PM
import Data.Monoid
--import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
--import Data.Maybe (catMaybes)
-- import qualified Data.Map.Strict as DM
import Data.Map.Strict (Map)
--import qualified Data.Set as Set
import Control.Lens (Prism', prism', (.~), (#))
import Control.Monad (guard)
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Either(Either(Left))
import Data.Map (lookup)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Swagger hiding (version)
import Data.Text (Text)
import GHC.Generics (Generic)
--import Gargantext.Core.Types.Main (Tree(..))
import Gargantext.Core.Utils.Prefix (unPrefix)
import Gargantext.Database.Types.Node (NodeType(..))
import Gargantext.Database.Schema.Node (defaultList, HasNodeError)
import qualified Gargantext.Database.Schema.Ngrams as Ngrams
import Gargantext.Database.Utils (Cmd)
import Gargantext.Prelude
import Gargantext.Core.Types (ListType(..), ListId, CorpusId, Limit, Offset)
import Prelude (Enum, Bounded, minBound, maxBound)
import Servant hiding (Patch)
import Test.QuickCheck (elements)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

------------------------------------------------------------------------
--data FacetFormat = Table | Chart
data TabType   = Docs     | Terms  | Sources | Authors | Institutes | Trash
               | Contacts
  deriving (Generic, Enum, Bounded)

instance FromHttpApiData TabType
  where
    parseUrlPiece "Docs"       = pure Docs
    parseUrlPiece "Terms"      = pure Terms
    parseUrlPiece "Sources"    = pure Sources
    parseUrlPiece "Institutes" = pure Institutes
    parseUrlPiece "Authors"    = pure Authors
    parseUrlPiece "Trash"      = pure Trash
    
    parseUrlPiece "Contacts"   = pure Contacts
    
    parseUrlPiece _            = Left "Unexpected value of TabType"

instance ToParamSchema   TabType
instance ToJSON    TabType
instance FromJSON  TabType
instance ToSchema  TabType
instance Arbitrary TabType
  where
    arbitrary = elements [minBound .. maxBound]

------------------------------------------------------------------------
type NgramsTerm = Text

data NgramsElement =
     NgramsElement { _ne_ngrams      :: NgramsTerm
                   , _ne_list        :: ListType
                   , _ne_occurrences :: Int
                   , _ne_parent      :: Maybe NgramsTerm
                   , _ne_children    :: Set NgramsTerm
                   }
      deriving (Ord, Eq, Show, Generic)
$(deriveJSON (unPrefix "_ne_") ''NgramsElement)

instance ToSchema NgramsElement
instance Arbitrary NgramsElement where
  arbitrary = elements [NgramsElement "sport" GraphList 1 Nothing mempty]

------------------------------------------------------------------------
newtype NgramsTable = NgramsTable { _ngramsTable :: [NgramsElement] }
  deriving (Ord, Eq, Generic, ToJSON, FromJSON, Show)

instance Arbitrary NgramsTable where
  arbitrary = elements
              [ NgramsTable
                [ NgramsElement "animal"  GraphList     1  Nothing       (Set.fromList ["dog", "cat"])
                , NgramsElement "cat"     GraphList     1 (Just "animal") mempty
                , NgramsElement "cats"    StopList      4  Nothing        mempty
                , NgramsElement "dog"     GraphList     3 (Just "animal")(Set.fromList ["dogs"])
                , NgramsElement "dogs"    StopList      4 (Just "dog")    mempty
                , NgramsElement "fox"     GraphList     1  Nothing        mempty
                , NgramsElement "object"  CandidateList 2  Nothing        mempty
                , NgramsElement "nothing" StopList      4  Nothing        mempty
                , NgramsElement "organic" GraphList     3  Nothing        (Set.singleton "flower")
                , NgramsElement "flower"  GraphList     3 (Just "organic") mempty
                , NgramsElement "moon"    CandidateList 1  Nothing         mempty
                , NgramsElement "sky"     StopList      1  Nothing         mempty
                ]
              ]
instance ToSchema NgramsTable

------------------------------------------------------------------------
-- On the Client side:
--data Action = InGroup     NgramsId NgramsId
--            | OutGroup    NgramsId NgramsId
--            | SetListType NgramsId ListType

data PatchSet a = PatchSet
  { _rem :: Set a
  , _add :: Set a
  }
  deriving (Eq, Ord, Show, Generic)

instance (Ord a, Arbitrary a) => Arbitrary (PatchSet a) where
  arbitrary = PatchSet <$> arbitrary <*> arbitrary

instance ToJSON a => ToJSON (PatchSet a) where
  toJSON     = genericToJSON     $ unPrefix "_"
  toEncoding = genericToEncoding $ unPrefix "_"

instance (Ord a, FromJSON a) => FromJSON (PatchSet a) where
  parseJSON = genericParseJSON $ unPrefix "_"

instance ToSchema a => ToSchema (PatchSet a)

instance ToSchema a => ToSchema (Replace a) where
  declareNamedSchema (_ :: proxy (Replace a)) = do
    aSchema <- declareSchemaRef (Proxy :: Proxy a)
    return $ NamedSchema (Just "Replace") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          InsOrdHashMap.fromList
          [ ("old", aSchema)
          , ("new", aSchema)
          ]
      & required .~ [ "old", "new" ]

data NgramsPatch =
     NgramsPatch { _patch_children :: PatchSet NgramsElement
                 , _patch_list     :: Replace ListType   -- TODO Map UserId ListType
                 }
      deriving (Ord, Eq, Show, Generic)
$(deriveJSON (unPrefix "_") ''NgramsPatch)

-- instance Semigroup NgramsPatch where

instance ToSchema  NgramsPatch

instance Arbitrary NgramsPatch where
  arbitrary = NgramsPatch <$> arbitrary <*> (replace <$> arbitrary <*> arbitrary)

-- TODO:
-- * This should be a Map NgramsId NgramsPatch
-- * Patchs -> Patches
newtype NgramsTablePatch =
     NgramsTablePatch { _nip_ngramsIdPatchs :: Map NgramsTerm NgramsPatch }
      deriving (Ord, Eq, Show, Generic, Arbitrary)
$(deriveJSON (unPrefix "_nip_") ''NgramsTablePatch)
instance ToSchema  NgramsTablePatch

-- TODO: replace by mempty once we have the Monoid instance
emptyNgramsTablePatch :: NgramsTablePatch
emptyNgramsTablePatch = NgramsTablePatch mempty

------------------------------------------------------------------------
------------------------------------------------------------------------
type Version = Int

data Versioned a = Versioned
  { _v_version :: Version
  , _v_data    :: a
  }

{-
-- TODO sequencs of modifications (Patchs)
type NgramsIdPatch = Patch NgramsId NgramsPatch

ngramsPatch :: Int -> NgramsPatch
ngramsPatch n = NgramsPatch (DM.fromList [(1, StopList)]) (Set.fromList [n]) Set.empty

toEdit :: NgramsId -> NgramsPatch -> Edit NgramsId NgramsPatch
toEdit n p = Edit n p
ngramsIdPatch :: Patch NgramsId NgramsPatch
ngramsIdPatch = fromList $ catMaybes $ reverse [ replace (1::NgramsId) (Just $ ngramsPatch 1) Nothing
                                       , replace (1::NgramsId) Nothing (Just $ ngramsPatch 2)
                                       , replace (2::NgramsId) Nothing (Just $ ngramsPatch 2)
                                       ]

-- applyPatchBack :: Patch -> IO Patch
-- isEmptyPatch = Map.all (\x -> Set.isEmpty (add_children x) && Set.isEmpty ... )
-}
------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

type TableNgramsApiGet = Summary " Table Ngrams API Get"
                      :> QueryParam "ngramsType"   TabType
                      :> QueryParam "list"   ListId
                      :> QueryParam "limit"  Limit
                      :> QueryParam "offset" Offset
                      :> Get    '[JSON] NgramsTable

type TableNgramsApi = Summary " Table Ngrams API Change"
                      :> QueryParam "list"   ListId
                      :> ReqBody '[JSON] NgramsTablePatch -- (Versioned NgramsTablePatch)
                      :> Put     '[JSON] NgramsTablePatch -- (Versioned NgramsTablePatch)

data NgramError = UnsupportedVersion
  deriving (Show)

class HasNgramError e where
  _NgramError :: Prism' e NgramError

instance HasNgramError ServantErr where
  _NgramError = prism' make match
    where
      err = err500 { errBody = "NgramError: Unsupported version" }
      make UnsupportedVersion = err
      match e = guard (e == err) $> UnsupportedVersion

ngramError :: (MonadError e m, HasNgramError e) => NgramError -> m a
ngramError nne = throwError $ _NgramError # nne

{-
toLists :: ListId -> NgramsTablePatch -> [(ListId, NgramsId, ListTypeId)]
-- toLists = undefined
toLists lId np = [ (lId,ngId,listTypeId lt) | map (toList lId) (_nip_ngramsIdPatchs np) ]

toList :: ListId -> NgramsIdPatch -> (ListId, NgramsId, ListTypeId)
toList = undefined

toGroups :: ListId -> (NgramsPatch -> Set NgramsId) -> NgramsTablePatch -> [NodeNgramsNgrams]
toGroups lId addOrRem ps = concat $ map (toGroup lId addOrRem) $ _nip_ngramsIdPatchs ps

toGroup :: ListId -> (NgramsPatch -> Set NgramsId) -> NgramsIdPatch -> [NodeNgramsNgrams]
-- toGroup = undefined
toGroup lId addOrRem (NgramsIdPatch ngId patch)  =
  map (\ng -> (NodeNgramsNgrams lId ngId ng (Just 1))) (Set.toList $ addOrRem patch)

-}

-- Apply the given patch to the DB and returns the patch to be applied on the
-- cilent.
-- TODO:
-- In this perliminary version the OT aspect is missing, therefore the version
-- number is always 1 and the returned patch is always empty.
tableNgramsPatch :: (HasNgramError err, HasNodeError err)
                 => CorpusId -> Maybe ListId
                 -- -> Versioned NgramsTablePatch
                 -- -> Cmd err (Versioned NgramsTablePatch)
                 -> any
                 -> Cmd err any
tableNgramsPatch _ _ _ = undefined
{-
tableNgramsPatch corpusId maybeList (Versioned version _patch) = do
  when (version /= 1) $ ngramError UnsupportedVersion
  _listId <- maybe (defaultList corpusId) pure maybeList
{-
  _ <- ngramsGroup' Add $ toGroups listId _np_add_children patch
  _ <- ngramsGroup' Del $ toGroups listId _np_rem_children patch
  _ <- updateNodeNgrams (toLists listId patch)
-}
  pure $ Versioned 1 emptyNgramsTablePatch
-}

-- | TODO Errors management
--  TODO: polymorphic for Annuaire or Corpus or ...
getTableNgrams :: HasNodeError err
               => CorpusId -> Maybe TabType
               -> Maybe ListId -> Maybe Limit -> Maybe Offset
               -> Cmd err NgramsTable
getTableNgrams cId maybeTabType maybeListId mlimit moffset = do
  let lieu = "Garg.API.Ngrams: " :: Text
  let ngramsType = case maybeTabType of
        Nothing  -> Ngrams.Sources -- panic (lieu <> "Indicate the Table")
        Just tab -> case tab of
            Sources    -> Ngrams.Sources
            Authors    -> Ngrams.Authors
            Institutes -> Ngrams.Institutes
            Terms      -> Ngrams.NgramsTerms
            _          -> panic $ lieu <> "No Ngrams for this tab"

  listId <- maybe (defaultList cId) pure maybeListId

  let
    defaultLimit = 10 -- TODO
    limit_  = maybe defaultLimit identity mlimit
    offset_ = maybe 0 identity moffset

  (ngramsTableDatas, mapToParent, mapToChildren) <-
    Ngrams.getNgramsTableDb NodeDocument ngramsType (Ngrams.NgramsTableParam listId cId) limit_ offset_

  -- printDebug "ngramsTableDatas" ngramsTableDatas

  pure $ NgramsTable $ map (\(Ngrams.NgramsTableData ngs _ lt w) ->
                              NgramsElement ngs
                                            (maybe (panic $ lieu <> "listType") identity lt)
                                            (round w)
                                            (lookup ngs mapToParent)
                                            (maybe mempty identity $ lookup ngs mapToChildren)
                           ) ngramsTableDatas


