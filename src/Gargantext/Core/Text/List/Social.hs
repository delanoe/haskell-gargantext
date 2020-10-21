{-|
Module      : Gargantext.Core.Text.List.Social
Description :
Copyright   : (c) CNRS, 2018-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.Core.Text.List.Social
  where

-- findList imports
import Gargantext.Core.Types.Individu
import Gargantext.Database.Admin.Config
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Prelude
import Gargantext.Database.Query.Table.Node.Error
import Gargantext.Database.Query.Tree
import Gargantext.Database.Query.Tree.Root (getRootId)
import Gargantext.Prelude

-- filterList imports
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Set (Set)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Gargantext.API.Ngrams.Tools -- (getListNgrams)
import Gargantext.API.Ngrams.Types
import Gargantext.Core.Types.Main
import Gargantext.Database.Schema.Ngrams
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Set   as Set

------------------------------------------------------------------------
flowSocialList :: ( RepoCmdM env err m
                  , CmdM     env err m
                  , HasNodeError err
                  , HasTreeError err
                  )
                  => User -> NgramsType -> Set Text
                  -> m (Map ListType (Set Text))
flowSocialList user nt ngrams' = do
  privateLists <- flowSocialListByMode Private user nt ngrams'
  -- printDebug "* privateLists *: \n" privateLists
  -- here preference to privateLists (discutable)
  sharedLists  <- flowSocialListByMode Shared  user nt (termsByList CandidateTerm privateLists)
  -- printDebug "* sharedLists *: \n" sharedLists
   -- TODO publicMapList

  let result = unions [ Map.mapKeys (fromMaybe CandidateTerm) privateLists
                      , Map.mapKeys (fromMaybe CandidateTerm) sharedLists
                      ]
  -- printDebug "* socialLists *: results \n" result
  pure result

------------------------------------------------------------------------
unions :: (Ord a, Semigroup a, Semigroup b, Ord b)
      => [Map a (Set b)] -> Map a (Set b)
unions = invertBack . Map.unionsWith (<>) . map invertForw

invertForw :: (Ord b, Semigroup a) => Map a (Set b) -> Map b a
invertForw = Map.unionsWith (<>)
           . (map (\(k,sets) -> Map.fromSet (\_ -> k) sets))
           . Map.toList

invertBack :: (Ord a, Ord b) => Map b a -> Map a (Set b)
invertBack = Map.fromListWith (<>)
           . (map (\(b,a) -> (a, Set.singleton b)))
           .  Map.toList

unions_test :: Map ListType (Set Text)
unions_test = unions [m1, m2]
  where
    m1 = Map.fromList [ (StopTerm     , Set.singleton "Candidate")]
    m2 = Map.fromList [ (CandidateTerm, Set.singleton "Candidate")
                      , (MapTerm      , Set.singleton "Candidate")
                      ]

------------------------------------------------------------------------
termsByList :: ListType -> (Map (Maybe ListType) (Set Text)) -> Set Text
termsByList CandidateTerm m = Set.unions
                          $ map (\lt -> fromMaybe Set.empty $ Map.lookup lt m)
                          [ Nothing, Just CandidateTerm ]
termsByList l m =
  fromMaybe Set.empty $ Map.lookup (Just l) m


flowSocialListByMode :: ( RepoCmdM env err m
                        , CmdM     env err m
                        , HasNodeError err
                        , HasTreeError err
                        )
                     => NodeMode -> User -> NgramsType -> Set Text
                     -> m (Map (Maybe ListType) (Set Text))
flowSocialListByMode mode user nt ngrams' = do
  listIds <- findListsId mode user
  case listIds of
    [] -> pure $ Map.fromList [(Nothing, ngrams')]
    _  -> do
      counts  <- countFilterList ngrams' nt listIds Map.empty
      -- printDebug "flowSocialListByMode counts" counts
      let r = toSocialList counts ngrams'
      -- printDebug "flowSocialListByMode r" r
      pure r

------------------------------------------------------------------------
-- TODO: maybe use social groups too
toSocialList :: Map Text (Map ListType Int)
             -> Set Text
             -> Map (Maybe ListType) (Set Text)
toSocialList m = Map.fromListWith (<>)
               . Set.toList
               . Set.map (toSocialList1 m)

-- | TODO what if equality ?
-- choice depends on Ord instance of ListType
-- for now : data ListType  =  StopTerm | CandidateTerm | MapTerm
-- means MapTerm > CandidateTerm > StopTerm in case of equality of counts
-- (we minimize errors on MapTerms if doubt)
toSocialList1 :: Map Text (Map ListType Int)
             -> Text
             -> (Maybe ListType, Set Text)
toSocialList1 m t = case Map.lookup t m of
  Nothing -> (Nothing, Set.singleton t)
  Just  m' -> ( (fst . fst) <$> Map.maxViewWithKey m'
              , Set.singleton t
              )

toSocialList1_testIsTrue :: Bool
toSocialList1_testIsTrue = result == (Just MapTerm, Set.singleton token)
  where
    result = toSocialList1 (Map.fromList [(token, m)]) token
    token  = "token"
    m = Map.fromList [ (CandidateTerm, 1)
                     , (MapTerm      , 2)
                     , (StopTerm     , 3)
                     ]

------------------------------------------------------------------------
-- | [ListId] does not merge the lists (it is for Master and User lists
-- here we need UserList only
countFilterList :: RepoCmdM env err m
        => Set Text -> NgramsType -> [ListId]
        -> Map Text (Map ListType Int)
        -> m (Map Text (Map ListType Int))
countFilterList st nt ls input =
  foldM' (\m l -> countFilterList' st nt [l] m) input ls


countFilterList' :: RepoCmdM env err m
        => Set Text -> NgramsType -> [ListId]
        -> Map Text (Map ListType Int)
        -> m (Map Text (Map ListType Int))
countFilterList' st nt ls input = do
  ml <- toMapTextListType <$> getListNgrams ls nt
  -- printDebug "countFilterList'" ml
  pure $ Set.foldl' (\m t -> countList t ml m) input st

------------------------------------------------------------------------
-- FIXME children have to herit the ListType of the parent
toMapTextListType :: Map Text NgramsRepoElement -> Map Text ListType
toMapTextListType m = Map.fromListWith (<>)
                    $ List.concat
                    $ map (toList m)
                    $ Map.toList m

----------------------
-- | Tools to inherit groupings
----------------------
type Parent = Text

parentUnionsMerge :: (Ord a, Ord b, Num c) 
             => [Map a (Map b c)]
             ->  Map a (Map b c)
parentUnionsMerge = Map.unionsWith (Map.unionWith (+))

-- This Parent union is specific
-- [Private, Shared, Public]
-- means the following preferences:
-- Private > Shared > Public
-- if data have not been tagged privately, then use others tags
-- This unions behavior takes first key only and ignore others
parentUnionsExcl :: Ord a
                 => [Map a b]
                 -> Map a b
parentUnionsExcl = Map.unions


hasParent :: Text
          -> Map Text (Map Parent Int)
          -> Maybe Parent
hasParent t m = case Map.lookup t m of
  Nothing  -> Nothing
  Just  m' -> (fst . fst) <$> Map.maxViewWithKey m'


toMapTextParent :: Set Text
                ->  Map Text (Map Parent Int)
                -> [Map Text NgramsRepoElement]
                -> Map Text (Map Parent Int)
toMapTextParent ts = foldl' (toMapTextParent' ts)
  where

    toMapTextParent' :: Set Text
                    -> Map Text (Map Parent Int)
                    -> Map Text NgramsRepoElement
                    -> Map Text (Map Parent Int)
    toMapTextParent' ts' to from = Set.foldl' (toMapTextParent'' ts' from) to ts'


    toMapTextParent'' :: Set Text
                      -> Map Text NgramsRepoElement
                      -> Map Text (Map Parent Int)
                      -> Text
                      -> Map Text (Map Parent Int)
    toMapTextParent'' ss from to t = case Map.lookup t from of
      Nothing  -> to
      Just nre -> case _nre_parent nre of
        Just (NgramsTerm p')  -> if Set.member p' ss
                                    then Map.alter (addParent p') t to
                                    else to
          where
            addParent p'' Nothing   = Just $ addCountParent p'' Map.empty
            addParent p'' (Just ps) = Just $ addCountParent p'' ps

            addCountParent :: Parent -> Map Parent Int -> Map Parent Int
            addCountParent p m = Map.alter addCount p m
              where
                addCount Nothing  = Just 1
                addCount (Just n) = Just $ n + 1

        _ -> to


------------------------------------------------------------------------
toList :: Map Text NgramsRepoElement -> (Text, NgramsRepoElement) -> [(Text, ListType)]
toList m (t, nre@(NgramsRepoElement _ _ _ _ (MSet children))) =
     List.zip terms (List.cycle [lt'])
      where
        terms =  [t]
              -- <> maybe [] (\n -> [unNgramsTerm n]) root
              -- <> maybe [] (\n -> [unNgramsTerm n]) parent
              <> (map unNgramsTerm $ Map.keys children)
        lt'   = listOf m nre

listOf :: Map Text NgramsRepoElement -> NgramsRepoElement -> ListType
listOf m ng = case _nre_parent ng of
  Nothing -> _nre_list ng
  Just  p -> case Map.lookup (unNgramsTerm p) m of
    Just ng' -> listOf m ng'
    Nothing  -> CandidateTerm
    -- panic "[G.C.T.L.Social.listOf] Nothing: Should Not happen"

------------------------------------------------------------------------
countList :: Text
          -> Map Text ListType
          -> Map Text (Map ListType Int)
          -> Map Text (Map ListType Int)
countList t m input = case Map.lookup t m of
  Nothing -> input
  Just l  -> Map.alter addList t input
    where
      addList Nothing   = Just $ addCountList l Map.empty
      addList (Just lm) = Just $ addCountList l lm

addCountList :: ListType -> Map ListType Int -> Map ListType Int
addCountList l m = Map.alter (plus l) l  m
  where
    plus CandidateTerm Nothing  = Just 1
    plus CandidateTerm (Just x) = Just $ x + 1

    plus _ Nothing              = Just 3
    plus _ (Just x)             = Just $ x + 3

------------------------------------------------------------------------
findListsId :: (HasNodeError err, HasTreeError err)
            => NodeMode -> User -> Cmd err [NodeId]
findListsId mode u = do
  r <- getRootId u
  ns <- map _dt_nodeId <$> filter (\n -> _dt_typeId n == nodeTypeId NodeList)
                       <$> findNodes' mode r
  -- printDebug "findListsIds" ns
  pure ns


findNodes' :: HasTreeError err
          => NodeMode -> RootId
          -> Cmd err [DbTreeNode]
findNodes' Private r = findNodes Private r $ [NodeFolderPrivate] <> commonNodes
findNodes' Shared  r = findNodes Shared  r $ [NodeFolderShared ] <> commonNodes
findNodes' Public  r = findNodes Public  r $ [NodeFolderPublic ] <> commonNodes

commonNodes:: [NodeType]
commonNodes = [NodeFolder, NodeCorpus, NodeList]

