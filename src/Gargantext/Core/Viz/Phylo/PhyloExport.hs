{-|
Module      : Gargantext.Core.Viz.Phylo.PhyloExport
Description : Exportation module of a Phylo
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TypeSynonymInstances #-}

module Gargantext.Core.Viz.Phylo.PhyloExport where

import Control.Lens hiding (Level)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.GraphViz hiding (DotGraph, Order)
import Data.GraphViz.Attributes.Complete hiding (EdgeType, Order)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic
import Data.List ((++), sort, nub, null, concat, sortOn, groupBy, union, (\\), (!!), init, partition, notElem, unwords, nubBy, inits, elemIndex)
import Data.Map (Map, fromList, empty, fromListWith, insert, (!), elems, unionWith, findWithDefault, toList, member)
import Data.Text.Lazy (fromStrict, pack, unpack)
import Data.Vector (Vector)
import Debug.Trace (trace)
import Gargantext.Core.Viz.Phylo
import Gargantext.Core.Viz.Phylo.PhyloTools
import Gargantext.Core.Viz.Phylo.TemporalMatching (filterDocs, filterDiago, reduceDiagos, toProximity, getNextPeriods)
import Gargantext.Prelude hiding (scale)
import Prelude (writeFile)
import System.FilePath
import qualified Data.GraphViz.Attributes.HTML as H
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Vector as Vector

--------------------
-- | Dot export | --
--------------------

dotToFile :: FilePath -> DotGraph DotId -> IO ()
dotToFile filePath dotG = writeFile filePath $ dotToString dotG

dotToString :: DotGraph DotId  -> [Char]
dotToString dotG = unpack (printDotGraph dotG)

dynamicToColor :: Double -> H.Attribute
dynamicToColor d
  | d == 0    = H.BGColor (toColor LightCoral)
  | d == 1    = H.BGColor (toColor Khaki)
  | d == 2    = H.BGColor (toColor SkyBlue)
  | otherwise = H.Color   (toColor Black)

pickLabelColor :: [Double] -> H.Attribute
pickLabelColor lst
  | elem 0 lst = dynamicToColor 0
  | elem 2 lst = dynamicToColor 2
  | elem 1 lst = dynamicToColor 1
  | otherwise  = dynamicToColor 3

toDotLabel :: Text.Text -> Label
toDotLabel lbl = StrLabel $ fromStrict lbl

toAttr :: AttributeName -> Lazy.Text -> CustomAttribute
toAttr k v = customAttribute k v

metaToAttr :: Map Text.Text [Double] -> [CustomAttribute]
metaToAttr meta = map (\(k,v) -> toAttr (fromStrict k) $ (pack . unwords) $ map show v) $ toList meta

groupIdToDotId :: PhyloGroupId -> DotId
groupIdToDotId (((d,d'),lvl),idx) = (fromStrict . Text.pack) $ ("group" <> (show d) <> (show d') <> (show lvl) <> (show idx))

branchIdToDotId :: PhyloBranchId -> DotId
branchIdToDotId bId = (fromStrict . Text.pack) $ ("branch" <> show (snd bId))

periodIdToDotId :: Period -> DotId
periodIdToDotId prd = (fromStrict . Text.pack) $ ("period" <> show (fst prd) <> show (snd prd))

groupToTable :: Vector Ngrams -> PhyloGroup -> H.Label
groupToTable fdt g = H.Table H.HTable
                    { H.tableFontAttrs = Just [H.PointSize 14, H.Align H.HLeft]
                    , H.tableAttrs = [H.Border 0, H.CellBorder 0, H.BGColor (toColor White)]
                    , H.tableRows = [header]
                                 <> [H.Cells [H.LabelCell [H.Height 10] $ H.Text [H.Str $ fromStrict ""]]]
                                 <> ( map ngramsToRow $ splitEvery 4
                                    $ reverse $ sortOn (snd . snd)
                                    $ zip (ngramsToText fdt (g ^. phylo_groupNgrams))
                                    $ zip ((g ^. phylo_groupMeta) ! "dynamics") ((g ^. phylo_groupMeta) ! "inclusion"))}
    where
        --------------------------------------
        ngramsToRow :: [(Ngrams,(Double,Double))] -> H.Row
        ngramsToRow ns = H.Cells $ map (\(n,(d,_)) ->
                            H.LabelCell [H.Align H.HLeft,dynamicToColor d] $ H.Text [H.Str $ fromStrict n]) ns
        --------------------------------------
        header :: H.Row
        header =
            H.Cells [ H.LabelCell [pickLabelColor ((g ^. phylo_groupMeta) ! "dynamics")]
                    $ H.Text [H.Str $ (((fromStrict . Text.toUpper) $ g ^. phylo_groupLabel)
                                   <> (fromStrict " ( ")
                                   <> (pack $ show (fst $ g ^. phylo_groupPeriod))
                                   <> (fromStrict " , ")
                                   <> (pack $ show (snd $ g ^. phylo_groupPeriod))
                                   <> (fromStrict " ) ")
                                   <> (pack $ show (getGroupId g)))]]
        --------------------------------------

branchToDotNode :: PhyloBranch -> Int -> Dot DotId
branchToDotNode b bId =
    node (branchIdToDotId $ b ^. branch_id)
         ([FillColor [toWColor CornSilk], FontName "Arial", FontSize 40, Shape Egg, Style [SItem Bold []], Label (toDotLabel $ b ^. branch_label)]
         <> (metaToAttr $ b ^. branch_meta)
         <> [ toAttr "nodeType" "branch"
            , toAttr "bId"      (pack $ show bId)
            , toAttr "branchId" (pack $ unwords (map show $ snd $ b ^. branch_id))
            , toAttr "branch_x" (fromStrict $ Text.pack $ (show $ b ^. branch_x))
            , toAttr "branch_y" (fromStrict $ Text.pack $ (show $ b ^. branch_y))
            , toAttr "label"    (pack $ show $ b ^. branch_label)
            ])

periodToDotNode :: (Date,Date) -> (Text.Text,Text.Text) -> Dot DotId
periodToDotNode prd prd' =
    node (periodIdToDotId prd)
         ([Shape BoxShape, FontSize 50, Label (toDotLabel $ Text.pack (show (fst prd) <> " " <> show (snd prd)))]
         <> [ toAttr "nodeType" "period"
            , toAttr "strFrom" (fromStrict $ Text.pack $ (show $ fst prd'))
            , toAttr "strTo"   (fromStrict $ Text.pack $ (show $ snd prd'))
            , toAttr "from" (fromStrict $ Text.pack $ (show $ fst prd))
            , toAttr "to"   (fromStrict $ Text.pack $ (show $ snd prd))])


groupToDotNode :: Vector Ngrams -> PhyloGroup -> Int -> Dot DotId
groupToDotNode fdt g bId =
    node (groupIdToDotId $ getGroupId g)
                     ([FontName "Arial", Shape Square, penWidth 4,  toLabel (groupToTable fdt g)]
                      <> [ toAttr "nodeType" "group"
                         , toAttr "gid" (groupIdToDotId $ getGroupId g)
                         , toAttr "from" (pack $ show (fst $ g ^. phylo_groupPeriod))
                         , toAttr "to"   (pack $ show (snd $ g ^. phylo_groupPeriod))
                         , toAttr "strFrom" (pack $ show (fst $ g ^. phylo_groupPeriod'))
                         , toAttr "strTo"   (pack $ show (snd $ g ^. phylo_groupPeriod'))
                         , toAttr "branchId" (pack $ unwords (init $ map show $ snd $ g ^. phylo_groupBranchId))
                         , toAttr "bId" (pack $ show bId)
                         , toAttr "support" (pack $ show (g ^. phylo_groupSupport))
                         , toAttr "weight" (pack $ show (g ^. phylo_groupWeight))
                         , toAttr "source" (pack $ show (nub $ g ^. phylo_groupSources))
                         , toAttr "sourceFull" (pack $ show (g ^. phylo_groupSources))
                         , toAttr "lbl" (pack $ show (ngramsToLabel fdt (g ^. phylo_groupNgrams)))
                         , toAttr "foundation" (pack $ show (idxToLabel (g ^. phylo_groupNgrams)))
                         , toAttr "role" (pack $ show (idxToLabel' ((g ^. phylo_groupMeta) ! "dynamics")))
                         , toAttr "frequence" (pack $ show (idxToLabel' ((g ^. phylo_groupMeta) ! "frequence")))
                         , toAttr "seaLvl" (pack $ show ((g ^. phylo_groupMeta) ! "seaLevels"))
                         ])


toDotEdge' :: DotId -> DotId -> [Char] -> [Char] -> EdgeType -> Dot DotId
toDotEdge' source target thr w edgeType = edge source target
    (case edgeType of
        GroupToGroup    -> undefined
        GroupToGroupMemory  -> [ Width 3, penWidth 4, Color [toWColor Black], Constraint True] <> [toAttr "edgeType" "memoryLink", toAttr "thr" (pack thr), toAttr "weight" (pack w)]
        BranchToGroup   -> undefined
        BranchToBranch  -> undefined
        GroupToAncestor -> undefined
        PeriodToPeriod  -> undefined)


toDotEdge :: DotId -> DotId -> [Char] -> EdgeType -> Dot DotId
toDotEdge source target lbl edgeType = edge source target
    (case edgeType of
        GroupToGroup    -> [ Width 3, penWidth 4, Color [toWColor Black], Constraint True] <> [toAttr "edgeType" "link", toAttr "lbl" (pack lbl), toAttr "source" source, toAttr "target" target]
        GroupToGroupMemory  -> undefined
        BranchToGroup   -> [ Width 3, Color [toWColor Black], ArrowHead (AType [(ArrMod FilledArrow RightSide,DotArrow)])] <> [toAttr "edgeType" "branchLink" ]
        BranchToBranch  -> [ Width 2, Color [toWColor Black], Style [SItem Dashed []], ArrowHead (AType [(ArrMod FilledArrow BothSides,DotArrow)])]
        GroupToAncestor -> [ Width 3, Color [toWColor Red], Style [SItem Dashed []], ArrowHead (AType [(ArrMod FilledArrow BothSides,NoArrow)]), PenWidth 4] <> [toAttr "edgeType" "ancestorLink", toAttr "lbl" (pack lbl), toAttr "source" source, toAttr "target" target]
        PeriodToPeriod  -> [ Width 5, Color [toWColor Black]])


mergePointers :: [PhyloGroup] -> Map (PhyloGroupId,PhyloGroupId) Double
mergePointers groups =
    let toChilds  = fromList $ concat $ map (\g -> map (\(target,w) -> ((getGroupId g,target),w)) $ g ^. phylo_groupPeriodChilds) groups
        toParents = fromList $ concat $ map (\g -> map (\(target,w) -> ((target,getGroupId g),w)) $ g ^. phylo_groupPeriodParents) groups
    in  unionWith (\w w' -> max w w') toChilds toParents

mergePointersMemory :: [PhyloGroup] -> [((PhyloGroupId,PhyloGroupId),(Double,Double))]
mergePointersMemory groups =
    let toChilds  = concat $ map (\g -> map (\(target,(t,w)) -> ((getGroupId g,target),(t,w))) $ g ^. phylo_groupPeriodMemoryChilds) groups
        toParents = concat $ map (\g -> map (\(target,(t,w)) -> ((target,getGroupId g),(t,w))) $ g ^. phylo_groupPeriodMemoryParents) groups
    in  concat [toChilds,toParents]

mergeAncestors :: [PhyloGroup] -> [((PhyloGroupId,PhyloGroupId), Double)]
mergeAncestors groups = concat
                      $ map (\g -> map (\(target,w) -> ((getGroupId g,target),w)) $ g ^. phylo_groupAncestors)
                      $ filter (\g -> (not . null) $ g ^. phylo_groupAncestors) groups


toBid :: PhyloGroup -> [PhyloBranch] -> Int
toBid g bs =
  let b' = head' "toBid" (filter (\b -> b ^. branch_id == g ^. phylo_groupBranchId) bs)
   in fromJust $ elemIndex b' bs

exportToDot :: Phylo -> PhyloExport -> DotGraph DotId
exportToDot phylo export =
    trace ("\n-- | Convert " <> show(length $ export ^. export_branches) <> " branches and "
         <> show(length $ export ^. export_groups) <> " groups "
         <> show(length $ nub $ concat $ map (\g -> g ^. phylo_groupNgrams) $ export ^. export_groups) <> " terms to a dot file\n\n"
         <> "##########################") $
    digraph ((Str . fromStrict) $ (phyloName $ getConfig phylo)) $ do

        {- 1) init the dot graph -}
        graphAttrs ( [ Label (toDotLabel $ (phyloName $ getConfig phylo))]
                  <> [ FontSize 30, LabelLoc VTop, NodeSep 1, RankSep [1], Rank SameRank, Splines SplineEdges, Overlap ScaleOverlaps
                     , Ratio FillRatio
                     -- , Ratio AutoRatio
                     , Style [SItem Filled []],Color [toWColor White]]
                  {-- home made attributes -}
                  <> [(toAttr (fromStrict "phyloFoundations") $ pack $ show (length $ Vector.toList $ getRoots phylo))
                     ,(toAttr (fromStrict "phyloTerms") $ pack $ show (length $ nub $ concat $ map (\g -> g ^. phylo_groupNgrams) $ export ^. export_groups))
                     ,(toAttr (fromStrict "phyloDocs") $ pack $ show (sum $ elems $ phylo ^. phylo_timeDocs))
                     ,(toAttr (fromStrict "phyloPeriods") $ pack $ show (length $ elems $ phylo ^. phylo_periods))
                     ,(toAttr (fromStrict "phyloBranches") $ pack $ show (length $ export ^. export_branches))
                     ,(toAttr (fromStrict "phyloGroups") $ pack $ show (length $ export ^. export_groups))
                     ,(toAttr (fromStrict "phyloSources") $ pack $ show (Vector.toList $ getSources phylo))
                     ,(toAttr (fromStrict "phyloTimeScale") $ pack $ getTimeScale phylo)
                     ,(toAttr (fromStrict "PhyloScale") $ pack $ show (_qua_granularity $ phyloQuality $ getConfig phylo))
                     ,(toAttr (fromStrict "phyloQuality") $ pack $ show (phylo ^. phylo_quality))
                     ,(toAttr (fromStrict "phyloSeaRiseStart") $ pack $ show (getPhyloSeaRiseStart phylo))
                     ,(toAttr (fromStrict "phyloSeaRiseSteps") $ pack $ show (getPhyloSeaRiseSteps phylo))
                     -- ,(toAttr (fromStrict "phyloTermsFreq") $ pack $ show (toList $ _phylo_lastTermFreq phylo))
                     ])

{-
 -- toAttr (fromStrict k) $ (pack . unwords) $ map show v

        --  2) create a layer for the branches labels -}
        subgraph (Str "Branches peaks") $ do

            -- graphAttrs [Rank SameRank]
{-
            --  3) group the branches by hierarchy
            -- mapM (\branches ->
            --         subgraph (Str "Branches clade") $ do
            --             graphAttrs [Rank SameRank]

            --             --  4) create a node for each branch
            --             mapM branchToDotNode branches
            --     ) $ elems $ fromListWith (++) $ map (\b -> ((init . snd) $ b ^. branch_id,[b])) $ export ^. export_branches
-}
            mapM (\b -> branchToDotNode b (fromJust $ elemIndex b (export ^. export_branches))) $ export ^. export_branches

        {--  5) create a layer for each period -}
        _ <- mapM (\period ->
                subgraph ((Str . fromStrict . Text.pack) $ ("Period" <> show (fst $ _phylo_periodPeriod period) <> show (snd $ _phylo_periodPeriod period))) $ do
                    graphAttrs [Rank SameRank]
                    periodToDotNode (period ^. phylo_periodPeriod) (period ^. phylo_periodPeriodStr)

                    {--  6) create a node for each group -}
                    mapM (\g -> groupToDotNode (getRoots phylo) g (toBid g (export ^. export_branches))) (filter (\g -> g ^. phylo_groupPeriod == (period ^. phylo_periodPeriod)) $ export ^. export_groups)
            ) $ phylo ^. phylo_periods

        {--  7) create the edges between a branch and its first groups -}
        _ <- mapM (\(bId,groups) ->
                mapM (\g -> toDotEdge (branchIdToDotId bId) (groupIdToDotId $ getGroupId g) "" BranchToGroup) groups
             )
           $ toList
           $ map (\groups -> head' "toDot"
                           $ groupBy (\g g' -> g' ^. phylo_groupPeriod == g ^. phylo_groupPeriod)
                           $ sortOn (fst . _phylo_groupPeriod) groups)
           $ fromListWith (++) $ map (\g -> (g ^. phylo_groupBranchId,[g])) $ export ^. export_groups

        {-  8) create the edges between the groups -}
        _ <- mapM (\((k,k'),v) ->
                toDotEdge (groupIdToDotId k) (groupIdToDotId k') (show v) GroupToGroup
            ) $ (toList . mergePointers) $ export ^. export_groups

        {-  8-bis) create the edges between the groups -}
{-        _ <- mapM (\((k,k'),v) ->
                toDotEdge' (groupIdToDotId k) (groupIdToDotId k') (show (fst v)) (show (snd v)) GroupToGroupMemory
            ) $ mergePointersMemory $ export ^. export_groups       -}

        _ <- mapM (\((k,k'),v) ->
                toDotEdge (groupIdToDotId k) (groupIdToDotId k') (show v) GroupToAncestor
          ) $ mergeAncestors $ export ^. export_groups

        -- 10) create the edges between the periods
        _ <- mapM (\(prd,prd') ->
                toDotEdge (periodIdToDotId prd) (periodIdToDotId prd') "" PeriodToPeriod
            ) $ nubBy (\combi combi' -> fst combi == fst combi') $ listToCombi' $ getPeriodIds phylo

        {-  8) create the edges between the branches
        -- _ <- mapM (\(bId,bId') ->
        --         toDotEdge (branchIdToDotId bId) (branchIdToDotId bId')
        --         (Text.pack $ show(branchIdsToProximity bId bId'
        --                             (getThresholdInit $ phyloProximity $ getConfig phylo)
        --                             (getThresholdStep $ phyloProximity $ getConfig phylo))) BranchToBranch
        --     ) $ nubBy (\combi combi' -> fst combi == fst combi') $ listToCombi' $ map _branch_id $ export ^. export_branches
        -}


        graphAttrs [Rank SameRank]


----------------
-- | Filter | --
----------------

filterByBranchSize :: Double -> PhyloExport -> PhyloExport
filterByBranchSize thr export =
    let splited  = partition (\b -> head' "filter" ((b ^. branch_meta) ! "size") >= thr) $ export ^. export_branches
     in export & export_branches .~ (fst splited)
               & export_groups %~ (filter (\g -> not $ elem  (g ^. phylo_groupBranchId) (map _branch_id $ snd splited)))


processFilters :: [Filter] -> Quality -> PhyloExport -> PhyloExport
processFilters filters qua export =
    foldl (\export' f -> case f of
                ByBranchSize thr -> if (thr < (fromIntegral $ qua ^. qua_minBranch))
                                      then filterByBranchSize (fromIntegral $ qua ^. qua_minBranch) export'
                                      else filterByBranchSize thr export'
        ) export filters

--------------
-- | Sort | --
--------------

branchToIso :: [PhyloBranch] -> [PhyloBranch]
branchToIso branches =
    let steps = map sum
              $ inits
              $ map (\(b,x) -> b ^. branch_y + 0.05 - x)
              $ zip branches
              $ ([0] ++ (map (\(b,b') ->
                                 let idx = length $ commonPrefix (b ^. branch_canonId) (b' ^. branch_canonId) []
                                     lmin = min (length $ b ^. branch_seaLevel)  (length $ b' ^. branch_seaLevel)
                                  in
                                    if ((idx - 1) > ((length $ b' ^. branch_seaLevel) - 1))
                                      then (b' ^. branch_seaLevel) !! (lmin - 1)
                                      else (b' ^. branch_seaLevel) !! (idx - 1)
                                 ) $ listToSeq branches))
     in map (\(x,b) -> b & branch_x .~ x)
      $ zip steps branches

branchToIso' :: Double -> Double -> [PhyloBranch] -> [PhyloBranch]
branchToIso' start step branches =
  let bx = map (\l -> (sum l) + ((fromIntegral $ length l) * 0.5))
         $ inits
         $ ([0] ++ (map (\(b,b') ->
                      let root = fromIntegral $ length $ commonPrefix (snd $ b ^. branch_id) (snd $ b' ^. branch_id) []
                      in 1 - start - step * root) $ listToSeq branches))
  in map (\(x,b) -> b & branch_x .~ x)
   $ zip bx branches


sortByHierarchy :: Int -> [PhyloBranch] -> [PhyloBranch]
sortByHierarchy depth branches =
    if (length branches == 1)
        then branches
        else concat
           $ map (\branches' ->
                    let partitions = partition (\b -> depth + 1 == ((length . snd) $ b ^. branch_id)) branches'
                    in  (sortOn (\b -> (b ^. branch_meta) ! "birth") (fst partitions))
                    ++  (sortByHierarchy (depth + 1) (snd partitions)))
            $ groupBy (\b b' -> ((take depth . snd) $ b ^. branch_id) == ((take depth . snd) $ b' ^. branch_id) )
            $ sortOn (\b -> (take depth . snd) $ b ^. branch_id) branches


sortByBirthDate :: Order -> PhyloExport -> PhyloExport
sortByBirthDate order export =
    let branches  = sortOn (\b -> (b ^. branch_meta) ! "birth") $ export ^. export_branches
        branches' = case order of
                    Asc  -> branches
                    Desc -> reverse branches
    in  export & export_branches .~ branches'

processSort :: Sort -> SeaElevation -> PhyloExport -> PhyloExport
processSort sort' elev export = case sort' of
    ByBirthDate o -> sortByBirthDate o export
    ByHierarchy _ -> case elev of
            Constante  s s' ->  export & export_branches .~ (branchToIso' s s' $ sortByHierarchy 0 (export ^. export_branches))
            Adaptative _ ->  export & export_branches .~ (branchToIso $ sortByHierarchy 0 (export ^. export_branches))       

-----------------
-- | Metrics | --
-----------------

-- | Return the conditional probability of i knowing j
conditional :: Ord a => Map (a,a) Double -> a -> a -> Double
conditional m i j = (findWithDefault 0 (i,j) m)
                  / (m ! (j,j))


-- | Return the genericity score of a given ngram
genericity :: Map (Int, Int) Double -> [Int] -> Int -> Double
genericity m l i = ( (sum $ map (\j -> conditional m i j) l)
                   - (sum $ map (\j -> conditional m j i) l)) / (fromIntegral $ (length l) + 1)


-- | Return the specificity score of a given ngram
specificity :: Map (Int, Int) Double -> [Int] -> Int -> Double
specificity m l i = ( (sum $ map (\j -> conditional m j i) l)
                    - (sum $ map (\j -> conditional m i j) l)) / (fromIntegral $ (length l) + 1)


-- | Return the inclusion score of a given ngram
inclusion :: Map (Int, Int) Double -> [Int] -> Int -> Double
inclusion m l i = ( (sum $ map (\j -> conditional m j i) l)
                  + (sum $ map (\j -> conditional m i j) l)) / (fromIntegral $ (length l) + 1)


ngramsMetrics :: Phylo -> PhyloExport -> PhyloExport
ngramsMetrics phylo export =
    over ( export_groups
         .  traverse )
    (\g -> g & phylo_groupMeta %~ insert "genericity"
                                  (map (\n -> genericity  (g ^. phylo_groupCooc) ((g ^. phylo_groupNgrams) \\ [n]) n) $ g ^. phylo_groupNgrams)
             & phylo_groupMeta %~ insert "specificity"
                                  (map (\n -> specificity (g ^. phylo_groupCooc) ((g ^. phylo_groupNgrams) \\ [n]) n) $ g ^. phylo_groupNgrams)
             & phylo_groupMeta %~ insert "inclusion"
                                  (map (\n -> inclusion   (g ^. phylo_groupCooc) ((g ^. phylo_groupNgrams) \\ [n]) n) $ g ^. phylo_groupNgrams)
             & phylo_groupMeta %~ insert "frequence"
                                  (map (\n -> getInMap n (phylo ^. phylo_lastTermFreq)) $ g ^. phylo_groupNgrams)
        ) export


branchDating :: PhyloExport -> PhyloExport
branchDating export =
    over ( export_branches
         .  traverse )
    (\b ->
        let groups = sortOn fst
                   $ foldl' (\acc g -> if (g ^. phylo_groupBranchId == b ^. branch_id)
                                      then acc ++ [g ^. phylo_groupPeriod]
                                      else acc ) [] $ export ^. export_groups
            periods = nub groups
            birth = fst $ head' "birth" groups
            age   = (snd $ last' "age"  groups) - birth
        in b & branch_meta %~ insert "birth" [fromIntegral birth]
             & branch_meta %~ insert "age"   [fromIntegral age]
             & branch_meta %~ insert "size"  [fromIntegral $ length periods] ) export

processMetrics :: Phylo -> PhyloExport -> PhyloExport
processMetrics phylo export = ngramsMetrics phylo
                            $ branchDating export


-----------------
-- | Taggers | --
-----------------

nk :: Int -> [[Int]] -> Int
nk n groups = sum
            $ map (\g -> if (elem n g)
                          then 1
                          else 0) groups


tf :: Int -> [[Int]] -> Double
tf n groups = (fromIntegral $ nk n groups) / (fromIntegral $ length $ concat groups)


idf :: Int -> [[Int]] -> Double
idf n groups = log ((fromIntegral $ length groups) / (fromIntegral $ nk n groups))


findTfIdf :: [[Int]] -> [(Int,Double)]
findTfIdf groups = reverse $ sortOn snd $ map (\n -> (n,(tf n groups) * (idf n groups))) $ sort $ nub $ concat groups


findEmergences :: [PhyloGroup] -> Map Int Double -> [(Int,Double)]
findEmergences groups freq =
  let ngrams = map _phylo_groupNgrams groups
      dynamics = map (\g -> (g ^. phylo_groupMeta) ! "dynamics") groups
      emerging = nubBy (\n1 n2 -> fst n1 == fst n2)
               $ concat $ map (\g -> filter (\(_,d) -> d == 0) $ zip (fst g) (snd g)) $ zip ngrams dynamics
  in reverse $ sortOn snd
   $ map (\(n,_) -> if (member n freq)
                      then (n,freq ! n)
                      else (n,0)) emerging


mostEmergentTfIdf :: Int -> Map Int Double -> Vector Ngrams -> PhyloExport -> PhyloExport
mostEmergentTfIdf nth freq foundations export =
    over ( export_branches
         .  traverse )
         (\b ->
            let groups = filter (\g -> g ^. phylo_groupBranchId == b ^. branch_id) $ export ^. export_groups
                tfidf  = findTfIdf (map _phylo_groupNgrams groups)
                emergences = findEmergences groups freq
                selected = if (null emergences)
                            then map fst $ take nth tfidf
                            else [fst $ head' "mostEmergentTfIdf" emergences]
                              ++ (map fst $ take (nth - 1) $ filter (\(n,_) -> n /= (fst $ head' "mostEmergentTfIdf" emergences)) tfidf)
            in b & branch_label .~ (ngramsToLabel foundations selected)) export


getNthMostMeta :: Int -> [Double] -> [Int] -> [Int]
getNthMostMeta nth meta ns = map (\(idx,_) -> (ns !! idx))
                           $ take nth
                           $ reverse
                           $ sortOn snd $ zip [0..] meta


mostInclusive :: Int -> Vector Ngrams -> PhyloExport -> PhyloExport
mostInclusive nth foundations export =
    over ( export_branches
         .  traverse )
         (\b ->
            let groups = filter (\g -> g ^. phylo_groupBranchId == b ^. branch_id) $ export ^. export_groups
                cooc   = foldl (\acc g -> unionWith (+) acc (g ^. phylo_groupCooc)) empty groups
                ngrams = sort $ foldl (\acc g -> union acc (g ^. phylo_groupNgrams)) [] groups
                inc    = map (\n -> inclusion cooc (ngrams \\ [n]) n) ngrams
                lbl    = ngramsToLabel foundations $ getNthMostMeta nth inc ngrams
            in b & branch_label .~ lbl ) export


mostEmergentInclusive :: Int -> Vector Ngrams -> PhyloExport -> PhyloExport
mostEmergentInclusive nth foundations export =
    over ( export_groups
         .  traverse )
         (\g ->
            let lbl = ngramsToLabel foundations
                    $ take nth
                    $ map (\(_,(_,idx)) -> idx)
                    $ concat
                    $ map (\groups -> sortOn (fst . snd) groups)
                    $ groupBy ((==) `on` fst) $ reverse $ sortOn fst
                    $ zip ((g ^. phylo_groupMeta) ! "inclusion")
                    $ zip ((g ^. phylo_groupMeta) ! "dynamics") (g ^. phylo_groupNgrams)
            in g & phylo_groupLabel .~ lbl ) export


processLabels :: [PhyloLabel] -> Vector Ngrams -> Map Int Double -> PhyloExport -> PhyloExport
processLabels labels foundations freq export =
    foldl (\export' label ->
                case label of
                    GroupLabel  tagger nth ->
                        case tagger of
                            MostEmergentInclusive -> mostEmergentInclusive nth foundations export'
                            _ -> panic "[ERR][Viz.Phylo.PhyloExport] unknown tagger"
                    BranchLabel tagger nth ->
                        case tagger of
                            MostInclusive -> mostInclusive nth foundations export'
                            MostEmergentTfIdf -> mostEmergentTfIdf nth freq foundations export'
                            _ -> panic "[ERR][Viz.Phylo.PhyloExport] unknown tagger" ) export labels


------------------
-- | Dynamics | --
------------------

-- utiliser & creer une Map FdtId [PhyloGroup]
-- n = index of the current term
toDynamics :: FdtId -> [PhyloGroup] -> PhyloGroup -> Map FdtId (Date,Date) -> Double
toDynamics n elders g m =
    let prd = g ^. phylo_groupPeriod
        end = last' "dynamics" (sort $ map snd $ elems m)
    in  if (((snd prd) == (snd $ m ! n)) && (snd prd /= end))
            {- decrease -}
            then 2
        else if ((fst prd) == (fst $ m ! n))
            {- emerging -}
            then 0
        else if isNew
            {- emergence -}
            then 1
        else 3
    where
        --------------------------------------
        isNew :: Bool
        isNew = not $ elem n $ concat $ map _phylo_groupNgrams elders

type FdtId = Int 
processDynamics :: [PhyloGroup] -> [PhyloGroup]
processDynamics groups =
    map (\g ->
        let elders = filter (\g' -> (g ^. phylo_groupBranchId == g' ^. phylo_groupBranchId)
                                  && ((fst $ g ^. phylo_groupPeriod) > (fst $ g' ^. phylo_groupPeriod))) groups
        in  g & phylo_groupMeta %~ insert "dynamics" (map (\n -> toDynamics n elders g mapNgrams) $ g ^. phylo_groupNgrams) ) groups
    where
        --------------------------------------
        mapNgrams :: Map FdtId (Date,Date)
        mapNgrams = map (\dates ->
                        let dates' = sort dates
                        in (head' "dynamics" dates', last' "dynamics" dates'))
                  $ fromListWith (++)
                  $ foldl (\acc g -> acc ++ ( map (\n -> (n,[fst $ g ^. phylo_groupPeriod, snd $ g ^. phylo_groupPeriod]))
                                            $ (g ^. phylo_groupNgrams))) [] groups


-----------------
-- | horizon | --
-----------------

getGroupThr :: Double -> PhyloGroup -> Double
getGroupThr step g =
    let seaLvl = (g ^. phylo_groupMeta) ! "seaLevels"
        breaks = (g ^. phylo_groupMeta) ! "breaks"
     in (last' "export" (take (round $ (last' "export" breaks) + 1) seaLvl)) - step

toAncestor :: Double -> Map Int Double -> Proximity -> Double -> [PhyloGroup] -> PhyloGroup -> PhyloGroup
toAncestor nbDocs diago proximity step candidates ego =
  let curr = ego ^. phylo_groupAncestors
   in ego & phylo_groupAncestors .~ (curr ++ (map (\(g,w) -> (getGroupId g,w))
         $ filter (\(g,w) -> (w > 0) && (w >= (min (getGroupThr step ego) (getGroupThr step g))))
         $ map (\g -> (g, toProximity nbDocs diago proximity (ego ^. phylo_groupNgrams) (g ^. phylo_groupNgrams) (g ^. phylo_groupNgrams)))
         $ filter (\g -> g ^. phylo_groupBranchId /= ego ^. phylo_groupBranchId ) candidates))


headsToAncestors :: Double -> Map Int Double -> Proximity -> Double -> [PhyloGroup] -> [PhyloGroup] -> [PhyloGroup]
headsToAncestors nbDocs diago proximity step heads acc =
  if (null heads)
    then acc
    else
      let ego    = head' "headsToAncestors" heads
          heads' = tail' "headsToAncestors" heads
       in headsToAncestors nbDocs diago proximity step heads' (acc ++ [toAncestor nbDocs diago proximity step heads' ego])


toHorizon :: Phylo -> Phylo
toHorizon phylo =
  let phyloAncestor = updatePhyloGroups
                    scale
                    (fromList $ map (\g -> (getGroupId g, g))
                              $ concat
                              $ tracePhyloAncestors newGroups) phylo
      reBranched = fromList $ map (\g -> (getGroupId g, g)) $ concat
                 $ groupsToBranches' $ fromList $ map (\g -> (getGroupId g, g)) $ getGroupsFromScale scale phyloAncestor
   in updatePhyloGroups scale reBranched phylo
  where
    -- | 1) for each periods
    periods :: [Period]
    periods = getPeriodIds phylo
    -- --
    scale :: Scale
    scale = getLastLevel phylo
    -- --
    frame :: Int
    frame = getTimeFrame $ timeUnit $ getConfig phylo
    -- | 2) find ancestors between groups without parents
    mapGroups :: [[PhyloGroup]]
    mapGroups = map (\prd ->
      let groups  = getGroupsFromScalePeriods scale [prd] phylo
          childs  = getPreviousChildIds scale frame prd periods phylo
              -- maybe add a better filter for non isolated  ancestors
          heads   = filter (\g -> (not . null) $ (g ^. phylo_groupPeriodChilds))
                  $ filter (\g -> null (g ^. phylo_groupPeriodParents) && (notElem (getGroupId g) childs)) groups
          noHeads = groups \\ heads
          nbDocs  = sum $ elems  $ filterDocs  (phylo ^. phylo_timeDocs) [prd]
          diago   = reduceDiagos $ filterDiago (phylo ^. phylo_timeCooc) [prd]
          proximity = (phyloProximity $ getConfig phylo)
          step = case getSeaElevation phylo of
            Constante  _ s -> s
            Adaptative _ -> 0
       -- in headsToAncestors nbDocs diago proximity heads groups []
       in map (\ego -> toAncestor nbDocs diago proximity step noHeads ego)
        $ headsToAncestors nbDocs diago proximity step heads []
      ) periods
    -- | 3) process this task concurrently
    newGroups :: [[PhyloGroup]]
    newGroups = mapGroups `using` parList rdeepseq
    --------------------------------------

getPreviousChildIds :: Scale -> Int -> Period -> [Period] -> Phylo -> [PhyloGroupId]
getPreviousChildIds lvl frame curr prds phylo =
    concat $ map ((map fst) . _phylo_groupPeriodChilds)
           $ getGroupsFromScalePeriods lvl (getNextPeriods ToParents frame curr prds) phylo

---------------------
-- | phyloExport | --
---------------------

toPhyloExport :: Phylo -> DotGraph DotId
toPhyloExport phylo = exportToDot phylo
                    $ processFilters (exportFilter $ getConfig phylo) (phyloQuality $ getConfig phylo)
                    $ processSort    (exportSort   $ getConfig phylo) (getSeaElevation phylo)
                    $ processLabels  (exportLabel  $ getConfig phylo) (getRoots phylo) (_phylo_lastTermFreq phylo)
                    $ processMetrics phylo export
    where
        export :: PhyloExport
        export = PhyloExport groups branches
        --------------------------------------
        branches :: [PhyloBranch]
        branches = map (\g ->
                      let seaLvl = (g ^. phylo_groupMeta) ! "seaLevels"
                          breaks = (g ^. phylo_groupMeta) ! "breaks"
                          canonId = take (round $ (last' "export" breaks) + 2) (snd $ g ^. phylo_groupBranchId)
                       in PhyloBranch (g ^. phylo_groupBranchId)
                                      canonId
                                      seaLvl
                                      0
                                      (last' "export" (take (round $ (last' "export" breaks) + 1) seaLvl))
                                      0
                                      0
                                      "" empty)
                  $ map (\gs -> head' "export" gs)
                  $ groupBy (\g g' -> g ^. phylo_groupBranchId == g' ^. phylo_groupBranchId)
                  $ sortOn (\g -> g ^. phylo_groupBranchId) groups
        --------------------------------------
        groups :: [PhyloGroup]
        groups = traceExportGroups
               -- necessaire ?
               $ processDynamics
               $ getGroupsFromScale (phyloScale $ getConfig phylo)
               $ tracePhyloInfo phylo


traceExportBranches :: [PhyloBranch] -> [PhyloBranch]
traceExportBranches branches = trace ("\n"
  <> "-- | Export " <> show(length branches) <> " branches") branches

tracePhyloAncestors :: [[PhyloGroup]] -> [[PhyloGroup]]
tracePhyloAncestors groups = trace ( "-- | Found " <> show(length $ concat $ map _phylo_groupAncestors $ concat groups) <> " ancestors") groups

tracePhyloInfo :: Phylo -> Phylo
tracePhyloInfo phylo = trace ("\n"  <> "##########################" <> "\n\n" <> "-- | Phylo with λ = "
    <> show(_qua_granularity $ phyloQuality $ getConfig phylo) <> " applied to "
    <> show(length $ Vector.toList $ getRoots phylo) <> " foundations"
  ) phylo


traceExportGroups :: [PhyloGroup] -> [PhyloGroup]
traceExportGroups groups = trace ("\n" <> "-- | Export "
    <> show(length $ nub $ map (\g -> g ^. phylo_groupBranchId) groups) <> " branches, "
    <> show(length groups) <> " groups and "
    <> show(length $ nub $ concat $ map (\g -> g ^. phylo_groupNgrams) groups) <> " terms"
  ) groups

