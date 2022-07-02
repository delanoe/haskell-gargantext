{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -O0                 #-}
{-# LANGUAGE TypeOperators #-}

module Gargantext.API.Client where

import Data.Int
import Data.Maybe
import Data.Map (Map)
import Data.Morpheus.Types.IO (GQLRequest, GQLResponse)
import Data.Proxy
import Data.Text (Text)
import Data.Time.Clock
import Data.Vector (Vector)
import Gargantext.API
import Gargantext.API.Admin.Auth (ForgotPasswordAsyncParams)
import Gargantext.API.Admin.Auth.Types hiding (Token)
import Gargantext.API.Admin.Orchestrator.Types
import Gargantext.API.Count
import Gargantext.API.EKG
import qualified Gargantext.API.GraphQL                    as GraphQL
import Gargantext.API.HashedResponse
import Gargantext.API.Ngrams as Ngrams
import Gargantext.API.Ngrams.NgramsTree
import Gargantext.API.Ngrams.List.Types
import Gargantext.API.Node
import Gargantext.API.Node.Contact
import Gargantext.API.Node.Corpus.Export.Types
import Gargantext.API.Node.Corpus.New
import qualified Gargantext.API.Node.Document.Export.Types as DocumentExport
import Gargantext.API.Node.DocumentsFromWriteNodes
import Gargantext.API.Node.DocumentUpload
import Gargantext.API.Node.File
import Gargantext.API.Node.FrameCalcUpload
import Gargantext.API.Node.New
import Gargantext.API.Node.Share
import Gargantext.API.Node.Types
import Gargantext.API.Node.Update
import Gargantext.API.Public
import Gargantext.API.Routes
import Gargantext.API.Search
import Gargantext.API.Table
import Gargantext.Core.Types (NodeTableResult)
import Gargantext.Core.Types.Main hiding (Limit, Offset)
import Gargantext.Core.Viz.Graph hiding (Node, Version)
import Gargantext.Core.Viz.Graph.API
import Gargantext.Core.Viz.Phylo.API (PhyloData)
import Gargantext.Core.Viz.Types
import Gargantext.Database.Admin.Types.Metrics
import Gargantext.Database.Admin.Types.Hyperdata
import Gargantext.Database.Admin.Types.Node
import Gargantext.Database.Schema.Ngrams
import Gargantext.Database.Query.Facet as Facet
import Servant.API
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Client
import Servant.Job.Core
import Servant.Job.Types
import System.Metrics.Json (Sample, Value)


-- * version API
getBackendVersion :: ClientM Text

-- * auth API
postAuth :: AuthRequest -> ClientM AuthResponse
forgotPasswordPost :: ForgotPasswordRequest -> ClientM ForgotPasswordResponse
forgotPasswordGet :: Maybe Text -> ClientM ForgotPasswordGet
postForgotPasswordAsync :: ClientM (JobStatus 'Safe JobLog)
postForgotPasswordAsyncJob :: JobInput Maybe ForgotPasswordAsyncParams -> ClientM (JobStatus 'Safe JobLog)
killForgotPasswordAsyncJob :: JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollForgotPasswordAsyncJob :: JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitForgotPasswordAsyncJob :: JobID 'Unsafe -> ClientM (JobOutput JobLog)

-- * admin api
getRoots :: Token -> ClientM [Node HyperdataUser]
putRoots :: Token -> ClientM Int -- not actually implemented in the backend
deleteNodes :: Token -> [NodeId] -> ClientM Int

-- * node api
getNode    :: Token -> NodeId -> ClientM (Node HyperdataAny)
getContext :: Token -> ContextId -> ClientM (Node HyperdataAny)
renameNode :: Token -> NodeId -> RenameNode -> ClientM [Int]
postNode :: Token -> NodeId -> PostNode -> ClientM [NodeId]
postNodeAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postNodeAsyncJob :: Token -> NodeId -> JobInput Maybe PostNode -> ClientM (JobStatus 'Safe JobLog)
killNodeAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollNodeAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitNodeAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postNodeFrameCalcAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postNodeFrameCalcAsyncJob :: Token -> NodeId -> JobInput Maybe FrameCalcUpload -> ClientM (JobStatus 'Safe JobLog)
killNodeFrameCalcAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollNodeFrameCalcAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitNodeFrameCalcAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

putNode :: Token -> NodeId -> HyperdataAny -> ClientM Int

postUpdateNodeAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postUpdateNodeAsyncJob :: Token -> NodeId -> JobInput Maybe UpdateNodeParams -> ClientM (JobStatus 'Safe JobLog)
killUpdateNodeAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollUpdateNodeAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitUpdateNodeAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

deleteNode :: Token -> NodeId -> ClientM Int
getNodeChildren :: Token -> NodeId -> Maybe NodeType -> Maybe Int -> Maybe Int -> ClientM (NodeTableResult HyperdataAny)

getNodeTable :: Token -> NodeId -> Maybe TabType -> Maybe ListId -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> Maybe Text -> ClientM (HashedResponse FacetTableResult)
postNodeTableQuery :: Token -> NodeId -> TableQuery -> ClientM FacetTableResult
getNodeTableHash :: Token -> NodeId -> Maybe TabType -> ClientM Text

getNodeNgramsTable :: Token -> NodeId -> TabType -> ListId -> Int -> Maybe Int -> Maybe ListType -> Maybe MinSize -> Maybe MaxSize -> Maybe Ngrams.OrderBy -> Maybe Text -> ClientM (VersionedWithCount NgramsTable)
putNodeNgramsTablePatch :: Token -> NodeId -> TabType -> ListId -> Versioned NgramsTablePatch -> ClientM (Versioned NgramsTablePatch)
postNodeRecomputeNgramsTableScores :: Token -> NodeId -> TabType -> ListId -> ClientM Int
getNodeNgramsTableVersion :: Token -> NodeId -> TabType -> ListId -> ClientM Version
postNodeUpdateNgramsTableChartsAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postNodeUpdateNgramsTableChartsAsyncJob :: Token -> NodeId -> JobInput Maybe UpdateTableNgramsCharts -> ClientM (JobStatus 'Safe JobLog)
killNodeUpdateNgramsTableChartsAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollNodeUpdateNgramsTableChartsAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitNodeUpdateNgramsTableChartsAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

putNodeCategory :: Token -> NodeId -> NodesToCategory -> ClientM [Int]
putNodeScore :: Token -> NodeId -> NodesToScore -> ClientM [Int]

postNodeSearch :: Token -> NodeId -> SearchQuery -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> ClientM SearchResult

postNodeShare :: Token -> NodeId -> ShareNodeParams -> ClientM Int

postNodePairCorpusAnnuaire :: Token -> NodeId -> AnnuaireId -> Maybe ListId -> ClientM [Int]
getNodePairs :: Token -> NodeId -> ClientM [AnnuaireId]
getNodePairings :: Token -> NodeId -> Maybe TabType -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> ClientM [FacetDoc]

getNodeScatterMetrics :: Token -> NodeId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM (HashedResponse Metrics)
postNodeScatterMetricsUpdate :: Token -> NodeId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getNodeScatterMetricsHash :: Token -> NodeId -> Maybe NodeId -> TabType -> ClientM Text
getNodeChart :: Token  -> NodeId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ClientM (HashedResponse (ChartMetrics Histo))
postNodeChartUpdate :: Token -> NodeId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getNodeChartHash :: Token -> NodeId -> Maybe NodeId -> TabType -> ClientM Text
getNodePie :: Token -> NodeId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ClientM (HashedResponse (ChartMetrics Histo))
postNodePieUpdate :: Token -> NodeId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getNodePieHash :: Token -> NodeId -> Maybe NodeId -> TabType -> ClientM Text
getNodeTree :: Token -> NodeId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ListType -> ClientM (HashedResponse (ChartMetrics (Vector NgramsTree)))
postNodeTreeUpdate :: Token -> NodeId -> Maybe NodeId -> TabType -> ListType -> ClientM ()
getNodeTreeHash :: Token -> NodeId -> Maybe NodeId -> TabType -> ListType -> ClientM Text
getNodePhylo :: Token -> NodeId -> Maybe NodeId -> Maybe Int -> Maybe Int -> ClientM PhyloData
putNodePhylo :: Token -> NodeId -> Maybe NodeId -> ClientM NodeId

putNodeMove :: Token -> NodeId -> ParentId -> ClientM [Int]
postNodeUnpublish :: Token -> NodeId -> NodeId -> ClientM Int

getNodeFile :: Token -> NodeId -> ClientM (Headers '[Header "Content-Type" Text] BSResponse)
postNodeFileAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postNodeFileAsyncJob :: Token -> NodeId -> JobInput Maybe NewWithFile -> ClientM (JobStatus 'Safe JobLog)
killNodeFileAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollNodeFileAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitNodeFileAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postNodeDocumentWriteNodesAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postNodeDocumentWriteNodesAsyncJob :: Token -> NodeId -> JobInput Maybe Params -> ClientM (JobStatus 'Safe JobLog)
killNodeDocumentWriteNodesAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollNodeDocumentWriteNodesAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitNodeDocumentWriteNodesAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postNodeDocumentUploadAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postNodeDocumentUploadAsyncJob :: Token -> NodeId -> JobInput Maybe DocumentUpload -> ClientM (JobStatus 'Safe JobLog)
killNodeDocumentUploadAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollNodeDocumentUploadAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitNodeDocumentUploadAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

-- * corpus api
getCorpus :: Token -> CorpusId -> ClientM (Node HyperdataCorpus)
renameCorpus :: Token -> CorpusId -> RenameNode -> ClientM [Int]
postCorpus :: Token -> CorpusId -> PostNode -> ClientM [CorpusId]
postCorpusAsync :: Token -> CorpusId -> ClientM (JobStatus 'Safe JobLog)
postCorpusAsyncJob :: Token -> CorpusId -> JobInput Maybe PostNode -> ClientM (JobStatus 'Safe JobLog)
killCorpusAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollCorpusAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitCorpusAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postCorpusFrameCalcAsync :: Token -> CorpusId -> ClientM (JobStatus 'Safe JobLog)
postCorpusFrameCalcAsyncJob :: Token -> CorpusId -> JobInput Maybe FrameCalcUpload -> ClientM (JobStatus 'Safe JobLog)
killCorpusFrameCalcAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollCorpusFrameCalcAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitCorpusFrameCalcAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

putCorpus :: Token -> CorpusId -> HyperdataCorpus -> ClientM Int

postUpdateCorpusAsync :: Token -> CorpusId -> ClientM (JobStatus 'Safe JobLog)
postUpdateCorpusAsyncJob :: Token -> CorpusId -> JobInput Maybe UpdateNodeParams -> ClientM (JobStatus 'Safe JobLog)
killUpdateCorpusAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollUpdateCorpusAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitUpdateCorpusAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

deleteCorpus :: Token -> CorpusId -> ClientM Int
getCorpusChildren :: Token -> CorpusId -> Maybe NodeType -> Maybe Int -> Maybe Int -> ClientM (NodeTableResult HyperdataCorpus)

getCorpusTable :: Token -> CorpusId -> Maybe TabType -> Maybe ListId -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> Maybe Text -> ClientM (HashedResponse FacetTableResult)
postCorpusTableQuery :: Token -> CorpusId -> TableQuery -> ClientM FacetTableResult
getCorpusTableHash :: Token -> CorpusId -> Maybe TabType -> ClientM Text

getCorpusNgramsTable :: Token -> CorpusId -> TabType -> ListId -> Int -> Maybe Int -> Maybe ListType -> Maybe MinSize -> Maybe MaxSize -> Maybe Ngrams.OrderBy -> Maybe Text -> ClientM (VersionedWithCount NgramsTable)
putCorpusNgramsTablePatch :: Token -> CorpusId -> TabType -> ListId -> Versioned NgramsTablePatch -> ClientM (Versioned NgramsTablePatch)
postCorpusRecomputeNgramsTableScores :: Token -> CorpusId -> TabType -> ListId -> ClientM Int
getCorpusNgramsTableVersion :: Token -> CorpusId -> TabType -> ListId -> ClientM Version
postCorpusUpdateNgramsTableChartsAsync :: Token -> CorpusId -> ClientM (JobStatus 'Safe JobLog)
postCorpusUpdateNgramsTableChartsAsyncJob :: Token -> CorpusId -> JobInput Maybe UpdateTableNgramsCharts -> ClientM (JobStatus 'Safe JobLog)
killCorpusUpdateNgramsTableChartsAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollCorpusUpdateNgramsTableChartsAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitCorpusUpdateNgramsTableChartsAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

putCorpusCategory :: Token -> CorpusId -> NodesToCategory -> ClientM [Int]
putCorpusScore :: Token -> CorpusId -> NodesToScore -> ClientM [Int]

postCorpusSearch :: Token -> CorpusId -> SearchQuery -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> ClientM SearchResult

postCorpusShare :: Token -> CorpusId -> ShareNodeParams -> ClientM Int

postCorpusPairCorpusAnnuaire :: Token -> CorpusId -> AnnuaireId -> Maybe ListId -> ClientM [Int]
getCorpusPairs :: Token -> CorpusId -> ClientM [AnnuaireId]
getCorpusPairings :: Token -> CorpusId -> Maybe TabType -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> ClientM [FacetDoc]

getCorpusScatterMetrics :: Token -> CorpusId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM (HashedResponse Metrics)
postCorpusScatterMetricsUpdate :: Token -> CorpusId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getCorpusScatterMetricsHash :: Token -> CorpusId -> Maybe NodeId -> TabType -> ClientM Text
getCorpusChart :: Token  -> CorpusId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ClientM (HashedResponse (ChartMetrics Histo))
postCorpusChartUpdate :: Token -> CorpusId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getCorpusChartHash :: Token -> CorpusId -> Maybe NodeId -> TabType -> ClientM Text
getCorpusPie :: Token -> CorpusId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ClientM (HashedResponse (ChartMetrics Histo))
postCorpusPieUpdate :: Token -> CorpusId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getCorpusPieHash :: Token -> CorpusId -> Maybe NodeId -> TabType -> ClientM Text
getCorpusTree :: Token -> CorpusId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ListType -> ClientM (HashedResponse (ChartMetrics (Vector NgramsTree)))
postCorpusTreeUpdate :: Token -> CorpusId -> Maybe NodeId -> TabType -> ListType -> ClientM ()
getCorpusTreeHash :: Token -> CorpusId -> Maybe NodeId -> TabType -> ListType -> ClientM Text
getCorpusPhylo :: Token -> CorpusId -> Maybe NodeId -> Maybe Int -> Maybe Int -> ClientM PhyloData
putCorpusPhylo :: Token -> CorpusId -> Maybe NodeId -> ClientM NodeId

putCorpusMove :: Token -> CorpusId -> ParentId -> ClientM [Int]
postCorpusUnpublish :: Token -> CorpusId -> CorpusId -> ClientM Int

getCorpusFile :: Token -> NodeId -> ClientM (Headers '[Header "Content-Type" Text] BSResponse)
postCorpusFileAsync :: Token -> CorpusId -> ClientM (JobStatus 'Safe JobLog)
postCorpusFileAsyncJob :: Token -> CorpusId -> JobInput Maybe NewWithFile -> ClientM (JobStatus 'Safe JobLog)
killCorpusFileAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollCorpusFileAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitCorpusFileAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postCorpusDocumentWriteNodesAsync :: Token -> CorpusId -> ClientM (JobStatus 'Safe JobLog)
postCorpusDocumentWriteNodesAsyncJob :: Token -> CorpusId -> JobInput Maybe Params -> ClientM (JobStatus 'Safe JobLog)
killCorpusDocumentWriteNodesAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollCorpusDocumentWriteNodesAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitCorpusDocumentWriteNodesAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postCorpusDocumentUploadAsync :: Token -> CorpusId -> ClientM (JobStatus 'Safe JobLog)
postCorpusDocumentUploadAsyncJob :: Token -> CorpusId -> JobInput Maybe DocumentUpload -> ClientM (JobStatus 'Safe JobLog)
killCorpusDocumentUploadAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollCorpusDocumentUploadAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitCorpusDocumentUploadAsyncJob :: Token -> CorpusId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

-- * corpus node/node API
getCorpusNodeNode :: Token -> NodeId -> NodeId -> ClientM (Node HyperdataAny)

-- * corpus export API
getCorpusExport :: Token -> CorpusId -> Maybe ListId -> Maybe NgramsType -> ClientM Corpus

-- * annuaire api
getAnnuaire :: Token -> AnnuaireId -> ClientM (Node HyperdataAnnuaire)
renameAnnuaire :: Token -> AnnuaireId -> RenameNode -> ClientM [Int]
postAnnuaire :: Token -> AnnuaireId -> PostNode -> ClientM [AnnuaireId]
postAnnuaireAsync :: Token -> AnnuaireId -> ClientM (JobStatus 'Safe JobLog)
postAnnuaireAsyncJob :: Token -> AnnuaireId -> JobInput Maybe PostNode -> ClientM (JobStatus 'Safe JobLog)
killAnnuaireAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollAnnuaireAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitAnnuaireAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postAnnuaireFrameCalcAsync :: Token -> AnnuaireId -> ClientM (JobStatus 'Safe JobLog)
postAnnuaireFrameCalcAsyncJob :: Token -> AnnuaireId -> JobInput Maybe FrameCalcUpload -> ClientM (JobStatus 'Safe JobLog)
killAnnuaireFrameCalcAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollAnnuaireFrameCalcAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitAnnuaireFrameCalcAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

putAnnuaire :: Token -> AnnuaireId -> HyperdataAnnuaire -> ClientM Int

postUpdateAnnuaireAsync :: Token -> AnnuaireId -> ClientM (JobStatus 'Safe JobLog)
postUpdateAnnuaireAsyncJob :: Token -> AnnuaireId -> JobInput Maybe UpdateNodeParams -> ClientM (JobStatus 'Safe JobLog)
killUpdateAnnuaireAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollUpdateAnnuaireAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitUpdateAnnuaireAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

deleteAnnuaire :: Token -> AnnuaireId -> ClientM Int
getAnnuaireChildren :: Token -> AnnuaireId -> Maybe NodeType -> Maybe Int -> Maybe Int -> ClientM (NodeTableResult HyperdataAnnuaire)

getAnnuaireTable :: Token -> AnnuaireId -> Maybe TabType -> Maybe ListId -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> Maybe Text -> ClientM (HashedResponse FacetTableResult)
postAnnuaireTableQuery :: Token -> AnnuaireId -> TableQuery -> ClientM FacetTableResult
getAnnuaireTableHash :: Token -> AnnuaireId -> Maybe TabType -> ClientM Text

getAnnuaireNgramsTable :: Token -> AnnuaireId -> TabType -> ListId -> Int -> Maybe Int -> Maybe ListType -> Maybe MinSize -> Maybe MaxSize -> Maybe Ngrams.OrderBy -> Maybe Text -> ClientM (VersionedWithCount NgramsTable)
putAnnuaireNgramsTablePatch :: Token -> AnnuaireId -> TabType -> ListId -> Versioned NgramsTablePatch -> ClientM (Versioned NgramsTablePatch)
postAnnuaireRecomputeNgramsTableScores :: Token -> AnnuaireId -> TabType -> ListId -> ClientM Int
getAnnuaireNgramsTableVersion :: Token -> AnnuaireId -> TabType -> ListId -> ClientM Version
postAnnuaireUpdateNgramsTableChartsAsync :: Token -> AnnuaireId -> ClientM (JobStatus 'Safe JobLog)
postAnnuaireUpdateNgramsTableChartsAsyncJob :: Token -> AnnuaireId -> JobInput Maybe UpdateTableNgramsCharts -> ClientM (JobStatus 'Safe JobLog)
killAnnuaireUpdateNgramsTableChartsAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollAnnuaireUpdateNgramsTableChartsAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitAnnuaireUpdateNgramsTableChartsAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

putAnnuaireCategory :: Token -> AnnuaireId -> NodesToCategory -> ClientM [Int]
putAnnuaireScore :: Token -> AnnuaireId -> NodesToScore -> ClientM [Int]

postAnnuaireSearch :: Token -> AnnuaireId -> SearchQuery -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> ClientM SearchResult
postAnnuaireShare :: Token -> AnnuaireId -> ShareNodeParams -> ClientM Int

postAnnuairePairCorpusAnnuaire :: Token -> AnnuaireId -> AnnuaireId -> Maybe ListId -> ClientM [Int]
getAnnuairePairs :: Token -> AnnuaireId -> ClientM [AnnuaireId]
getAnnuairePairings :: Token -> AnnuaireId -> Maybe TabType -> Maybe Int -> Maybe Int -> Maybe Facet.OrderBy -> ClientM [FacetDoc]

getAnnuaireScatterMetrics :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM (HashedResponse Metrics)
postAnnuaireScatterMetricsUpdate :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getAnnuaireScatterMetricsHash :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> ClientM Text
getAnnuaireChart :: Token -> AnnuaireId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ClientM (HashedResponse (ChartMetrics Histo))
postAnnuaireChartUpdate :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getAnnuaireChartHash :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> ClientM Text
getAnnuairePie :: Token -> AnnuaireId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ClientM (HashedResponse (ChartMetrics Histo))
postAnnuairePieUpdate :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> Maybe Int -> ClientM ()
getAnnuairePieHash :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> ClientM Text
getAnnuaireTree :: Token -> AnnuaireId -> Maybe UTCTime -> Maybe UTCTime -> Maybe NodeId -> TabType -> ListType -> ClientM (HashedResponse (ChartMetrics (Vector NgramsTree)))
postAnnuaireTreeUpdate :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> ListType -> ClientM ()
getAnnuaireTreeHash :: Token -> AnnuaireId -> Maybe NodeId -> TabType -> ListType -> ClientM Text
getAnnuairePhylo :: Token -> AnnuaireId -> Maybe NodeId -> Maybe Int -> Maybe Int -> ClientM PhyloData
putAnnuairePhylo :: Token -> AnnuaireId -> Maybe NodeId -> ClientM NodeId

putAnnuaireMove :: Token -> AnnuaireId -> ParentId -> ClientM [Int]
postAnnuaireUnpublish :: Token -> AnnuaireId -> AnnuaireId -> ClientM Int

getAnnuaireFile :: Token -> AnnuaireId -> ClientM (Headers '[Header "Content-Type" Text] BSResponse)
postAnnuaireFileAsync :: Token -> AnnuaireId -> ClientM (JobStatus 'Safe JobLog)
postAnnuaireFileAsyncJob :: Token -> AnnuaireId -> JobInput Maybe NewWithFile -> ClientM (JobStatus 'Safe JobLog)
killAnnuaireFileAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollAnnuaireFileAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitAnnuaireFileAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postAnnuaireDocumentWriteNodesAsync :: Token -> AnnuaireId -> ClientM (JobStatus 'Safe JobLog)
postAnnuaireDocumentWriteNodesAsyncJob :: Token -> AnnuaireId -> JobInput Maybe Params -> ClientM (JobStatus 'Safe JobLog)
killAnnuaireDocumentWriteNodesAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollAnnuaireDocumentWriteNodesAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitAnnuaireDocumentWriteNodesAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postAnnuaireDocumentUploadAsync :: Token -> AnnuaireId -> ClientM (JobStatus 'Safe JobLog)
postAnnuaireDocumentUploadAsyncJob :: Token -> AnnuaireId -> JobInput Maybe DocumentUpload -> ClientM (JobStatus 'Safe JobLog)
killAnnuaireDocumentUploadAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollAnnuaireDocumentUploadAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitAnnuaireDocumentUploadAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

-- * contact api
postAnnuaireContactAsync :: Token -> AnnuaireId -> ClientM (JobStatus 'Safe JobLog)
postAnnuaireContactAsyncJob :: Token -> AnnuaireId -> JobInput Maybe AddContactParams -> ClientM (JobStatus 'Safe JobLog)
killAnnuaireContactAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollAnnuaireContactAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitAnnuaireContactAsyncJob :: Token -> AnnuaireId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

-- * contact node/node API
getAnnuaireContactNodeNode :: Token -> NodeId -> NodeId -> ClientM (Node HyperdataContact)

-- * document ngrams api
getDocumentNgramsTable :: Token -> DocId -> TabType -> ListId -> Int -> Maybe Int -> Maybe ListType -> Maybe MinSize -> Maybe MaxSize -> Maybe Ngrams.OrderBy -> Maybe Text -> ClientM (VersionedWithCount NgramsTable)
putDocumentNgramsTable :: Token -> DocId -> TabType -> ListId -> Versioned NgramsTablePatch -> ClientM (Versioned NgramsTablePatch)
postRecomputeDocumentNgramsTableScore :: Token -> DocId -> TabType -> ListId -> ClientM Int
getDocumentNgramsTableVersion :: Token -> DocId -> TabType -> ListId -> ClientM Version
postDocumentNgramsTableAsync :: Token -> DocId -> ClientM (JobStatus 'Safe JobLog)
postDocumentNgramsTableAsyncJob :: Token -> DocId -> JobInput Maybe UpdateTableNgramsCharts -> ClientM (JobStatus 'Safe JobLog)
killDocumentNgramsTableAsyncJob :: Token -> DocId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollDocumentNgramsTableAsyncJob :: Token -> DocId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitDocumentNgramsTableAsyncJob :: Token -> DocId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

-- * document export API
getDocumentExportJSON :: Token -> DocId -> ClientM DocumentExport.DocumentExport
getDocumentExportCSV :: Token -> DocId -> ClientM Text

-- * count api
postCountQuery :: Token -> Query -> ClientM Counts

-- * graph api
getGraphHyperdata :: Token -> NodeId -> ClientM HyperdataGraphAPI
postGraphAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postGraphAsyncJob :: Token -> NodeId -> JobInput Maybe () -> ClientM (JobStatus 'Safe JobLog)
killGraphAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollGraphAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitGraphAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postGraphClone :: Token -> NodeId -> HyperdataGraphAPI -> ClientM NodeId
getGraphGexf :: Token -> NodeId -> ClientM (Headers '[Header "Content-Disposition" Text] Graph)
getGraphVersions :: Token -> NodeId -> ClientM GraphVersions
postGraphRecomputeVersion :: Token -> NodeId -> ClientM Graph
getTree :: Token -> NodeId -> [NodeType] -> ClientM (Tree NodeTree)
getTreeFirstLevel :: Token -> NodeId -> [NodeType] -> ClientM (Tree NodeTree)

-- * new corpus API
postNewCorpusWithFormAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postNewCorpusWithFormAsyncJob :: Token -> NodeId -> JobInput Maybe NewWithForm -> ClientM (JobStatus 'Safe JobLog)
killNewCorpusWithFormAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollNewCorpusWithFormAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitNewCorpusWithFormAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postNewCorpusWithQueryAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postNewCorpusWithQueryAsyncJob :: Token -> NodeId -> JobInput Maybe WithQuery -> ClientM (JobStatus 'Safe JobLog)
killNewCorpusWithQueryAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollNewCorpusWithQueryAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitNewCorpusWithQueryAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

-- * list API
getList :: Token -> NodeId -> ClientM (Headers '[Header "Content-Disposition" Text] (Map NgramsType (Versioned NgramsTableMap)))
postListJsonUpdateAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postListJsonUpdateAsyncJob :: Token -> NodeId -> JobInput Maybe WithFile -> ClientM (JobStatus 'Safe JobLog)
killListJsonUpdateAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit  -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollListJsonUpdateAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit  -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitListJsonUpdateAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

postListCsvUpdateAsync :: Token -> NodeId -> ClientM (JobStatus 'Safe JobLog)
postListCsvUpdateAsyncJob :: Token -> NodeId -> JobInput Maybe WithTextFile -> ClientM (JobStatus 'Safe JobLog)
killListCsvUpdateAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit  -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
pollListCsvUpdateAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> Maybe Limit  -> Maybe Offset -> ClientM (JobStatus 'Safe JobLog)
waitListCsvUpdateAsyncJob :: Token -> NodeId -> JobID 'Unsafe -> ClientM (JobOutput JobLog)

-- * public API
getPublicData :: ClientM [PublicData]
getPublicNodeFile :: NodeId -> ClientM (Headers '[Header "Content-Type" Text] BSResponse)

-- * ekg api
-- | get a sample of all metrics
getMetricsSample :: ClientM Sample
-- | open @<backend:port\/ekg\/index.html@ to see a list of metrics
getMetricSample :: [Text] -> ClientM Value

-- * graphql api

postGraphQL :: Token -> GQLRequest -> ClientM GQLResponse
postGraphQL = client (fstEndpoint (flatten GraphQL.gqapi))
  where fstEndpoint :: Proxy (a :<|> b) -> Proxy a
        fstEndpoint _ = Proxy

-- * unpacking of client functions to derive all the individual clients

clientApi :: Client ClientM (Flat GargAPI)
clientApi = client (flatten apiGarg)

getMetricsSample :<|> getMetricSample :<|> _ = client (Proxy :: Proxy (Flat EkgAPI))

postAuth
  :<|> forgotPasswordPost
  :<|> forgotPasswordGet
  :<|> postForgotPasswordAsync
  :<|> postForgotPasswordAsyncJob
  :<|> killForgotPasswordAsyncJob
  :<|> pollForgotPasswordAsyncJob
  :<|> waitForgotPasswordAsyncJob
  :<|> getBackendVersion
  :<|> getRoots
  :<|> putRoots
  :<|> deleteNodes
  :<|> getNode
  :<|> renameNode
  :<|> postNode
  :<|> postNodeAsync
  :<|> postNodeAsyncJob
  :<|> killNodeAsyncJob
  :<|> pollNodeAsyncJob
  :<|> waitNodeAsyncJob
  :<|> postNodeFrameCalcAsync
  :<|> postNodeFrameCalcAsyncJob
  :<|> killNodeFrameCalcAsyncJob
  :<|> pollNodeFrameCalcAsyncJob
  :<|> waitNodeFrameCalcAsyncJob
  :<|> putNode
  :<|> postUpdateNodeAsync
  :<|> postUpdateNodeAsyncJob
  :<|> killUpdateNodeAsyncJob
  :<|> pollUpdateNodeAsyncJob
  :<|> waitUpdateNodeAsyncJob
  :<|> deleteNode
  :<|> getNodeChildren
  :<|> getNodeTable
  :<|> postNodeTableQuery
  :<|> getNodeTableHash
  :<|> getNodeNgramsTable
  :<|> putNodeNgramsTablePatch
  :<|> postNodeRecomputeNgramsTableScores
  :<|> getNodeNgramsTableVersion
  :<|> postNodeUpdateNgramsTableChartsAsync
  :<|> postNodeUpdateNgramsTableChartsAsyncJob
  :<|> killNodeUpdateNgramsTableChartsAsyncJob
  :<|> pollNodeUpdateNgramsTableChartsAsyncJob
  :<|> waitNodeUpdateNgramsTableChartsAsyncJob
  :<|> putNodeCategory
  :<|> putNodeScore
  :<|> postNodeSearch
  :<|> postNodeShare
  :<|> postNodePairCorpusAnnuaire
  :<|> getNodePairs
  :<|> getNodePairings
  :<|> getNodeScatterMetrics
  :<|> postNodeScatterMetricsUpdate
  :<|> getNodeScatterMetricsHash
  :<|> getNodeChart
  :<|> postNodeChartUpdate
  :<|> getNodeChartHash
  :<|> getNodePie
  :<|> postNodePieUpdate
  :<|> getNodePieHash
  :<|> getNodeTree
  :<|> postNodeTreeUpdate
  :<|> getNodeTreeHash
  :<|> getNodePhylo
  :<|> putNodePhylo
  :<|> putNodeMove
  :<|> postNodeUnpublish
  :<|> getNodeFile
  :<|> postNodeFileAsync
  :<|> postNodeFileAsyncJob
  :<|> killNodeFileAsyncJob
  :<|> pollNodeFileAsyncJob
  :<|> waitNodeFileAsyncJob
  :<|> postNodeDocumentWriteNodesAsync
  :<|> postNodeDocumentWriteNodesAsyncJob
  :<|> killNodeDocumentWriteNodesAsyncJob
  :<|> pollNodeDocumentWriteNodesAsyncJob
  :<|> waitNodeDocumentWriteNodesAsyncJob
  :<|> postNodeDocumentUploadAsync
  :<|> postNodeDocumentUploadAsyncJob
  :<|> killNodeDocumentUploadAsyncJob
  :<|> pollNodeDocumentUploadAsyncJob
  :<|> waitNodeDocumentUploadAsyncJob
  :<|> getContext
  :<|> getCorpus
  :<|> renameCorpus
  :<|> postCorpus
  :<|> postCorpusAsync
  :<|> postCorpusAsyncJob
  :<|> killCorpusAsyncJob
  :<|> pollCorpusAsyncJob
  :<|> waitCorpusAsyncJob
  :<|> postCorpusFrameCalcAsync
  :<|> postCorpusFrameCalcAsyncJob
  :<|> killCorpusFrameCalcAsyncJob
  :<|> pollCorpusFrameCalcAsyncJob
  :<|> waitCorpusFrameCalcAsyncJob
  :<|> putCorpus
  :<|> postUpdateCorpusAsync
  :<|> postUpdateCorpusAsyncJob
  :<|> killUpdateCorpusAsyncJob
  :<|> pollUpdateCorpusAsyncJob
  :<|> waitUpdateCorpusAsyncJob
  :<|> deleteCorpus
  :<|> getCorpusChildren
  :<|> getCorpusTable
  :<|> postCorpusTableQuery
  :<|> getCorpusTableHash
  :<|> getCorpusNgramsTable
  :<|> putCorpusNgramsTablePatch
  :<|> postCorpusRecomputeNgramsTableScores
  :<|> getCorpusNgramsTableVersion
  :<|> postCorpusUpdateNgramsTableChartsAsync
  :<|> postCorpusUpdateNgramsTableChartsAsyncJob
  :<|> killCorpusUpdateNgramsTableChartsAsyncJob
  :<|> pollCorpusUpdateNgramsTableChartsAsyncJob
  :<|> waitCorpusUpdateNgramsTableChartsAsyncJob
  :<|> putCorpusCategory
  :<|> putCorpusScore
  :<|> postCorpusSearch
  :<|> postCorpusShare
  :<|> postCorpusPairCorpusAnnuaire
  :<|> getCorpusPairs
  :<|> getCorpusPairings
  :<|> getCorpusScatterMetrics
  :<|> postCorpusScatterMetricsUpdate
  :<|> getCorpusScatterMetricsHash
  :<|> getCorpusChart
  :<|> postCorpusChartUpdate
  :<|> getCorpusChartHash
  :<|> getCorpusPie
  :<|> postCorpusPieUpdate
  :<|> getCorpusPieHash
  :<|> getCorpusTree
  :<|> postCorpusTreeUpdate
  :<|> getCorpusTreeHash
  :<|> getCorpusPhylo
  :<|> putCorpusPhylo
  :<|> putCorpusMove
  :<|> postCorpusUnpublish
  :<|> getCorpusFile
  :<|> postCorpusFileAsync
  :<|> postCorpusFileAsyncJob
  :<|> killCorpusFileAsyncJob
  :<|> pollCorpusFileAsyncJob
  :<|> waitCorpusFileAsyncJob
  :<|> postCorpusDocumentWriteNodesAsync
  :<|> postCorpusDocumentWriteNodesAsyncJob
  :<|> killCorpusDocumentWriteNodesAsyncJob
  :<|> pollCorpusDocumentWriteNodesAsyncJob
  :<|> waitCorpusDocumentWriteNodesAsyncJob
  :<|> postCorpusDocumentUploadAsync
  :<|> postCorpusDocumentUploadAsyncJob
  :<|> killCorpusDocumentUploadAsyncJob
  :<|> pollCorpusDocumentUploadAsyncJob
  :<|> waitCorpusDocumentUploadAsyncJob
  :<|> getCorpusNodeNode
  :<|> getCorpusExport
  :<|> getAnnuaire
  :<|> renameAnnuaire
  :<|> postAnnuaire
  :<|> postAnnuaireAsync
  :<|> postAnnuaireAsyncJob
  :<|> killAnnuaireAsyncJob
  :<|> pollAnnuaireAsyncJob
  :<|> waitAnnuaireAsyncJob
  :<|> postAnnuaireFrameCalcAsync
  :<|> postAnnuaireFrameCalcAsyncJob
  :<|> killAnnuaireFrameCalcAsyncJob
  :<|> pollAnnuaireFrameCalcAsyncJob
  :<|> waitAnnuaireFrameCalcAsyncJob
  :<|> putAnnuaire
  :<|> postUpdateAnnuaireAsync
  :<|> postUpdateAnnuaireAsyncJob
  :<|> killUpdateAnnuaireAsyncJob
  :<|> pollUpdateAnnuaireAsyncJob
  :<|> waitUpdateAnnuaireAsyncJob
  :<|> deleteAnnuaire
  :<|> getAnnuaireChildren
  :<|> getAnnuaireTable
  :<|> postAnnuaireTableQuery
  :<|> getAnnuaireTableHash
  :<|> getAnnuaireNgramsTable
  :<|> putAnnuaireNgramsTablePatch
  :<|> postAnnuaireRecomputeNgramsTableScores
  :<|> getAnnuaireNgramsTableVersion
  :<|> postAnnuaireUpdateNgramsTableChartsAsync
  :<|> postAnnuaireUpdateNgramsTableChartsAsyncJob
  :<|> killAnnuaireUpdateNgramsTableChartsAsyncJob
  :<|> pollAnnuaireUpdateNgramsTableChartsAsyncJob
  :<|> waitAnnuaireUpdateNgramsTableChartsAsyncJob
  :<|> putAnnuaireCategory
  :<|> putAnnuaireScore
  :<|> postAnnuaireSearch
  :<|> postAnnuaireShare
  :<|> postAnnuairePairCorpusAnnuaire
  :<|> getAnnuairePairs
  :<|> getAnnuairePairings
  :<|> getAnnuaireScatterMetrics
  :<|> postAnnuaireScatterMetricsUpdate
  :<|> getAnnuaireScatterMetricsHash
  :<|> getAnnuaireChart
  :<|> postAnnuaireChartUpdate
  :<|> getAnnuaireChartHash
  :<|> getAnnuairePie
  :<|> postAnnuairePieUpdate
  :<|> getAnnuairePieHash
  :<|> getAnnuaireTree
  :<|> postAnnuaireTreeUpdate
  :<|> getAnnuaireTreeHash
  :<|> getAnnuairePhylo
  :<|> putAnnuairePhylo
  :<|> putAnnuaireMove
  :<|> postAnnuaireUnpublish
  :<|> getAnnuaireFile
  :<|> postAnnuaireFileAsync
  :<|> postAnnuaireFileAsyncJob
  :<|> killAnnuaireFileAsyncJob
  :<|> pollAnnuaireFileAsyncJob
  :<|> waitAnnuaireFileAsyncJob
  :<|> postAnnuaireDocumentWriteNodesAsync
  :<|> postAnnuaireDocumentWriteNodesAsyncJob
  :<|> killAnnuaireDocumentWriteNodesAsyncJob
  :<|> pollAnnuaireDocumentWriteNodesAsyncJob
  :<|> waitAnnuaireDocumentWriteNodesAsyncJob
  :<|> postAnnuaireDocumentUploadAsync
  :<|> postAnnuaireDocumentUploadAsyncJob
  :<|> killAnnuaireDocumentUploadAsyncJob
  :<|> pollAnnuaireDocumentUploadAsyncJob
  :<|> waitAnnuaireDocumentUploadAsyncJob
  :<|> postAnnuaireContactAsync
  :<|> postAnnuaireContactAsyncJob
  :<|> killAnnuaireContactAsyncJob
  :<|> pollAnnuaireContactAsyncJob
  :<|> waitAnnuaireContactAsyncJob
  :<|> getAnnuaireContactNodeNode
  :<|> getDocumentNgramsTable
  :<|> putDocumentNgramsTable
  :<|> postRecomputeDocumentNgramsTableScore
  :<|> getDocumentNgramsTableVersion
  :<|> postDocumentNgramsTableAsync
  :<|> postDocumentNgramsTableAsyncJob
  :<|> killDocumentNgramsTableAsyncJob
  :<|> pollDocumentNgramsTableAsyncJob
  :<|> waitDocumentNgramsTableAsyncJob
  :<|> getDocumentExportJSON
  :<|> getDocumentExportCSV
  :<|> postCountQuery
  :<|> getGraphHyperdata
  :<|> postGraphAsync
  :<|> postGraphAsyncJob
  :<|> killGraphAsyncJob
  :<|> pollGraphAsyncJob
  :<|> waitGraphAsyncJob
  :<|> postGraphClone
  :<|> getGraphGexf
  :<|> getGraphVersions
  :<|> postGraphRecomputeVersion
  :<|> getTree
  :<|> getTreeFirstLevel
  :<|> postNewCorpusWithFormAsync
  :<|> postNewCorpusWithFormAsyncJob
  :<|> killNewCorpusWithFormAsyncJob
  :<|> pollNewCorpusWithFormAsyncJob
  :<|> waitNewCorpusWithFormAsyncJob
  :<|> postNewCorpusWithQueryAsync
  :<|> postNewCorpusWithQueryAsyncJob
  :<|> killNewCorpusWithQueryAsyncJob
  :<|> pollNewCorpusWithQueryAsyncJob
  :<|> waitNewCorpusWithQueryAsyncJob
  :<|> getList
  :<|> postListJsonUpdateAsync
  :<|> postListJsonUpdateAsyncJob
  :<|> killListJsonUpdateAsyncJob
  :<|> pollListJsonUpdateAsyncJob
  :<|> waitListJsonUpdateAsyncJob
  :<|> postListCsvUpdateAsync
  :<|> postListCsvUpdateAsyncJob
  :<|> killListCsvUpdateAsyncJob
  :<|> pollListCsvUpdateAsyncJob
  :<|> waitListCsvUpdateAsyncJob
  :<|> getPublicData
  :<|> getPublicNodeFile
  = clientApi
