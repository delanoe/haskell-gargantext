{-|
Module      : Gargantext.API.Node.Document.Export
Description : Document export
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX
-}

module Gargantext.API.Node.Document.Export
  where

import qualified Data.Text as T
import Data.Version (showVersion)
import Gargantext.API.Node.Document.Export.Types
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core (toDBid)
import Gargantext.Core.Types
-- import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Database.Query.Facet (runViewDocuments, Facet(..))
import Gargantext.Database.Query.Table.Node (getClosestParentIdByType)
import Gargantext.Database.Schema.Node (NodePoly(..))
import Gargantext.Prelude
import qualified Paths_gargantext as PG -- cabal magic build module
-- import Servant (Proxy(..))

--------------------------------------------------
-- | Hashes are ordered by Set
getDocuments :: UserId
             -> DocId
             -> GargNoServer DocumentExport
getDocuments uId pId = do
  printDebug "[getDocuments] pId" pId
  mcId <- getClosestParentIdByType pId NodeCorpus
  let cId = maybe (panic "[G.A.N.D.Export] Node has no parent") identity mcId
  printDebug "[getDocuments] cId" cId
  --docs <- getDocumentsWithParentId cId -- NodeDocument (Proxy :: Proxy HyperdataDocument)
  docs <- runViewDocuments cId False Nothing Nothing Nothing Nothing
  printDebug "[getDocuments] got docs" docs
  --de_docs <- mapM mapFacetDoc docs
  pure $ DocumentExport { _de_documents = mapFacetDoc <$> docs
                        , _de_garg_version = T.pack $ showVersion PG.version }
  where
    mapFacetDoc (FacetDoc { .. }) =
      Document { _d_document = 
                 Node { _node_id = facetDoc_id
                      , _node_hash_id = Nothing
                      , _node_typename = toDBid NodeDocument
                      , _node_user_id = uId
                      , _node_parent_id = Nothing
                      , _node_name = facetDoc_title
                      , _node_date = facetDoc_created
                      , _node_hyperdata = facetDoc_hyperdata }
               , _d_ngrams = Ngrams { _ng_ngrams = []
                                    , _ng_hash = "" }
               , _d_hash = "" }
    _mapDoc d = Document { _d_document = d
                         , _d_ngrams   = Ngrams { _ng_ngrams = []
                                                , _ng_hash = "" }
                         , _d_hash     = ""}
