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

import Gargantext.API.Node.Document.Export.Types
import Gargantext.API.Prelude (GargNoServer)
import Gargantext.Core.Types
-- import Gargantext.Database.Admin.Types.Hyperdata (HyperdataDocument(..))
import Gargantext.Database.Query.Table.Node (getDocumentsWithParentId)
import Gargantext.Prelude
-- import Servant (Proxy(..))

--------------------------------------------------
-- | Hashes are ordered by Set
getDocuments :: DocId
             -> GargNoServer DocumentExport
getDocuments pId = do
  printDebug "[getDocuments] pId" pId
  docs <- getDocumentsWithParentId pId -- NodeDocument (Proxy :: Proxy HyperdataDocument)
  printDebug "[getDocuments] got docs" docs
  pure $ DocumentExport { _de_documents = mapDoc <$> docs
                        , _de_garg_version = "" }
  where
    mapDoc d = Document { _d_document = d
                        , _d_ngrams   = Ngrams { _ng_ngrams = []
                                             , _ng_hash = "" }
                        , _d_hash     = ""}
