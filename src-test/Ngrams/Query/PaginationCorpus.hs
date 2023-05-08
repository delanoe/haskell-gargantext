
module Ngrams.Query.PaginationCorpus where

import           Data.Map.Strict (Map)
import           Gargantext.API.Ngrams
import           Gargantext.Core.Types.Main
import           Gargantext.Database.Admin.Types.Node
import           Gargantext.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


implementationElem :: NgramsElement
implementationElem = NgramsElement {
    _ne_ngrams = "implementation"
  , _ne_size = 1
  , _ne_list = MapTerm
  , _ne_occurrences = Set.fromList [ NodeId 1, NodeId 2, NodeId 3, NodeId 4, NodeId 5 ]
  , _ne_root = Nothing
  , _ne_parent = Nothing
  , _ne_children = mSetFromList [ "code", "functions", "language", "programs" ]
  }

languagesElem :: NgramsElement
languagesElem = NgramsElement {
    _ne_ngrams = NgramsTerm {unNgramsTerm = "languages"}
  , _ne_size = 1
  , _ne_list = MapTerm
  , _ne_occurrences = Set.fromList [ NodeId 1, NodeId 2 , NodeId 3 , NodeId 4 ]
  , _ne_root = Nothing
  , _ne_parent = Nothing
  , _ne_children = mSetFromList [ "approach", "use" ]
  }

termsElem :: NgramsElement
termsElem = NgramsElement {
    _ne_ngrams = NgramsTerm {unNgramsTerm = "terms"}
  , _ne_size = 1
  , _ne_list = MapTerm
  , _ne_occurrences = Set.fromList [ NodeId 1, NodeId 2 , NodeId 3 ]
  , _ne_root = Nothing
  , _ne_parent = Nothing
  , _ne_children = mSetFromList [ "algorithm", "evaluation", "monad", "programmers" ]
  }

proofElem :: NgramsElement
proofElem = NgramsElement {
    _ne_ngrams = NgramsTerm {unNgramsTerm = "proof"}
  , _ne_size = 1
  , _ne_list = MapTerm
  , _ne_occurrences = Set.fromList [ NodeId 1, NodeId 2 ]
  , _ne_root = Nothing
  , _ne_parent = Nothing
  , _ne_children = mSetFromList [ "proofs" ]
  }

sideEffectsElem :: NgramsElement
sideEffectsElem = NgramsElement {
    _ne_ngrams = NgramsTerm {unNgramsTerm = "side effects"}
  , _ne_size = 1
  , _ne_list = StopTerm
  , _ne_occurrences = Set.fromList [ NodeId 1, NodeId 2, NodeId 3, NodeId 4, NodeId 5, NodeId 6 ]
  , _ne_root = Nothing
  , _ne_parent = Nothing
  , _ne_children = mSetFromList [ ]
  }

ooElem :: NgramsElement
ooElem = NgramsElement {
    _ne_ngrams = NgramsTerm {unNgramsTerm = "object oriented"}
  , _ne_size = 1
  , _ne_list = StopTerm
  , _ne_occurrences = Set.fromList [ NodeId 1, NodeId 2, NodeId 3, NodeId 4, NodeId 5 ]
  , _ne_root = Nothing
  , _ne_parent = Nothing
  , _ne_children = mSetFromList [ "null pointer exception" ]
  }

haskellElem :: NgramsElement
haskellElem = NgramsElement {
    _ne_ngrams = NgramsTerm {unNgramsTerm = "haskell"}
  , _ne_size = 1
  , _ne_list = CandidateTerm
  , _ne_occurrences = Set.fromList [ NodeId 1, NodeId 2, NodeId 3, NodeId 4, NodeId 5, NodeId 6, NodeId 7, NodeId 8 ]
  , _ne_root = Nothing
  , _ne_parent = Nothing
  , _ne_children = mSetFromList [ ]
  }

concHaskellElem :: NgramsElement
concHaskellElem = NgramsElement {
    _ne_ngrams = NgramsTerm {unNgramsTerm = "concurrent haskell"}
  , _ne_size = 1
  , _ne_list = CandidateTerm
  , _ne_occurrences = Set.fromList [ NodeId 1, NodeId 2, NodeId 3, NodeId 4, NodeId 5 ]
  , _ne_root = Nothing
  , _ne_parent = Nothing
  , _ne_children = mSetFromList [ "Simon Marlow" ]
  }

-- | A big (for the sake of the tests anyway) corpus which has
-- * 4 @MapTerm@s
-- * 4 @StopTerm@s
-- * 2 @CandidateTerm@s
paginationCorpus :: Versioned (Map NgramsTerm NgramsElement)
paginationCorpus = Versioned 0 $ Map.fromList [
    -- Map terms
    ( "implementation", implementationElem)
  , ( "languages", languagesElem)
  , ( "terms", termsElem)
  , ("proof", proofElem)

    -- Stop terms
  , ("side effects", sideEffectsElem)
  , ("object oriented", ooElem)

    -- Candidate terms
  , ("haskell", haskellElem)
  , ("concurrent haskell", concHaskellElem)
  ]
