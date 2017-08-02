-- | CNRS Copyrights 
-- Licence: https://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE
-- Author: Alexandre Delanoë (alexandre.delanoe@iscpif.fr)

module Data.Gargantext.Types.Main where

import Protolude (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Gargantext.Prelude
import Data.Gargantext.Types.Node ( NodePoly
                               , HyperdataFolder   , HyperdataCorpus   , HyperdataDocument
                               , HyperdataFavorites, HyperdataResource
                               , HyperdataList  , HyperdataScore
                               , HyperdataGraph
                               , HyperdataPhylo
                               , HyperdataNotebook
                               )

-- | TODO add Symbolic Node / Document
--   TODO make instances of Nodes

-- All the Database is structred like a hierachical Tree
-- Where a is a NodeType:
data Tree a = Empty | Node' a (Tree a) (Tree a) deriving (Show)

--gargTree :: Tree NodeType
--gargTree = Node' NodeUser Empty 
--                         (Node' Empty
--                                (Project Empty Empty)
--                         )
--


data NodeType = NodeUser
              | Folder | Project | Corpus | Document 
              | Favorites
              | NodeSwap
              | List  | StopList    | MainList | MapList | GroupList
              | Score | Occurrences | Cooccurrences | Specclusion | Genclusion | Cvalue 
              | Tficf | TfidfCorpus | TfidfGlobal   | TirankLocal | TirankGlobal

              deriving (Show, Eq)



-- | NodePoly indicates that Node has a Polymorphism Type
type Node json   = NodePoly Integer NodeTypeId Integer Integer Text UTCTime json
-- type Node json   = NodePoly NodeId NodeTypeId UserId ParentId NodeName UTCTime json
type NodeTypeId = Int

--type NodeUser    = Node HyperdataUser

-- | Then a Node can be either a Folder or a Corpus or a Document
type Folder   = Node HyperdataFolder
type Project  = Folder
type Corpus   = Node HyperdataCorpus
type Document = Node HyperdataDocument

-- | Community Manager Use Case
type Annuaire  = Corpus
type Individu  = Document

-- | Favorites Node enable Node categorization
type Favorites = Node HyperdataFavorites

-- | Favorites Node enable Swap Node with some synonyms for clarity
type NodeSwap  = Node HyperdataResource

-- | Then a Node can be a List which as some synonyms
type List = Node HyperdataList
type StopList   = List
type MainList   = List
type MapList    = List
type GroupList  = List

-- | Then a Node can be a Score which as some synonyms
type Score = Node HyperdataScore
type Occurrences   = Score
type Cooccurrences = Score
type Specclusion   = Score
type Genclusion    = Score
type Cvalue        = Score
type Tficf         = Score
-- TODO All these Tfidf* will be replaced with TFICF
type TfidfCorpus   = Tficf
type TfidfGlobal   = Tficf
type TirankLocal   = Tficf
type TirankGlobal  = Tficf

-- | Then a Node can be either a Graph or a Phylo or a Notebook
type Graph    = Node HyperdataGraph
type Phylo    = Node HyperdataPhylo
type Notebook = Node HyperdataNotebook


nodeTypes :: [(NodeType, NodeTypeId)]
nodeTypes = [ 
                --(NodeUser      ,  1)
--            
             (Project       ,  2)
            , (NodeSwap     , 19)
            , (Corpus        ,  3)
            , (Document      ,  4)
------  Lists
            , (StopList      ,  5)
            , (GroupList     ,  6)
            , (MainList      ,  7)
            , (MapList       ,  8)
--  Scores 
            , (Occurrences   , 10)
            , (Cooccurrences ,  9)
            
            , (Specclusion   , 11)
            , (Genclusion    , 18)
            , (Cvalue       , 12)

            , (TfidfCorpus  , 13)
            , (TfidfGlobal  , 14)

            , (TirankLocal  , 16)
            , (TirankGlobal , 17)

--  Node management 
            , (Favorites     , 15)

            ]
--
nodeTypeId :: NodeType -> NodeTypeId
nodeTypeId tn = fromMaybe (error ("Typename " ++ show tn ++ " does not exist")) (lookup tn nodeTypes)


