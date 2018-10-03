{-|
Module      : Gargantext.Database.Bashql
Description : BASHQL to deal with Gargantext Database.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

* BASHQL is a Domain Specific Language to deal with the Database

* BASHQL = functional (Bash * SQL)

* Which language to chose when working with a database ? To make it
simple, instead of all common Object Relational Mapping (ORM) [1]
strategy used nowadays inspired more by object logic than functional
logic, the semantics of BASHQL with focus on the function first.

* BASHQL focus on the function, i.e. use bash language function name,
and make it with SQL behind the scene. Then BASHQL is inspired more
by Bash language [2] than SQL and then follows its main commands as
specification and documentation.

* Main arguments:
  1. Theoritical: database and FileSystems are each thought as a single
  category, assumption based on theoretical work on databases by David Spivak [0].
  2. Practical argument: basic bash commands are a daily practice among
  developper community.

* How to help ?
  1. Choose a command you like in Bash
  2. Implement it in Haskell-SQL according to Gargantext Shema (Tree like
  filesystem)
  3. Translate it in BASHQL (follow previous implementations)
  4. Make a pull request    (enjoy the community)

* Implementation strategy: Functional adapations are made to the
gargantext languages options and SQL optimization are done continuously
during the project. For the Haskellish part, you may be inspired by
Turtle implementation written by Gabriel Gonzales [3] which shows how to
write Haskell bash translations.

* Semantics
- FileSystem is now a NodeSystem where each File is a Node in a Directed Graph (DG).

* References

[0] MIT Press has published "Category theory for the sciences". The book
can also be purchased on Amazon. Here are reviews by the MAA, by the
AMS, and by SIAM.

[1] https://en.wikipedia.org/wiki/Object-relational_mapping

[2] https://en.wikipedia.org/wiki/Bash_(Unix_shell)

[3] https://github.com/Gabriel439/Haskell-Turtle-Library

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Gargantext.Database.Bashql ( get, get'
                                  , ls  , ls'
                                  , home, home'
                                  , post, post'
                                  , del , del'
                                  , tree, tree'
                                  , postCorpus, postAnnuaire
                                 )
    where

import Control.Monad.Reader -- (Reader, ask)

import Data.Text (Text, pack)
import Data.Aeson
import Data.Aeson.Types
import Data.List (last, concat)

import Gargantext.Core.Types
import Gargantext.Database.Utils (connectGargandb)
import Gargantext.Database.Node
import Gargantext.Prelude

import Opaleye hiding (FromField)
--type UserId = Int
--type NodeId = Int

-- List of NodeId
-- type PWD a = PWD UserId [a]
type PWD = [NodeId]
--data PWD' a = a | PWD' [a]

-- | TODO get Children or Node
get :: PWD -> Cmd [Node Value]
get []  = pure []
get pwd = Cmd . ReaderT $ \conn -> runQuery conn $ selectNodesWithParentID (last pwd)

-- | Home, need to filter with UserId
home :: Cmd PWD
home = map node_id <$> Cmd (ReaderT (getNodesWithParentId 0 Nothing))

-- | ls == get Children
ls :: PWD -> Cmd [Node Value]
ls = get


tree :: PWD -> Cmd [Node Value]
tree p = do
  ns       <- get p
  children <- mapM (\n -> get [node_id n]) ns
  pure $ ns <> concat children


-- | TODO
post :: PWD -> [NodeWrite'] -> Cmd Int64
post [] _   = pure 0
post _ []   = pure 0
post pth ns = Cmd . ReaderT $ mkNode (last pth) ns

--postR :: PWD -> [NodeWrite'] -> Cmd [Int]
--postR [] _ _ = pure [0]
--postR _ [] _ = pure [0]
--postR pth ns c = mkNodeR (last pth) ns c

--rm :: Connection -> PWD -> [NodeId] -> IO Int
--rm = del

del :: [NodeId] -> Cmd Int
del [] = pure 0
del ns = deleteNodes ns

-- | TODO
--put :: Connection -> PWD -> [a] -> IO Int64
--put = undefined

-- | TODO
-- cd (Home UserId) | (Node NodeId)
-- cd Path
-- jump NodeId
-- touch Dir

type CorpusName = Text

postCorpus :: ToJSON a => CorpusName -> (a -> Text) -> [a] -> Cmd NewNode
postCorpus corpusName title ns = do
  pid <- last <$> home
  let uid = 1
  postNode uid pid ( Node' NodeCorpus  corpusName emptyObject
                       (map (\n -> Node' Document (title n) (toJSON n) []) ns)
                   )

-- |
-- import IMTClient as C
-- postAnnuaire "Annuaire IMT" (\n -> (maybe "" identity (C.prenom n)) <> " " <> (maybe "" identity (C.nom n))) (take 30 annuaire)
postAnnuaire :: ToJSON a => CorpusName -> (a -> Text) -> [a] -> Cmd NewNode
postAnnuaire corpusName title ns = do
  pid <- last <$> home
  let uid = 1
  postNode uid pid ( Node' Annuaire  corpusName emptyObject
                       (map (\n -> Node' UserPage (title n) (toJSON n) []) ns)
                   )

--------------------------------------------------------------
-- Tests
--------------------------------------------------------------


get' :: PWD -> IO [Node Value]
get' = runCmd' . get

home' :: IO PWD
home' = runCmd' home

ls' :: IO [Node Value]
ls' = runCmd' $ do
  h <- home
  ls h

tree' :: IO [Node Value]
tree' = runCmd' $ do
  h <- home
  tree h

post' :: IO NewNode
post' = runCmd' $ do
  pid <- last <$> home
  let uid = 1
  postNode uid pid ( Node' NodeCorpus  (pack "Premier corpus") emptyObject [ Node' Document (pack "Doc1") emptyObject []
                                                          , Node' Document (pack "Doc2") emptyObject []
                                                          , Node' Document (pack "Doc3") emptyObject []
                                                          ]
                     )

-- | 
-- myCorpus <- Prelude.map doc2hyperdataDocument <$> toDocs <$> snd <$> readCsv "doc/corpus_imt/Gargantext_Corpus_small.csv"
-- There is an error in the CSV parsing...
-- let myCorpus' = Prelude.filter (\n -> T.length (maybe "" identity (hyperdataDocument_title n)) > 30) myCorpus


del' :: [NodeId] -> IO Int
del' ns = runCmd' $ del ns

-- corporaOf :: Username -> IO [Corpus]

runCmd' :: Cmd a -> IO a
runCmd' f = do
  c <- connectGargandb "gargantext.ini"
  runCmd c f
