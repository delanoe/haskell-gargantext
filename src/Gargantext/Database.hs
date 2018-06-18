{-|
Module      : Gargantext.Database
Description : Main commands of BASHQL a Domain Specific Language to deal with Gargantext Database.
Copyright   : (c) CNRS, 2017-Present
License     : AGPL + CECILL v3
Maintainer  : team@gargantext.org
Stability   : experimental
Portability : POSIX

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

module Gargantext.Database ( module Gargantext.Database.Utils
                           , get
                           , ls  , ls'
                           , home, home'
                           , post, post', postR'
                           , del , del'
                           )
    where

import Gargantext.Core.Types
import Gargantext.Core.Types.Node
import Gargantext.Database.Utils (connectGargandb)
import Gargantext.Database.Node
import Gargantext.Prelude
import Database.PostgreSQL.Simple (Connection)
import Data.Text (Text)
import Opaleye hiding (FromField)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (last)
type UserId = Int
--type NodeId = Int

-- List of NodeId
-- type PWD a = PWD UserId [a]
type PWD = [NodeId]
--data PWD' a = a | PWD' [a]

-- | TODO get Children or Node
get :: Connection -> PWD -> IO [Node Value]
get _ [] = pure []
get conn pwd = runQuery conn $ selectNodesWithParentID (last pwd)

-- | Home, need to filter with UserId
home :: Connection -> IO PWD
home c = map node_id <$> getNodesWithParentId c 0 Nothing

-- | ls == get Children
ls :: Connection -> PWD -> IO [Node Value]
ls = get

-- | TODO
-- post User
-- post Dir
-- post Corpus Parent_id (Empty|MyData)
-- post CorpusWith
-- post List
post :: Connection -> PWD -> [NodeWrite'] -> IO Int64
post _ [] _   = pure 0
post _ _ []   = pure 0
post c pth ns = mkNode c (last pth) ns

postR :: Connection -> PWD -> [NodeWrite'] -> IO [Int]
postR _ [] _   = pure [0]
postR _ _ []   = pure [0]
postR c pth ns = mkNodeR c (last pth) ns


rm :: Connection -> PWD -> [NodeId] -> IO Int
rm = del

del :: Connection -> PWD -> [NodeId] -> IO Int
del _ [] _ = pure 0
del _ _ [] = pure 0
del c pth ns = deleteNodes c ns

put :: Connection -> PWD -> [a] -> IO Int64
put = undefined

-- | TODO
-- cd (Home UserId) | (Node NodeId)
-- cd Path
-- jump NodeId
-- touch Dir

--------------------------------------------------------------
-- Tests
--------------------------------------------------------------

home' :: IO PWD
home' = do
  c <- connectGargandb "gargantext.ini"
  home c

ls' :: IO [Node Value]
ls' = do
  c <- connectGargandb "gargantext.ini"
  h <- home c
  ls c h

type Children a = Maybe a

post' :: IO Int64
post'  = do
  c <- connectGargandb "gargantext.ini"
  h <- home c
  let userId = 1
  post c h [ node userId (last h) Corpus  "name" "{}"
           , node userId (last h) Project "name" "{}"
           ]


postR' :: IO [Int]
postR'  = do
  c <- connectGargandb "gargantext.ini"
  h <- home c
  let userId = 1
  postR c h [ node userId (last h) Corpus  "name" "{}"
           , node userId (last h) Project "name" "{}"
           ]


del' :: [NodeId] -> IO Int
del' ns = do
  c <- connectGargandb "gargantext.ini"
  h <- home c
  del c h ns



