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
logic, the semantics of BASHQL focus on the function first. 

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
                           , ls')
    where

import Gargantext.Core.Types
import Gargantext.Database.Utils (connectGargandb)
import Gargantext.Database.Node
import Gargantext.Prelude
import Database.PostgreSQL.Simple (Connection)

import Opaleye hiding (FromField)
import Data.Aeson
-- type PWD  =  Node NodeId
-- type Path = [PWD]

-- pwd :: [Node NodeId] -> 


ls :: Connection -> Int -> IO [Node Value]
ls conn n = runQuery conn $ selectNodesWithParentID n

ls' :: IO [Node Value]
ls' = connectGargandb "gargantext.ini" >>= \c -> ls c 347474


-- ls' Maybe PWD
-- cd (Home UserId) | (Node NodeId)

-- cd Path
-- jump PWD

-- mk User
-- mk Dir
-- mk Corpus Parent_id (Empty|MyData)
-- mk CorpusWith
-- mk List
-- touch Dir

