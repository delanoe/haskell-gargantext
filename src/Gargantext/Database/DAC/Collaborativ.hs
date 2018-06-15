{-|
Module      : Collaborativ
Description : Collaborativ framework 
Copyright   : (c) Alexandre Delanoe, 2017
License     : AGPL + CECILL v3
Maintainer  : alexandre.delanoe@iscpif.fr
Stability   : experimental
Portability : POSIX

Collaborativ framework describing main functions (in a perspective of a
Decentralized Autonomous Organization in mind).

-}

module Collaborativ where

type Proba = Double
-- Proba :: somme == 1

-- Main Concepts
-- BlockChain | SideChain | NodeChain
-- Inside the Node Chain: we have the history of all decisions

-- Axiom 1 : all Nodes have a Citizen at least
type Citizen = NodeUser

-- Community is a Node of 
type Community = NodeCommunity


-- | Rights
data Rights = Read | Write | Execute | Share | Clone

-- | Main policies implemented
-- Public policy: means anyone can RWES

type Free = Public

data Policy = Public             | Ancestor     | DictatorShip 
            | PreferenceMajority | BoolMajority | Unanimity 

runPolicy :: Citizen -> Node -> Bool
runPolicy = undefined

hasCitizen :: Node -> Set User
hasCitizen = undefined

isCitizen :: User -> Node -> Bool
isCitizen user node = member user (citizens node)

hasPolicy :: Node -> Policy
hasPolicy = undefined

--




-- | For a given Node, citizenShip is shared by Citizen according to a Policy
citizenRights :: 

ownership :: Node -> Map User Proba
ownership = undefined

nodePolicy :: Node -> Map User (Proba, Bool)
nodePolicy = undefined

-- | Starting from the simple case:
-- one can share a Node only, with Node th
-- One user can not share to himself (useless)
-- One user can not share a Shareable ressource to all the world
-- One user can not share a Shareable ressource to a group according
--      -> to anybody in the group
--      -> according to policy rules (dictatorship of one User, majority rule or others)
-- One user can     share a Shareable ressource individually 
--      -> to anybody individually if he knows his uniq ID

share :: for all Shareable a => User -> a -> Set User -> Bool
share = undefined

instance Shareable User where
    policy = undefined






