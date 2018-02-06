-- Right Management
-----------------------------------------------------------------
-- data Management = RolesRights | NodesRights | OperationsRights
-----------------------------------------------------------------
-----------------------------------------------------------------
-- Role Rights Management
--    rights to create roles (group)
-- Node Rights Management
--    rights to read/write Node
-- Operation Rights Management
--    rights for which operations
-----------------------------------------------------------------
-- Roles Rights Management
-----------------------------------------------------------------
-- 2 main roles
-- admin : can create group and assign Node Rights to it
-- user  : can not create group and assign Node rights inside his group (if he has the rights)

-- Use cases:
-- if all user are in public and have read/write permissions: everything is free inside the public group
-- else:
--      in X institution x admin can create an gx group or a gy group for each department and assign user to it
--      users y can share with user y withing the group if he has the rights for it
--      an admin can give admin group to a user

-- Roles Rights Management are stored in "User Node"
-- right to read on group called "x"  == can share permissions inside group x
-- right to write on group called "x" == can modify group x itself

-- Question: how to manage the hierarchy of roles/groups ?
-- Example: use can create a group inside a group but not outside of it

-----------------------------------------------------------------
-- Node Rights Management
-----------------------------------------------------------------
-- Les actions sur un Node (if /= Graph) depends on the rights of his parent

-- | rightsOf: 
-- technically : get the column Node (in table nodes) with rights (ACL)
rightsOf :: Node -> Rights
rightsOf n = undefined

rightsOfNode :: User -> Node -> Rights
rightsOfNode u n = case n of
                UserNode     -> rightsOf n
                ProjectNode  -> rightsOf n
                CorpusNode   -> rightsOf n
                GraphNode    -> rightsOf n
                _            -> rightsOf (parentOf n)

rightsOfNodeNgram :: User -> NodeNgram -> Rights
rightsOfNodeNgram u n = rightsOf n'
    where 
        n' = nodeOf n

rightsOfNodeNgramNgram :: User -> NodeNgramNgram -> Rights
rightsOfNodeNgramNgram u n = rightsOf n'
    where 
        n' = nodeOf n

rightsOfNodeNodeNgram
rightsOfNodeNode


-----------------------------------------------------------------
-- Operation Rights Management
-----------------------------------------------------------------
data Operation = Read | Write 
-- Starting with simple case:
-- type ModifyRights = Write
-- type Exec = Write

data Rights = { _rightsRead  :: Bool
              , _rightsWrite :: Bool
              }
    deriving (Show, Read, Eq)

data LogRightsMessage = RightsSuccess | RightsError
    deriving (Show, Read, Eq)

type Read  = Bool
type Write = Bool


-----------------------------------------------------------------
-- | TODO 
-- find the tables where there is the relation Node / User / Rights
getRightsOfNodeWithUser :: Node -> User -> IO Rights
getRightsOfNodeWithUser n u = undefined

userCan :: Operation -> User -> Node -> IO Bool
userCan op u n = do
    rights <- getRightsOfNodeWithUser u n
    r = case op of
          Read  -> _rightsRead  rights
          Write -> _rightsWrite rights
    pure (r == True)

-- | User can (or can not) give/change rights of the Node
userCanModifyRights :: User -> Node -> IO Bool
userCanModifyRights u n = True `==` <$> userCan Write u n

-- | User can see who has access to the Node
userCanReadRights :: User -> Node -> IO Bool
userCanReadRights u n = True `==` <$> userCan Read u n


chmod :: Rights -> User -> Node -> IO LogRightsMessage
chmod r u n = undefined

chmod' :: Read -> Write -> User -> Node -> IO LogRightsMessage
chmod' r w u n = chmod rights u n
    where
        rights = Rights r w


readAccessOnly :: User -> Node -> IO LogRightsMessage
readAccessOnly u n = chmod r u n
    where
        r = Rights True False


stopAccess :: User -> Node -> IO LogRightsMessage
stopAccess = 

chmodAll :: Rights -> User -> [Node] -> IO [LogRightsMessage]
chmd b r u ns = map (chmod b r u n) ns

chmodChildren :: Rights -> User -> [Node] -> IO [LogRightsMessage]
chmodChildren b r u n = map (chmod br u n) ns'
    where
        ns' = childrenOf n


