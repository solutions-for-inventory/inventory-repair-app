{-# OPTIONS_GHC -w #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Graphql.Root (api, apiDoc) where

import           GHC.Generics
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     ()
import           Data.Morpheus.Types        (RootResolver (..), GQLType(..), Undefined(..), Res, MutRes, GQLRequest, GQLResponse)
import           Data.Morpheus.Document (toGraphQLDocument)
import           Import
import           Data.ByteString.Lazy.Internal (ByteString)
import           Graphql.Session
import           Graphql.Admin.Privilege
import           Graphql.Admin.Role
import           Graphql.Admin.DataTypes
import           Graphql.Admin.Person
import           Graphql.Admin.User
import           Graphql.PartCategory
import           Graphql.Unit
--import           Graphql.Maintenance.SubTask.SubTaskKind
--import           Graphql.Maintenance.Task.TaskCategory
import           Graphql.Part.Resolvers
import           Graphql.Inventory.Resolvers
import           Graphql.InventoryPart.Resolvers
import           Graphql.Utils ()
import           Graphql.DataTypes
--import           Graphql.Human.EmployeeJob
-- importGQLDocumentWithNamespace "schema.gql"

data QueryQL m = QueryQL { -- deity :: DeityArgs -> m Deity
                           session :: () -> Res () Handler Session
                         , privileges :: () -> m Privileges
                         , roles :: () -> m Roles
--                         , persons :: () -> m Persons
--                         , users :: () -> m Users
                         , persons :: () -> Res () Handler (Persons Res)
                         , users :: () -> Res () Handler (Users Res)
                         , categories :: PartCategoryFilter -> m [PartCategory]
                         , units :: () -> m [Unit]
                         , inventories :: () -> Res () Handler (Inventories Res)
                         , items :: () -> Res () Handler (Items Res)
                         , inventoryItems :: () -> Res () Handler (InventoryItems Res)
                         } deriving (Generic, GQLType)

data Mutation m = Mutation { savePrivilege :: PrivilegeArg -> m Privilege
                           , saveRole :: RoleArg -> m (Role MutRes)
                           , persons :: () -> MutRes () Handler (Persons MutRes)
                           , users :: () -> MutRes () Handler (Users MutRes)
--                           , savePerson :: PersonArg -> m (Person MutRes)
                           , saveCategory :: PartCategoryArg -> m PartCategory
                           , saveUnit :: UnitArg -> m Unit
--                           , saveTaskCategory :: TaskPartCategoryArg -> m TaskCategory
--                           , saveSubTaskKind :: SubTaskKindArg -> m SubTaskKind
--                           , saveEmployeeJob :: EmployeeJobArg -> m EmployeeJob
--                           , saveInventory :: InventoryArg -> m (Inventory MutRes)
--                           , saveItem :: PartArg -> m (Part MutRes)
--                           , saveInventoryItem :: InventoryPartArg -> m (InventoryItem MutRes)
                           , inventoryItems :: () -> MutRes () Handler (InventoryItems MutRes)
                           , items :: () -> MutRes () Handler (Items MutRes)
                           , inventories :: () -> MutRes () Handler (Inventories MutRes)
                           } deriving (Generic, GQLType)

--data DeityArgs = DeityArgs { name :: Text, mythology :: Maybe Text } deriving (Generic)

-- | The query resolver
resolveQuery::QueryQL (Res () Handler)
resolveQuery = QueryQL { --deity = resolveDeity
                         session = getUserSessionResolver
                       , privileges = resolvePrivilege
                       , roles = resolveRole
                       , persons = personResolver
                       , users = userResolver
                       , categories = listCategoryResolver
                       , units = listUnitResolver
                       , inventories = inventoryResolver
                       , items = itemResolver
                       , inventoryItems = inventoryItemsResolver
                       }
-- | The mutation resolver
resolveMutation::Mutation (MutRes () Handler)
resolveMutation = Mutation { savePrivilege = resolveSavePrivilege
                           , saveRole =  resolveSaveRole
                           , persons = personResolver
                           , users = userResolver
                           , saveCategory = saveCategoryResolver
                           , saveUnit = saveUnitResolver
                           , inventories = inventoryResolver
                           , items = itemResolver
                           , inventoryItems = inventoryItemsResolver
                           }


-- BASE EXAMPLE
-- https://github.com/dnulnets/haccessability
--dbFetchDeity:: Text -> Handler Deity
--dbFetchDeity name = do
--                     let userId = (toSqlKey 3)::User_Id
--                     deity <- runDB $ getEntity userId
--                     return $ Deity {fullName = "dummy", power = Just "Shapeshifting", tests = testsResolver}

--resolveDeity :: DeityArgs -> Res e Handler Deity
--resolveDeity DeityArgs { name, mythology } = lift $ dbFetchDeity name

--testsResolver :: TestArg -> Res e Handler NoDeity
--testsResolver TestArg {yourFullName } = pure NoDeity {noFullName = "Test no full am", nopower = Just "no power"}

rootResolver :: RootResolver Handler () QueryQL Mutation Undefined
rootResolver = RootResolver { queryResolver = resolveQuery
                               , mutationResolver = resolveMutation
                               , subscriptionResolver = Undefined
                               }

-- | Compose the graphQL api
api:: GQLRequest -> Handler GQLResponse
api r = do
         interpreter rootResolver r

apiDoc :: Data.ByteString.Lazy.Internal.ByteString
apiDoc = toGraphQLDocument $ Just rootResolver
