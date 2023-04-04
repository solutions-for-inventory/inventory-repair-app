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
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Server (printSchema)
import           Data.Morpheus.Types (RootResolver (..), GQLType(..), Undefined(..), QUERY, MUTATION, GQLRequest, GQLResponse, ResolverQ)
import           Import hiding (Proxy, proxy, ByteString, Query)
import           Data.Proxy (Proxy(..))
import           Graphql.Session
import           Graphql.Admin.Privilege
import           Graphql.Admin.Role
import           Graphql.Admin.DataTypes
import           Graphql.Admin.Person
import           Graphql.Admin.User
import           Graphql.PartCategory
import           Graphql.Unit
import           Graphql.Part.Resolvers
import           Graphql.Inventory.Resolvers
import           Graphql.InventoryPart.Resolvers
import           Graphql.Utils ()
import           Graphql.DataTypes

data Query m = Query {
                            session :: m Session
                          , privileges :: m (Privileges m)
                          , roles :: m (Roles m)
                          , persons :: m (Persons m)
                          , users :: m (Users m)
                          , categories :: PartCategoryFilter -> m [PartCategory]
                          , units :: m [Unit]
                          , inventories :: m (Inventories m)
                          , items :: m (Items m)
                          , inventoryItems :: m (InventoryItems m)
                         } deriving (Generic, GQLType)

data Mutation m = Mutation {
                             savePrivilege :: PrivilegeArg -> m Privilege
                           , saveRole :: RoleArg -> m (Role m)
                           , persons :: m (Persons m)
                           , users :: m (Users m)
                           , saveCategory :: PartCategoryArg -> m PartCategory
                           , saveUnit :: UnitArg -> m Unit
                           , inventoryItems :: m (InventoryItems m)
                           , items :: m (Items m)
                           , inventories :: m (Inventories m)
                           } deriving (Generic, GQLType)

-- | The query resolver
--resolveQuery::Query (QUERY () Handler)
resolveQuery = Query {
                         session = getUserSessionResolver
                       , privileges = resolvePrivilege
                       , roles = resolveRole ()
                       , persons = personResolver ()
                       , users = userResolver ()
                       , categories = listCategoryResolver
                       , units = listUnitResolver ()
                       , inventories = inventoryResolver ()
                       , items = itemResolver ()
                       , inventoryItems = inventoryItemsResolver ()
                       }
-- | The mutation resolver
--resolveMutation::Mutation (MUTATION () Handler)
resolveMutation = Mutation {
                             savePrivilege = resolveSavePrivilege
                           , saveRole =  resolveSaveRole
                           , persons = personResolver ()
                           , users = userResolver ()
                           , saveCategory = saveCategoryResolver
                           , saveUnit = saveUnitResolver
                           , inventories = inventoryResolver ()
                           , items = itemResolver ()
                           , inventoryItems = inventoryItemsResolver ()
                           }

-- BASE EXAMPLE
-- https://github.com/dnulnets/haccessability
--dbFetchDeity:: Text -> Handler Deity
--dbFetchDeity name = do
--                     let userId = (toSqlKey 3)::User_Id
--                     deity <- runDB $ getEntity userId
--                     return $ Deity {fullName = "dummy", power = Just "Shapeshifting", tests = testsResolver}

rootResolver :: RootResolver Handler () Query Mutation Undefined
rootResolver = RootResolver {
                                 queryResolver = resolveQuery
                               , mutationResolver = resolveMutation
--                               , subscriptionResolver = Undefined
                               }

-- | Compose the graphQL api
api:: GQLRequest -> Handler GQLResponse
api r = do
         interpreter rootResolver r

--api :: ByteString -> Handler ByteString
--api = interpreter rootResolver

--apiDoc :: Data.ByteString.Lazy.Internal.ByteString
--apiDoc = toGraphQLDocument $ Just rootResolver

proxy :: Proxy (RootResolver IO () Query Mutation Undefined)
proxy = Proxy

apiDoc = printSchema $ proxy
