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
{-# LANGUAGE RecordWildCards       #-}

module Graphql.InventoryPart.Resolvers (
        inventoryItemsResolver
      , getInventoryItemByIdResolver_
      , inventoryItemsPageResolver_
      , inventoryItemsItemPageResolver_
      , saveInventoryItemResolver
      , saveInventoryItemsResolver
      , toInventoryItemQL
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Graphql.Utils
import {-# SOURCE #-} Graphql.Part.Resolvers
import {-# SOURCE #-} Graphql.Inventory.Resolvers
import Graphql.Unit ()
import Graphql.DataTypes
import Graphql.DataTypes ()
import Graphql.InventoryPart.Persistence

-- Query Resolvers
--inventoryItemsResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (InventoryItems o)
inventoryItemsResolver _ = pure InventoryItems { inventoryItem = findInventoryItemByIdResolver
                                               , page = inventoryItemsPageResolver
                                               , saveInventoryItem = saveInventoryItemResolver
                                               }

--findInventoryItemByIdResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => EntityIdArg -> t Handler (InventoryItem o)
findInventoryItemByIdResolver EntityIdArg {..} = lift $ do
                                              let inventoryItemId = (toSqlKey $ fromIntegral $ entityId)::InventoryPart_Id
--                                              let inventoryItemId = InventoryPart_Key {unInventoryPart_Key  = partId}
                                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                                              return $ toInventoryItemQL inventoryItem

--getInventoryItemByIdResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => InventoryPart_Id -> () -> t Handler (InventoryItem o)
getInventoryItemByIdResolver_ inventoryItemId = lift $ do
                                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                                              return $ toInventoryItemQL inventoryItem

--inventoryItemsPageResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => Inventory_Id -> PageArg -> t Handler (Page (InventoryItem o))
--inventoryItemsPageResolver_ :: Entity Inventory_ -> PageArg -> Page (InventoryItem (t1 (HandlerFor site0)))
inventoryItemsPageResolver_ inventoryId page = lift $ do
                                    countItems <- inventoryItemQueryCount page
                                    items <- inventoryItemQuery page
                                    let itemsQL = P.map (\r -> toInventoryItemQL r) items
                                    return Page { totalCount = countItems
                                                , content = itemsQL
                                                , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                                      , hasPreview = pageIndex' * pageSize' > 0
                                                                      , pageSize = pageSize'
                                                                      , pageIndex = pageIndex'
                                                }
                                    }
                                     where
                                       PageArg {..} = page
                                       pageIndex' = case pageIndex of Just  x  -> x; Nothing -> 0
                                       pageSize' = case pageSize of Just y -> y; Nothing -> 10

--inventoryItemsItemPageResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => Part_Id -> PageArg -> t Handler (Page (InventoryItem o))
inventoryItemsItemPageResolver_ partId (PageArg {..}) = lift $ do
                                    countItems <- runDB $ count ([InventoryPart_PartId ==. partId] :: [Filter InventoryPart_])
                                    items <- runDB $ selectList [InventoryPart_PartId ==. partId] [Asc InventoryPart_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                                    let itemsQL = P.map (\r -> toInventoryItemQL r) items
                                    return Page { totalCount = countItems
                                                , content = itemsQL
                                                , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                                      , hasPreview = pageIndex' * pageSize' > 0
                                                                      , pageSize = pageSize'
                                                                      , pageIndex = pageIndex'
                                                }
                                    }
                                     where
                                      pageIndex' = case pageIndex of
                                                    Just  x  -> x
                                                    Nothing -> 0
                                      pageSize' = case pageSize of
                                                      Just y -> y
                                                      Nothing -> 10

--inventoryItemsPageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (InventoryItem o))
inventoryItemsPageResolver PageArg {..} = lift $ do
                        countItems <- runDB $ count ([] :: [Filter InventoryPart_])
                        items <- runDB $ selectList [] [Asc InventoryPart_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                        let itemsQL = P.map (\r -> toInventoryItemQL r) items
                        return Page { totalCount = countItems
                                    , content = itemsQL
                                    , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                          , hasPreview = pageIndex' * pageSize' > 0
                                                          , pageSize = pageSize'
                                                          , pageIndex = pageIndex'
                                    }
                        }
                         where
                          pageIndex' = case pageIndex of
                                        Just  x  -> x
                                        Nothing -> 0
                          pageSize' = case pageSize of
                                          Just y -> y
                                          Nothing -> 10

-- Mutation Resolvers
--saveInventoryItemResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => InventoryPartArg -> t Handler (InventoryItem o)
saveInventoryItemResolver arg = lift $ do
                              inventoryItemId <- createOrUpdateInventoryItem arg
                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                              return $ toInventoryItemQL inventoryItem

--saveInventoryItemsResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => InventoryItemsArg -> t Handler [InventoryItem o]
saveInventoryItemsResolver arg = lift $ do
                              let InventoryItemsArg {..} = arg
                              let inventoryItemArgs =  P.map (\partId -> InventoryPartArg { inventoryItemId = 0
                                                                                          , level = level
                                                                                          , maxLevelAllowed = maxLevelAllowed
                                                                                          , minLevelAllowed = minLevelAllowed
                                                                                          , price = price
                                                                                          , location = location
                                                                                          , inventoryId = inventoryId
                                                                                          , dateExpiry = dateExpiry
                                                                                          , partId = partId
                                                                                          }) itemIds
                              inventoryItemIds <- createOrUpdateInventoryItems inventoryItemArgs
                              inventoryItems <- runDB $ mapM getJustEntity inventoryItemIds
                              return $ P.map toInventoryItemQL inventoryItems

--toInventoryItemQL :: (Typeable o, MonadTrans (o ())) => Entity InventoryPart_ -> InventoryItem o
toInventoryItemQL (Entity inventoryItemId inventoryItem) = InventoryItem { inventoryItemId = fromIntegral $ fromSqlKey inventoryItemId
                                                                         , level = inventoryPart_Level
                                                                         , maxLevelAllowed = inventoryPart_MaxLevelAllowed
                                                                         , minLevelAllowed = inventoryPart_MinLevelAllowed
                                                                         , price = realToFrac inventoryPart_Price
                                                                         , location = inventoryPart_Location
                                                                         , status = T.pack $ show inventoryPart_Status
                                                                         , dateExpiry = de
                                                                         , inventory = getInventoryByIdResolver_ inventoryPart_InventoryId
                                                                         , item = getItemByIdResolver_ inventoryPart_PartId
                                                                         , createdDate = fromString $ show inventoryPart_CreatedDate
                                                                         , modifiedDate = m
                                                                         }
                            where
                              InventoryPart_ {..} = inventoryItem
                              m = case inventoryPart_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing
                              de = case inventoryPart_DateExpiry of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing
