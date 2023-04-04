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

module Graphql.Part.Resolvers (
        itemResolver
      , getItemByIdResolver_
      , saveItemResolver
      , createOrUpdateItem
      , toItemQL
      , availableItemsPageResolver_
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Data.Text as T
import Prelude as P
import Graphql.Utils
import Enums
import Data.Time ()
import Graphql.PartCategory
import Graphql.DataTypes
import Graphql.InventoryPart.Resolvers
import Graphql.Unit
import Graphql.Part.Persistence

-- Query Resolvers
--getItemByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (Part o)
getItemByIdResolver EntityIdArg {..} = lift $ do
                                              let partId = (toSqlKey $ fromIntegral $ entityId)::Part_Id
                                              item <- runDB $ getJustEntity partId
                                              return $ toItemQL item

--forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Part_Id -> o (Part o)
getItemByIdResolver_ partId = lift $ do
                                         item <- runDB $ getJustEntity partId
                                         return $ toItemQL item

--itemsPageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (Part o))
itemsPageResolver page = lift $ do
                        countItems <- itemQueryCount page
                        result <- itemQuery page
                        let itemsQL = P.map (\r -> toItemQL r) result
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
                          pageIndex' = case pageIndex of
                                        Just  x  -> x
                                        Nothing -> 0
                          pageSize' = case pageSize of
                                          Just y -> y
                                          Nothing -> 10

--availableItemsPageResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => Inventory_Id -> PageArg -> t (HandlerFor App) (Page (Part o))
availableItemsPageResolver_ inventoryId page = lift $ do
                        countItems <- availableItemsQueryCount inventoryId page
                        result <- availableItemsQuery inventoryId page
                        let itemsQL = P.map (\r -> toItemQL r) result
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
                            pageIndex' = case pageIndex of
                                          Just  x  -> x
                                          Nothing -> 0
                            pageSize' = case pageSize of
                                            Just y -> y
                                            Nothing -> 10

--itemResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => p -> f (Items o)
itemResolver _ = pure Items { item = getItemByIdResolver
                            , page = itemsPageResolver
                            , saveItem = saveItemResolver
                            , changeItemStatus = changeItemStatusResolver
                            }

-- itemResolver :: Items (QUERY () Handler)
-- itemResolver = Items {  item = getItemByIdResolver, page = itemsPageResolver }

-- categoryResolver :: PartCategory_Id -> () -> QUERY e Handler PartCategory
--categoryResolver categoryId arg = lift $ do
--                                      category <- dbFetchCategoryById categoryId
--                                      return category

--getUnitByIdResolver_ unitId _ = lift $ do
--                                      unit <- dbFetchUnitById unitId
--                                      return unit

--toItemQL :: forall {m :: * -> *}. Entity Part_ -> Part m
toItemQL (Entity partId item) = Part { partId = fromIntegral $ fromSqlKey partId
                                     , partNumber = part_PartNumber
                                     , name = part_Name
                                     , defaultPrice = realToFrac part_DefaultPrice
                                     , description = part_Description
                                     , manufacturer = part_Manufacturer
                                     , model = part_Model
                                     , notes = part_Notes
                                     , status = T.pack $ show part_Status
                                     , images = part_Images
                                     , partCategory = case part_PartCategoryId of Nothing -> Nothing; Just c -> Just $ getCategoryByIdResolver_ c
                                     , unit = case part_UnitId of Nothing -> Nothing; Just u -> Just $ getUnitByIdResolver_ u
                                     , inventoryItems = inventoryItemsItemPageResolver_ partId
                                     , createdDate = fromString $ show part_CreatedDate
                                     , modifiedDate = m
                                     }
                            where
                              Part_ {..} = item
                              m = case part_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing

changeItemStatusResolver :: MonadTrans t => EntityChangeStatusArg -> t Handler Bool
changeItemStatusResolver EntityChangeStatusArg {..} = lift $ do
                              () <- changeStatus entityIds (readEntityStatus status)
                              return True

--saveItemResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PartArg -> t Handler (Part o)
saveItemResolver arg = lift $ do
                              partId <- createOrUpdateItem arg
                              item <- runDB $ getJustEntity partId
                              return $ toItemQL item
