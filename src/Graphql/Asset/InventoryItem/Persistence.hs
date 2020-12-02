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

module Graphql.Asset.InventoryItem.Persistence (
    createOrUpdateInventoryItems
  , createOrUpdateInventoryItem
  , inventoryItemQueryCount
  , inventoryItemQuery
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Data.Text as T
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (%), (++.), notIn, in_)
import Prelude as P
import qualified Data.Set as S
import Graphql.Utils
import Data.Time
import Graphql.Asset.Unit
import Graphql.Asset.DataTypes
import Graphql.Asset.Item.Persistence (itemFilters)
import Enums

getInventoryItemPredicate :: E.SqlExpr (Entity InventoryPart_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getInventoryItemPredicate item Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                              | T.strip field == "status" = [getOperator operator (item ^. InventoryPart_Status) (E.val (readEntityStatus $ T.strip value))]
                                              | T.strip field == "level" = [getOperator operator (item ^. InventoryPart_Level) (E.val $ parseToInteger value)]
                                              | T.strip field == "location" = [getOperator operator (item ^. InventoryPart_Location) (E.val $ T.strip value)]
                                              | T.strip field == "itemId" = [getOperator operator (item ^. InventoryPart_ItemId) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                              | T.strip field == "inventoryId" = [getOperator operator (item ^. InventoryPart_InventoryId) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                              | otherwise = []

getInventoryItemInPredicate :: E.SqlExpr (Entity InventoryPart_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getInventoryItemInPredicate item Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                                | T.strip field == "status" = [(item ^. InventoryPart_Status) `in_` (E.valList $ fromText readEntityStatus value)]
                                                | T.strip field == "level" = [(item ^. InventoryPart_Level) `in_` (E.valList $ fromText parseToInteger value)]
                                                | T.strip field == "location" = [(item ^. InventoryPart_Location) `in_` (E.valList $ fromText P.id value)]
                                                | T.strip field == "itemId" = [(item ^. InventoryPart_ItemId) `in_` (E.valList $ fromText (\ e -> toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                                | T.strip field == "inventoryId" = [(item ^. InventoryPart_InventoryId) `in_` (E.valList $ fromText (\ e -> toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                                | otherwise = []

getInventoryItemNotInPredicate :: E.SqlExpr (Entity InventoryPart_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getInventoryItemNotInPredicate item Predicate {..} | T.strip operator /= "notIn" || T.strip value == "" = []
                                                   | T.strip field == "status" = [(item ^. InventoryPart_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
                                                   | T.strip field == "level" = [(item ^. InventoryPart_Level) `notIn` (E.valList $ fromText parseToInteger value)]
                                                   | T.strip field == "location" = [(item ^. InventoryPart_Location) `notIn` (E.valList $ fromText P.id value)]
                                                   | T.strip field == "itemId" = [(item ^. InventoryPart_ItemId) `notIn` (E.valList $ fromText (\ e -> toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                                   | T.strip field == "inventoryId" = [(item ^. InventoryPart_InventoryId) `notIn` (E.valList $ fromText (\ e -> toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                                   | otherwise = []

getInventoryItemPredicates :: E.SqlExpr (Entity InventoryPart_) -> [Predicate] -> [[E.SqlExpr (E.Value Bool)]]
getInventoryItemPredicates _ [] = []
getInventoryItemPredicates item (x:xs) | P.length p == 0 = getInventoryItemPredicates item xs
                          | otherwise = p : getInventoryItemPredicates item xs
                   where
                      p = (getInventoryItemPredicate item x) P.++ (getInventoryItemInPredicate item x) P.++ (getInventoryItemNotInPredicate item x)

inventoryItemFilters :: Monad m => E.SqlExpr (Entity InventoryPart_) -> PageArg -> m (E.SqlExpr (E.Value Bool))
inventoryItemFilters item PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ getInventoryItemPredicates item justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (item ^. InventoryPart_Id E.==. item ^. InventoryPart_Id)
                            return predicates_


-- QUERIES
inventoryItemQueryCount :: PageArg -> Handler Int
inventoryItemQueryCount page =  do
                      result  <- runDB
                                   $ E.select
                                   $ E.from $ \ (item `E.InnerJoin` inventoryItem) -> do
                                     E.on $ item ^. Part_Id E.==. inventoryItem ^. InventoryPart_ItemId
                                     itemFilters <- itemFilters item page
                                     inventoryItemFilters <- inventoryItemFilters inventoryItem page
                                     E.where_ (itemFilters E.&&. inventoryItemFilters)
                                     return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ result

inventoryItemQuery :: PageArg -> Handler [Entity InventoryPart_]
inventoryItemQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ (item `E.InnerJoin` inventoryItem) -> do
                                     E.on $ item ^. Part_Id E.==. inventoryItem ^. InventoryPart_ItemId
                                     itemFilters <- itemFilters item page
                                     inventoryItemFilters <- inventoryItemFilters inventoryItem page
                                     E.where_ (itemFilters E.&&. inventoryItemFilters)
                                     E.offset $ pageIndex_ * pageSize_
                                     E.limit pageSize_
                                     return inventoryItem
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

createOrUpdateInventoryItems :: [InventoryItemArg] -> Handler [InventoryPart_Id]
createOrUpdateInventoryItems [] = pure []
createOrUpdateInventoryItems (x: xs) = do
                                        inventoryItemId <- createOrUpdateInventoryItem x
                                        inventoryItemIds <- createOrUpdateInventoryItems xs
                                        return (inventoryItemId : inventoryItemIds)

createOrUpdateInventoryItem :: InventoryItemArg -> Handler InventoryPart_Id
createOrUpdateInventoryItem inventoryItem = do
                            let InventoryItemArg {..} = inventoryItem
                            now <- liftIO getCurrentTime
                            itemEntityId <- if inventoryItemId > 0 then
                                        do
                                         let inventoryItemKey = (toSqlKey $ fromIntegral $ inventoryItemId)::InventoryPart_Id
--                                         let inventoryItemKey = InventoryPart_Key {unInventoryPart_Key  = itemId}
                                         _ <- runDB $ update inventoryItemKey [ InventoryPart_Level =. level
                                                                     , InventoryPart_MaxLevelAllowed =. maxLevelAllowed
                                                                     , InventoryPart_MinLevelAllowed =. minLevelAllowed
                                                                     , InventoryPart_Price =. realToFrac price
                                                                     , InventoryPart_Location =. location
                                                                     , InventoryPart_Status =. readEntityStatus status
                                                                     , InventoryPart_DateExpiry =.  Just now
                                                                     , InventoryPart_InventoryId =. ((toSqlKey $ fromIntegral inventoryId)::Inventory_Id)
--                                                                     , InventoryPart_ItemId =. ((toSqlKey $ fromIntegral itemId)::Part_Id)
                                                                     , InventoryPart_ModifiedDate =. Just now
                                                                     ]
                                         return inventoryItemKey
                                      else do
                                            itemKey <- runDB $ insert $ fromInventoryItemQL inventoryItem now Nothing
                                            return itemKey
                            return itemEntityId

fromInventoryItemQL :: InventoryItemArg -> UTCTime -> Maybe UTCTime -> InventoryPart_
fromInventoryItemQL (InventoryItemArg {..}) cd md = InventoryPart_ { inventoryPart_Level = level
                                                                   , inventoryPart_MaxLevelAllowed = maxLevelAllowed
                                                                   , inventoryPart_MinLevelAllowed = minLevelAllowed
                                                                   , inventoryPart_Price = realToFrac price
                                                                   , inventoryPart_Location = location
                                                                   , inventoryPart_Status = readEntityStatus status
                                                                   , inventoryPart_DateExpiry = md
                                                                   , inventoryPart_ItemId = (toSqlKey $ fromIntegral itemId)::Part_Id
                                                                   , inventoryPart_InventoryId = ((toSqlKey $ fromIntegral $ inventoryId)::Inventory_Id)
                                                                   , inventoryPart_CreatedDate = cd
                                                                   , inventoryPart_ModifiedDate = md
                                                                   }
