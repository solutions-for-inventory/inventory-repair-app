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

module Graphql.Asset.Item.Persistence (
      itemQueryCount
    , itemQuery
    , availableItemsQueryCount
    , availableItemsQuery
    , changeStatus
    , createOrUpdateItem
    , itemFilters
) where

import Import hiding (union)
import Database.Persist.Sql (toSqlKey)
import qualified Data.Text as T
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (%), (++.), notIn, in_)
import Prelude as P
import Graphql.Utils
import Enums
import Graphql.Asset.Unit ()
import Graphql.Asset.DataTypes
--getFilters Nothing = []
--getFilters (Just []) = []
--getFilters (Just (x:xs)) | T.strip field == "" || T.strip operator == "" || T.strip value == ""  = getFilters $ Just xs
--                         | T.strip field == "status" = ((getOperator operator) Part_Status (readEntityStatus $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "itemType" = ((getOperator operator) Part_ItemType (readItemType $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "name" = ((getOperator operator) Part_Name (T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "partNumber" = ((getOperator operator) Part_PartNumber (Just $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "manufacturer" = ((getOperator operator) Part_Manufacturer (Just $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "model" = ((getOperator operator) Part_Model (Just $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "categoryId" = ((getOperator operator) Part_CategoryId (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value)) : (getFilters $ Just xs)
--                         | otherwise = getFilters $ Just xs
--                   where
--                      Predicate {..} = x

--getItemPredicate Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
--                            | T.strip field == "status" = [((getOperator operator) Part_Status (readEntityStatus $ T.strip value))]
--                            | T.strip field == "itemType" = [((getOperator operator) Part_ItemType (readItemType $ T.strip value))]
--                            | T.strip field == "name" = [((getOperator operator) Part_Name (T.strip value))]
--                            | T.strip field == "partNumber" = [((getOperator operator) Part_PartNumber (Just $ T.strip value))]
--                            | T.strip field == "manufacturer" = [((getOperator operator) Part_Manufacturer (Just $ T.strip value))]
--                            | T.strip field == "model" = [((getOperator operator) Part_Model (Just $ T.strip value))]
--                            | T.strip field == "categoryId" = [((getOperator operator) Part_CategoryId (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
--                            | otherwise = []
--
--getItemInPredicate Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
--                              | T.strip field == "status" = [Part_Status <-. textToList value readEntityStatus]
--                              | T.strip field == "itemType" = [Part_ItemType <-. textToList value readItemType]
--                              | T.strip field == "name" = [Part_Name <-. textToList value P.id]
--                              | T.strip field == "partNumber" = [Part_PartNumber <-. textToList value Just]
--                              | T.strip field == "manufacturer" = [Part_Manufacturer <-. textToList value Just]
--                              | T.strip field == "model" = [Part_Model <-. textToList value Just]
--                              | T.strip field == "categoryId" = [Part_CategoryId <-. textToList value (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e)]
--                              | otherwise = []

getItemPredicate :: E.SqlExpr (Entity Part_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getItemPredicate item Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                     | T.strip field == "name" = [getOperator operator (item ^. Part_Name) (E.val value)]
                                     | T.strip field == "code" = [getOperator operator (item ^. Part_Code) (E.val $ T.strip value)]
                                     | T.strip field == "status" = [getOperator operator (item ^. Part_Status) (E.val (readEntityStatus $ T.strip value))]
                                     | T.strip field == "itemType" = [getOperator operator (item ^. Part_ItemType) (E.val (readItemType $ T.strip value))]
                                     | T.strip field == "partNumber" = [getOperator operator (item ^. Part_PartNumber) (E.val $ Just $ T.strip value)]
                                     | T.strip field == "categoryId" = [getOperator operator (item ^. Part_CategoryId) (E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                     | T.strip field == "itemId" = [getOperator operator (item ^. Part_Id) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                     | otherwise = []

getItemInPredicate :: E.SqlExpr (Entity Part_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getItemInPredicate item Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                       | T.strip field == "name" = [(item ^. Part_Name) `in_` (E.valList $ fromText P.id value)]
                                       | T.strip field == "code" = [(item ^. Part_Code) `in_` (E.valList $ fromText P.id value)]
                                       | T.strip field == "status" = [(item ^. Part_Status) `in_` (E.valList $ fromText readEntityStatus value)]
                                       | T.strip field == "itemType" = [(item ^. Part_ItemType) `in_` (E.valList $ fromText readItemType value)]
                                       | T.strip field == "partNumber" = [(item ^. Part_PartNumber) `in_` (E.valList $ fromText (\e -> Just e) value)]
                                       | T.strip field == "categoryId" = [(item ^. Part_CategoryId) `in_` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                       | otherwise = []

getItemNotInPredicate :: E.SqlExpr (Entity Part_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getItemNotInPredicate item Predicate {..} | T.strip operator /= "notIn" || T.strip value == "" = []
                                      | T.strip field == "name" = [(item ^. Part_Name) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "code" = [(item ^. Part_Code) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "status" = [(item ^. Part_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
                                      | T.strip field == "itemType" = [(item ^. Part_ItemType) `notIn` (E.valList $ fromText readItemType value)]
                                      | T.strip field == "partNumber" = [(item ^. Part_PartNumber) `notIn` (E.valList $ fromText (\e -> Just e) value)]
                                      | T.strip field == "categoryId" = [(item ^. Part_CategoryId) `notIn` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                      | otherwise = []

getItemPredicates :: E.SqlExpr (Entity Part_) -> [Predicate] -> [[E.SqlExpr (E.Value Bool)]]
getItemPredicates _ [] = []
getItemPredicates item (x:xs) | P.length p == 0 = getItemPredicates item xs
                          | otherwise = p : getItemPredicates item xs
                   where
                      p = (getItemPredicate item x) P.++ (getItemInPredicate item x) P.++ (getItemNotInPredicate item x)

itemFilters :: Monad m => E.SqlExpr (Entity Part_) -> PageArg -> m (E.SqlExpr (E.Value Bool))
itemFilters item PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ getItemPredicates item justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (item ^. Part_Id E.==. item ^. Part_Id)
                            let searchFilters = case searchString of
                                                  Just s -> [item ^. Part_Code E.==. E.val s, item ^. Part_Name `E.like` (%) ++. E.val s ++. (%)]
                                                  Nothing -> [item ^. Part_Id E.==. item ^. Part_Id]
                            let searchFilters_ = unionFilters searchFilters
                            return (searchFilters_ E.&&. predicates_)

-- QUERIES
itemQueryCount :: PageArg -> Handler Int
itemQueryCount page =  do
                      result  <- runDB
                                   $ E.select
                                   $ E.from $ \ item -> do
                                        filters <- itemFilters item page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ result

itemQuery :: PageArg -> Handler [Entity Part_]
itemQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ item -> do
                                        iFilters <- itemFilters item page
                                        E.where_ iFilters
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return item
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

availableItemsQueryCount :: Inventory_Id -> PageArg -> Handler Int
availableItemsQueryCount inventoryId page = do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \item -> do
                                        let subquery =
                                              E.from $ \inventoryItem -> do
                                              E.where_ (inventoryItem ^. InventoryPart_InventoryId E.==. E.val inventoryId)
                                              return (inventoryItem ^. InventoryPart_ItemId)
                                        filters <- itemFilters item page
                                        E.where_ (item ^. Part_Id `E.notIn` (E.subList_select subquery) E.&&. filters)
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ result

availableItemsQuery :: Inventory_Id -> PageArg -> Handler [Entity Part_]
availableItemsQuery inventoryId page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \item -> do
                                        let subquery =
                                              E.from $ \inventoryItem -> do
                                              E.where_ (inventoryItem ^. InventoryPart_InventoryId E.==. E.val inventoryId)
                                              return (inventoryItem ^. InventoryPart_ItemId)
                                        filters <- itemFilters item page
                                        E.where_ (item ^. Part_Id `E.notIn` E.subList_select subquery E.&&. filters)
                                        return item
                      return result
-- END QUERIES

changeStatus :: [Int] -> EntityStatus -> Handler ()
changeStatus [] _ = pure ()
changeStatus (x:xs) status = do
                        let itemId = (toSqlKey $ fromIntegral $ x)::Part_Id
                        now <- liftIO getCurrentTime
                        _ <- runDB $ update itemId [ Part_Status =. status, Part_ModifiedDate =. Just now]
                        _ <- changeStatus xs status
                        return ()

createOrUpdateItem :: ItemArg -> Handler Part_Id
createOrUpdateItem item = do
                            let ItemArg {..} = item
                            now <- liftIO getCurrentTime
                            itemEntityId <- if itemId > 0 then
                                        do
                                         let itemKey = (toSqlKey $ fromIntegral $ itemId)::Part_Id
                                         _ <- runDB $ update itemKey [ Part_Code =. code
                                                                     , Part_Name =. name
                                                                     , Part_DefaultPrice =. realToFrac defaultPrice
                                                                     , Part_Description =. description
                                                                     , Part_PartNumber =. partNumber
                                                                     , Part_Manufacturer =. manufacturer
                                                                     , Part_Model =. model
                                                                     , Part_ItemType =. readItemType itemType
                                                                     , Part_Notes =. notes
                                                                     , Part_Status =. readEntityStatus status
                                                                     , Part_Images =. images
                                                                     , Part_CategoryId =. case categoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::PartCategory_Id)
                                                                     , Part_UnitId =. case unitId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Unit_Id)
                                                                     , Part_ModifiedDate =. Just now
                                                                     ]
                                         return itemKey
                                      else do
                                            itemKey <- runDB $ insert $ fromItemQL item now Nothing
                                            return itemKey
                            return itemEntityId

fromItemQL :: ItemArg -> UTCTime -> Maybe UTCTime -> Part_
fromItemQL (ItemArg {..}) cd md = Part_ { item_Code = code
                                        , item_Name = name
                                        , item_DefaultPrice = realToFrac defaultPrice
                                        , item_Description = description
                                        , item_PartNumber = partNumber
                                        , item_Manufacturer = manufacturer
                                        , item_Model = model
                                        , item_ItemType = readItemType itemType
                                        , item_Notes = notes
                                        , item_Status = readEntityStatus status
                                        , item_Images = images
                                        , item_CategoryId = case categoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::PartCategory_Id)
                                        , item_UnitId = case unitId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Unit_Id)
                                        , item_OrgUnitId = (toSqlKey $ fromIntegral $ orgUnitId)
                                        , item_CreatedDate = cd
                                        , item_ModifiedDate = md
                                        }

