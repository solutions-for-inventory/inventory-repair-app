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

module Graphql.Part.Persistence (
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
import Graphql.Unit ()
import Graphql.DataTypes
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
                                     | T.strip field == "partNumber" = [getOperator operator (item ^. Part_PartNumber) (E.val $ T.strip value)]
                                     | T.strip field == "status" = [getOperator operator (item ^. Part_Status) (E.val (readEntityStatus $ T.strip value))]
--                                     | T.strip field == "itemType" = [getOperator operator (item ^. Part_ItemType) (E.val (readItemType $ T.strip value))]
--                                     | T.strip field == "partNumber" = [getOperator operator (item ^. Part_PartNumber) (E.val $ Just $ T.strip value)]
                                     | T.strip field == "partCategoryId" = [getOperator operator (item ^. Part_PartCategoryId) (E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                     | T.strip field == "partId" = [getOperator operator (item ^. Part_Id) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                     | otherwise = []

getItemInPredicate :: E.SqlExpr (Entity Part_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getItemInPredicate item Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                       | T.strip field == "name" = [(item ^. Part_Name) `in_` (E.valList $ fromText P.id value)]
                                       | T.strip field == "partNumber" = [(item ^. Part_PartNumber) `in_` (E.valList $ fromText P.id value)]
                                       | T.strip field == "status" = [(item ^. Part_Status) `in_` (E.valList $ fromText readEntityStatus value)]
--                                       | T.strip field == "itemType" = [(item ^. Part_ItemType) `in_` (E.valList $ fromText readItemType value)]
--                                       | T.strip field == "partNumber" = [(item ^. Part_PartNumber) `in_` (E.valList $ fromText (\e -> Just e) value)]
                                       | T.strip field == "partCategoryId" = [(item ^. Part_PartCategoryId) `in_` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                       | otherwise = []

getItemNotInPredicate :: E.SqlExpr (Entity Part_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getItemNotInPredicate item Predicate {..} | T.strip operator /= "notIn" || T.strip value == "" = []
                                      | T.strip field == "name" = [(item ^. Part_Name) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "partNumber" = [(item ^. Part_PartNumber) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "status" = [(item ^. Part_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
--                                      | T.strip field == "itemType" = [(item ^. Part_ItemType) `notIn` (E.valList $ fromText readItemType value)]
--                                      | T.strip field == "partNumber" = [(item ^. Part_PartNumber) `notIn` (E.valList $ fromText (\e -> Just e) value)]
                                      | T.strip field == "partCategoryId" = [(item ^. Part_PartCategoryId) `notIn` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
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
                                                  Just s -> [item ^. Part_PartNumber E.==. E.val s, item ^. Part_Name `E.like` (%) ++. E.val s ++. (%)]
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
                                              return (inventoryItem ^. InventoryPart_PartId)
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
                                              return (inventoryItem ^. InventoryPart_PartId)
                                        filters <- itemFilters item page
                                        E.where_ (item ^. Part_Id `E.notIn` E.subList_select subquery E.&&. filters)
                                        return item
                      return result
-- END QUERIES

changeStatus :: [Int] -> EntityStatus -> Handler ()
changeStatus [] _ = pure ()
changeStatus (x:xs) status = do
                        let partId = (toSqlKey $ fromIntegral $ x)::Part_Id
                        now <- liftIO getCurrentTime
                        _ <- runDB $ update partId [ Part_Status =. status, Part_ModifiedDate =. Just now]
                        _ <- changeStatus xs status
                        return ()

createOrUpdateItem :: PartArg -> Handler Part_Id
createOrUpdateItem item = do
                            let PartArg {..} = item
                            now <- liftIO getCurrentTime
                            itemEntityId <- if partId > 0 then
                                        do
                                         let itemKey = (toSqlKey $ fromIntegral $ partId)::Part_Id
                                         _ <- runDB $ update itemKey [ Part_PartNumber =. partNumber
                                                                     , Part_Name =. name
                                                                     , Part_DefaultPrice =. realToFrac defaultPrice
                                                                     , Part_Description =. description
                                                                     , Part_Manufacturer =. manufacturer
                                                                     , Part_Model =. model
                                                                     , Part_Notes =. notes
                                                                     , Part_Status =. readEntityStatus status
                                                                     , Part_Images =. images
                                                                     , Part_PartCategoryId =. case categoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::PartCategory_Id)
                                                                     , Part_UnitId =. case unitId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Unit_Id)
                                                                     , Part_ModifiedDate =. Just now
                                                                     ]
                                         return itemKey
                                      else do
                                            itemKey <- runDB $ insert $ fromItemQL item now Nothing
                                            return itemKey
                            return itemEntityId

fromItemQL :: PartArg -> UTCTime -> Maybe UTCTime -> Part_
fromItemQL (PartArg {..}) cd md = Part_ { part_PartNumber = partNumber
                                        , part_Name = name
                                        , part_DefaultPrice = realToFrac defaultPrice
                                        , part_Description = description
                                        , part_Manufacturer = manufacturer
                                        , part_Model = model
                                        , part_Notes = notes
                                        , part_Status = readEntityStatus status
                                        , part_Images = images
                                        , part_ParentId = Nothing
                                        , part_PartCategoryId = case categoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::PartCategory_Id)
                                        , part_UnitId = case unitId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Unit_Id)
                                        , part_OrgUnitId = (toSqlKey $ fromIntegral $ orgUnitId)
                                        , part_CreatedDate = cd
                                        , part_ModifiedDate = md
                                        }

