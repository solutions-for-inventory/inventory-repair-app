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
{-# LANGUAGE RecordWildCards       #-}

module Graphql.Category (
    Category
    , CategoryArg
    , CategoryFilter
    , listCategoryResolver
    , saveCategoryResolver
    , toCategoryQL
    , getCategoryByIdResolver_
) where

import Import
import Data.Morpheus.Types (GQLType, lift)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Enums (readCategoryScope)

data Category = Category { categoryId :: Int
                         , name :: Text
                         , key :: Maybe Text
                         , scope :: Text
                         , description :: Text
                         , orgUnitId :: Int
                         , createdDate :: Text
                         , modifiedDate :: Maybe Text
                         } deriving (Generic, GQLType)

-- Mutation
data CategoryArg = CategoryArg { categoryId :: Int
                               , name :: Text
                               , key :: Maybe Text
                               , scope :: Text
                               , description :: Text
                               , orgUnitId :: Int
                               } deriving (Generic)

data CategoryFilter = CategoryFilter { orgUnitId :: Int, scope :: Text } deriving (Generic)
-- DB ACTIONS

getCategoryByIdResolver_ :: (MonadTrans t) => PartCategory_Id -> () -> t Handler Category
getCategoryByIdResolver_ categoryId _ = lift $ do
                                      category <- runDB $ getJustEntity categoryId
                                      return $ toCategoryQL category

listCategoryResolver :: (MonadTrans t) => CategoryFilter -> t Handler [Category]
listCategoryResolver (CategoryFilter {..}) = lift $ do
                      categories <- runDB $ selectList [PartCategory_OrgUnitId ==. (toSqlKey $ fromIntegral $ orgUnitId), PartCategory_Scope ==. (readCategoryScope scope)] []
                      return $ P.map toCategoryQL categories

saveCategoryResolver :: (MonadTrans t) => CategoryArg -> t Handler Category
saveCategoryResolver arg = lift $ do
                        categoryId <- createOrUpdateCategory arg
                        category <- runDB $ getJustEntity categoryId
                        return $ toCategoryQL category

createOrUpdateCategory :: CategoryArg -> Handler PartCategory_Id
createOrUpdateCategory category = do
                let CategoryArg {..} = category
                now <- liftIO getCurrentTime
                entityId <- if categoryId > 0 then
                                do
                                  let categoryKey = (toSqlKey $ fromIntegral $ categoryId)::PartCategory_Id
                                  _ <- runDB $ update categoryKey [ PartCategory_Name =. name
                                                                  , PartCategory_Description =. description
                                                                  , PartCategory_Key =. key
                                                                  , PartCategory_Scope =. (readCategoryScope scope)
                                                                  , PartCategory_ModifiedDate =. Just now
                                                                  ]
                                  return categoryKey
                               else do
                                  categoryKey <- runDB $ insert $ fromCategoryQL category now Nothing
                                  return categoryKey
                return entityId

-- CONVERTERS
toCategoryQL :: Entity PartCategory_ -> Category
toCategoryQL (Entity categoryId category) = Category { categoryId = fromIntegral $ fromSqlKey categoryId
                                                     , name = category_Name
                                                     , key = category_Key
                                                     , description = category_Description
                                                     , scope = T.pack $ show category_Scope
                                                     , createdDate = fromString $ show category_CreatedDate
                                                     , modifiedDate = m
                                                     }
                                          where
                                            PartCategory_ {..} = category
                                            m = case category_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

fromCategoryQL :: CategoryArg -> UTCTime -> Maybe UTCTime -> PartCategory_
fromCategoryQL (CategoryArg {..}) cd md = PartCategory_ { category_Name = name
                                                    , category_Key = key
                                                    , category_Description = description
                                                    , category_Scope = (readCategoryScope scope)
                                                    , category_OrgUnitId = (toSqlKey $ fromIntegral $ orgUnitId)
                                                    , category_CreatedDate = cd
                                                    , category_ModifiedDate = md
                                                    }

{-
query {
  categories(queryString: "") {
    categoryId
    name
    description
  }
}

mutation {
  saveCategory(categoryId: 0, name: "test", description: "sss") {
    categoryId
    name
  }
}
-}
