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

module Graphql.PartCategory (
    PartCategory
    , PartCategoryArg
    , PartCategoryFilter
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

data PartCategory = PartCategory { categoryId :: Int
                         , name :: Text
                         , key :: Maybe Text
                         , scope :: Text
                         , description :: Text
                         , orgUnitId :: Int
                         , createdDate :: Text
                         , modifiedDate :: Maybe Text
                         } deriving (Generic, GQLType)

-- Mutation
data PartCategoryArg = PartCategoryArg { categoryId :: Int
                                       , name :: Text
                                       , key :: Maybe Text
                                       , scope :: Text
                                       , description :: Text
                                       , orgUnitId :: Int
                                       } deriving (Generic, GQLType)

data PartCategoryFilter = PartCategoryFilter { orgUnitId :: Int, scope :: Text } deriving (Generic, GQLType)
-- DB ACTIONS

getCategoryByIdResolver_ :: (MonadTrans t) => PartCategory_Id -> () -> t Handler PartCategory
getCategoryByIdResolver_ categoryId _ = lift $ do
                                      category <- runDB $ getJustEntity categoryId
                                      return $ toCategoryQL category

listCategoryResolver :: (MonadTrans t) => PartCategoryFilter -> t Handler [PartCategory]
listCategoryResolver (PartCategoryFilter {..}) = lift $ do
                      categories <- runDB $ selectList [] []
                      return $ P.map toCategoryQL categories

saveCategoryResolver :: (MonadTrans t) => PartCategoryArg -> t Handler PartCategory
saveCategoryResolver arg = lift $ do
                        categoryId <- createOrUpdateCategory arg
                        category <- runDB $ getJustEntity categoryId
                        return $ toCategoryQL category

createOrUpdateCategory :: PartCategoryArg -> Handler PartCategory_Id
createOrUpdateCategory category = do
                let PartCategoryArg {..} = category
                now <- liftIO getCurrentTime
                entityId <- if categoryId > 0 then
                                do
                                  let categoryKey = (toSqlKey $ fromIntegral $ categoryId)::PartCategory_Id
                                  _ <- runDB $ update categoryKey [ PartCategory_Name =. name
                                                                  , PartCategory_Description =. description
                                                                  , PartCategory_Key =. key
                                                                  , PartCategory_ModifiedDate =. Just now
                                                                  ]
                                  return categoryKey
                               else do
                                  categoryKey <- runDB $ insert $ fromCategoryQL category now Nothing
                                  return categoryKey
                return entityId

-- CONVERTERS
toCategoryQL :: Entity PartCategory_ -> PartCategory
toCategoryQL (Entity categoryId category) = PartCategory { categoryId = fromIntegral $ fromSqlKey categoryId
                                                     , name = partCategory_Name
                                                     , key = partCategory_Key
                                                     , description = partCategory_Description
                                                     , createdDate = fromString $ show partCategory_CreatedDate
                                                     , modifiedDate = m
                                                     }
                                          where
                                            PartCategory_ {..} = category
                                            m = case partCategory_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

fromCategoryQL :: PartCategoryArg -> UTCTime -> Maybe UTCTime -> PartCategory_
fromCategoryQL (PartCategoryArg {..}) cd md = PartCategory_ { partCategory_Name = name
                                                        , partCategory_Key = key
                                                        , partCategory_ParentId = Nothing
                                                        , partCategory_Description = description
                                                        , partCategory_CreatedDate = cd
                                                        , partCategory_ModifiedDate = md
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
