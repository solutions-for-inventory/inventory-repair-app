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

module Graphql.Inventory.Resolvers (
      inventoryResolver
    , getInventoryByIdResolver_
    , saveInventoryResolver
    , toInventoryQL
--    , dbFetchInventoryById
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Graphql.Utils
import Graphql.DataTypes
import Graphql.Inventory.Persistence
import Graphql.InventoryPart.Resolvers
import Graphql.Part.Resolvers
import Graphql.Inventory.Persistence ()

inventoryResolver :: (Applicative f, Typeable o,MonadTrans (o ())) => p -> f (Inventories o)
inventoryResolver _ = pure Inventories { inventory = getInventoryByIdResolver
                                       , list = listInventoryResolver
                                       , saveInventory = saveInventoryResolver
                                       , saveInventoryItems = saveInventoryItemsResolver
                                       }

getInventoryByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (Inventory o)
getInventoryByIdResolver EntityIdArg {..} = lift $ do
                                              let inventoryId = (toSqlKey $ fromIntegral $ entityId)::Inventory_Id
                                              inventory <- runDB $ getJustEntity inventoryId
                                              return $ toInventoryQL inventory

getInventoryByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Inventory_Id -> () -> o () Handler (Inventory o)
getInventoryByIdResolver_ inventoryId _ = lift $ do
                                    inventory <- runDB $ getJustEntity inventoryId
                                    return $ toInventoryQL inventory

listInventoryResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => () -> o () Handler [Inventory o]
listInventoryResolver _ = lift $ do
                            inventories <- runDB $ selectList ([] :: [Filter Inventory_]) []
                            return $ P.map toInventoryQL inventories

saveInventoryResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => InventoryArg -> o () Handler (Inventory o)
saveInventoryResolver arg = lift $ do
                                  inventoryId <- createOrUpdateInventory arg
                                  inventory <- runDB $ getJustEntity inventoryId
                                  return $ toInventoryQL inventory

-- CONVERTERS
toInventoryQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Inventory_ -> Inventory o
toInventoryQL (Entity inventoryId inventory) = Inventory { inventoryId = fromIntegral $ fromSqlKey inventoryId
                                                         , name = inventory_Name
                                                         , description = inventory_Description
                                                         , allowNegativeStocks = inventory_AllowNegativeStocks
                                                         , status = T.pack $ show inventory_Status
                                                         , inventoryItems = inventoryItemsPageResolver_ inventoryId
                                                         , availableItems = availableItemsPageResolver_ inventoryId
                                                         , createdDate = fromString $ show inventory_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            Inventory_ {..} = inventory
                                            m = case inventory_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing
