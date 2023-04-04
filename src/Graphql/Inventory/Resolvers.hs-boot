{-# LANGUAGE CPP                   #-}
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


module Graphql.Inventory.Resolvers (
    getInventoryByIdResolver_
) where

import Import
import Graphql.DataTypes

getInventoryByIdResolver_ ::
                   forall {site} {t :: (* -> *) -> * -> *} {t1 :: (* -> *) -> * -> *}.
                   (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
                    YesodPersist site, PersistStoreRead (YesodPersistBackend site),
                    MonadTrans t, MonadTrans t1) =>
                   Key Inventory_
                   -> t (HandlerFor site) (Inventory (t1 (HandlerFor App)))
--toInventoryQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Inventory_ -> Inventory o
