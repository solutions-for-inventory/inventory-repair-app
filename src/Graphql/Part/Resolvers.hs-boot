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

module Graphql.Part.Resolvers (
    getItemByIdResolver_
    , toItemQL
) where

import Import
import Graphql.DataTypes

getItemByIdResolver_ ::
                   forall {site} {t :: (* -> *) -> * -> *} {t1 :: (* -> *) -> * -> *}.
                   (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
                    YesodPersist site, PersistStoreRead (YesodPersistBackend site),
                    MonadTrans t, MonadTrans t1) =>
                   Key Part_ -> t (HandlerFor site) (Part (t1 (HandlerFor App)))
toItemQL ::
                   forall {t :: (* -> *) -> * -> *}.
                   MonadTrans t =>
                   Entity Part_ -> Part (t (HandlerFor App))
