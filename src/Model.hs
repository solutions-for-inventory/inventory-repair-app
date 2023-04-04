{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Enums
import Data.Kind (Type)
import Database.Persist.Class (ToBackendKey)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

--share [mkPersist sqlSettings]
--    $(persistFileWith lowerCaseSettings "config/models")


instance ToBackendKey SqlBackend Unit_ where
  toBackendKey = unUnit_Key
  fromBackendKey = Unit_Key

instance ToBackendKey SqlBackend Person_ where
  toBackendKey = unPerson_Key
  fromBackendKey = Person_Key

instance ToBackendKey SqlBackend ContactInfo_ where
  toBackendKey = unContactInfo_Key
  fromBackendKey = ContactInfo_Key

instance ToBackendKey SqlBackend Address_ where
  toBackendKey = unAddress_Key
  fromBackendKey = Address_Key

instance ToBackendKey SqlBackend Role_ where
  toBackendKey = unRole_Key
  fromBackendKey = Role_Key

instance ToBackendKey SqlBackend Privilege_ where
  toBackendKey = unPrivilege_Key
  fromBackendKey = Privilege_Key

instance ToBackendKey SqlBackend PartCategory_ where
  toBackendKey = unPartCategory_Key

instance ToBackendKey SqlBackend OrgUnit_ where
  toBackendKey = unOrgUnit_Key

instance ToBackendKey SqlBackend Part_ where
  toBackendKey = unPart_Key

instance ToBackendKey SqlBackend Inventory_ where
  toBackendKey = unInventory_Key

instance ToBackendKey SqlBackend InventoryPart_ where
  toBackendKey = unInventoryPart_Key
