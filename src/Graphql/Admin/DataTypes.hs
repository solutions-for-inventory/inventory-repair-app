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

module Graphql.Admin.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Types (GQLType(..))
import Graphql.Utils (Page, PageArg, EntityIdArg, EntityChangeStatusArg)
import Graphql.PartCategory
import Graphql.Admin.Privilege
import Graphql.Admin.Role


-- PERSON DATA TYPES
data Person m = Person { personId :: Int
                       , firstName :: Text
                       , lastName :: Text
                       , documentType :: Text
                       , documentId :: Text
                       , createdDate :: Text
                       , modifiedDate :: Maybe Text
                       , address :: m (Maybe Address)
                       , contactInfo :: m [ContactInfo]
--                       , user :: Maybe (() -> o () Handler (User o))
                       } deriving (Generic, GQLType)

data Persons m = Persons { person :: EntityIdArg -> m (Person m)
                         , page :: PageArg -> m (Page (Person m))
                         , createUpdatePerson :: PersonArg -> m (Person m)
                         } deriving (Generic, GQLType)

data ContactInfo = ContactInfo { contactId :: Int
                               , contact :: Text
                               , contactType :: Text
                               , createdDate :: Text
                               , modifiedDate :: Maybe Text
                               } deriving (Generic, GQLType)

data Address = Address {  addressId :: Int
                        , street1 :: Text
                        , street2 :: Text
                        , street3 :: Text
                        , zip :: Text
                        , city :: Text
                        , state :: Text
                        , country :: Text
                        , createdDate :: Text
                        , modifiedDate :: Maybe Text
                       } deriving (Generic, GQLType)

-- Person Graphql Arguments
--data PersonAddressArg = PersonAddressArg {address :: Maybe AddressArg} deriving (Generic, GQLType)
--data PersonContactInfoArg = PersonContactInfoArg {contactInfo :: Maybe [ContactInfoArg]} deriving (Generic, GQLType)
--data PersonUserArg = PersonUserArg {user :: Maybe UserArg} deriving (Generic, GQLType)
data PersonArg = PersonArg { personId :: Int
                           , firstName :: Text
                           , lastName :: Text
                           , documentType :: Text
                           , documentId :: Text
                           , address :: Maybe AddressArg
                           , contactInfo :: [ContactInfoArg]
                           , orgUnitId :: Int
--                           , user :: Maybe UserArg
                           } deriving (Generic, GQLType)

data AddressArg = AddressArg { addressId :: Int
                             , street1 :: Text
                             , street2 :: Text
                             , street3 :: Text
                             , zip :: Text
                             , city :: Text
                             , state :: Text
                             , country :: Text
                             } deriving (Generic, GQLType)

data ContactInfoArg = ContactInfoArg { contactId :: Int
                                     , contact :: Text
                                     , contactType :: Text
                                     } deriving (Generic, GQLType)

-- USER DATA TYPES
data User m = User { userId :: Int
                   , username :: Text
                   , email :: Text
                   , status :: Text
                   , locale :: Text
                   , expiration :: Bool
                   , newPasswordRequired :: Bool
                   , person :: m (Person m)
                   , roles :: m [Role m]
                   , privileges :: m [Privilege]
                   , createdDate :: Text
                   , modifiedDate :: Maybe Text
                   } deriving (Generic, GQLType)

data Users m = Users { user :: EntityIdArg -> m (User m)
                     , page :: PageArg -> m (Page (User m))
                     , createUpdateUser :: UserArg -> m (User m)
                     , resetPassword :: EntityIdArg -> m Text
                     , changePassword :: ChangePasswordArg -> m Bool
                     , updatePassword :: UpdatePasswordArg -> m Bool
                     } deriving (Generic, GQLType)

data ChangePasswordArg = ChangePasswordArg { userId:: Int, password :: Text, newPassword :: Text} deriving (Generic, GQLType)
data UpdatePasswordArg = UpdatePasswordArg { userId:: Int, password :: Text } deriving (Generic, GQLType)

-- User Graphql Arguments
data UserArg = UserArg { userId :: Int
                       , username :: Text
                       , email :: Text
                       , status :: Text
                       , locale :: Text
                       , expiration :: Bool
                       , person :: PersonArg
                       , orgUnitId :: Int
                       , roleIds :: [Int]
                       , privilegeIds :: [Int]
                       } deriving (Generic, GQLType)
