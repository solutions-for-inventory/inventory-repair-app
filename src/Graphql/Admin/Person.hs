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

module Graphql.Admin.Person (
                  personResolver
                , createUpdatePersonResolver
                , createOrUpdatePerson
                , createOrUpdateAddress
                , createOrUpdateContactInfo
                , addressResolver
                , contactInfoResolver
                , getPersonByIdResolver_
                ) where

import Import hiding (zip)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (%), (++.){-, (?.), notIn, in_-})
--import qualified Data.Text as T
--import qualified Data.ByteString.Char8 as B
import Prelude as P hiding (zip)
import Graphql.Utils
import Graphql.Admin.DataTypes

-- Query Resolvers
personResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (Persons o)
personResolver _ = pure Persons { person = getPersonByIdResolver
                                , page = pagePersonResolver
                                , createUpdatePerson = createUpdatePersonResolver
                                }

getPersonByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (Person o)
getPersonByIdResolver EntityIdArg {..} = lift $ do
                                      let personEntityId = (toSqlKey $ fromIntegral $ entityId)::Person_Id
                                      person <- runDB $ getJustEntity personEntityId
                                      return $ toPersonQL person

getPersonByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Person_Id -> () -> o () Handler (Person o)
getPersonByIdResolver_ personId _ = lift $ do
                                      person <- runDB $ getJustEntity personId
                                      return $ toPersonQL person

addressResolver :: (MonadTrans t) => Person_Id -> () -> t Handler (Maybe Address)
addressResolver personId _ = lift $ do
                    addressMaybe <- runDB $ selectFirst [Address_PersonId ==. personId] []
                    let address = case addressMaybe of
                                    Nothing -> Nothing
                                    Just a -> Just $ toAddressQL a
                    return address

contactInfoResolver :: (MonadTrans t) => Person_Id -> () -> t Handler [ContactInfo]
contactInfoResolver personId _ = lift $ do
                                      contacts <- runDB $ selectList [ContactInfo_PersonId ==. personId] []
                                      return $ P.map toContactQL contacts

pagePersonResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (Person o))
pagePersonResolver page = lift $ do
                                countItems <- personQueryCount page
                                persons <- personQuery page
                                let personsQL = P.map (\p -> toPersonQL p) persons
                                return Page { totalCount = countItems
                                            , content = personsQL
                                            , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                                  , hasPreview = pageIndex' * pageSize' > 0
                                                                  , pageSize = pageSize'
                                                                  , pageIndex = pageIndex'
                                            }
                                }
                         where
                          PageArg {..} = page
                          pageIndex' = case pageIndex of Just  x  -> x; Nothing -> 0
                          pageSize' = case pageSize of Just y -> y; Nothing -> 10

personFilters :: Monad m => E.SqlExpr (Entity Person_) -> PageArg -> m (E.SqlExpr (E.Value Bool))
personFilters person PageArg {..} = do
                            let searchFilters = case searchString of
                                                  Just s -> [ person ^. Person_DocumentId E.==. E.val s
                                                            , person ^. Person_FirstName `E.ilike` (%) ++. E.val s ++. (%)
                                                            , person ^. Person_LastName `E.ilike` (%) ++. E.val s ++. (%)
                                                            ]
                                                  Nothing -> [person ^. Person_Id E.==. person ^. Person_Id]
                            let searchFilters' = unionFilters searchFilters
                            return searchFilters'

personQueryCount :: PageArg -> Handler Int
personQueryCount page =  do
                      result  <- runDB
                                   $ E.select
                                   $ E.from $ \ person -> do
                                        filters <- personFilters person page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ result

personQuery :: PageArg -> Handler [Entity Person_]
personQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ person -> do
                                        pFilters <- personFilters person page
                                        E.where_ pFilters
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return person
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

createUpdatePersonResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PersonArg -> t Handler (Person o)
createUpdatePersonResolver arg = lift $ do
                                personId <- createOrUpdatePerson arg
                                person <- runDB $ getJustEntity personId
                                return $ toPersonQL person

createOrUpdatePerson :: PersonArg -> Handler Person_Id
createOrUpdatePerson personArg = do
                               let PersonArg{..} = personArg
                               now <- liftIO getCurrentTime
                               personEntityId <- if personId > 0 then
                                            do
                                              let personKey = (toSqlKey $ fromIntegral personId)::Person_Id
                                              _ <- runDB $ update personKey [  Person_FirstName =. firstName
                                                                             , Person_LastName =. lastName
                                                                             , Person_DocumentType =. documentType
                                                                             , Person_DocumentId =. documentId
                                                                             , Person_ModifiedDate =. Just now
                                                                            ]
                                              return personKey
                                            else
                                              do
                                                personKey <- runDB $ insert (fromPersonQL_ personArg now Nothing)
                                                return personKey
                               _ <- case address of
                                      Nothing -> return ()
                                      Just addressArg ->  do
                                                          _ <- createOrUpdateAddress personEntityId addressArg
                                                          return ()
                               _ <- createOrUpdateContactInfo personEntityId contactInfo
                               return personEntityId

createOrUpdateAddress :: Person_Id -> AddressArg -> Handler Address_Id
createOrUpdateAddress personId address = do
                               let AddressArg {..} = address
                               now <- liftIO getCurrentTime
                               addressEntityId <- if addressId > 0 then
                                                   do
                                                     let addressId' = (toSqlKey $ fromIntegral addressId)::Address_Id
                                                     _ <- runDB $ update addressId' [  Address_Street1 =. street1
                                                                                     , Address_Street2 =. street2
                                                                                     , Address_Street3 =. street3
                                                                                     , Address_Zip =. zip
                                                                                     , Address_City =. city
                                                                                     , Address_State =. state
                                                                                     , Address_Country =. country
                                                                                     , Address_ModifiedDate =. Just now
                                                                                    ]
                                                     return addressId'
                                                  else
                                                   do
                                                     addressId' <- runDB $ insert (fromAddressQL_ personId address now Nothing)
                                                     return addressId'
                               return addressEntityId

createOrUpdateContactInfo :: Person_Id -> [ContactInfoArg] -> Handler ()
createOrUpdateContactInfo personId  contactInfo = do
                               now <- liftIO getCurrentTime
                               let c1 = P.filter (\ContactInfoArg {..} -> contactId <= 0)  $ contactInfo
                               let c2 = P.filter (\ContactInfoArg {..} -> contactId > 0)  $ contactInfo
                               _ <- runDB $ insertMany  $ [fromContactQL_ personId c now Nothing | c <- c1]
                               _ <- updateContact_ c2
                               return ()

updateContact_ :: [ContactInfoArg] -> Handler ()
updateContact_ [] = return ()
updateContact_ (x:xs)= do
                        let ContactInfoArg {..} = x
                        now <- liftIO getCurrentTime
                        let entityContactId = (toSqlKey $ fromIntegral contactId)::ContactInfo_Id
                        _ <- runDB $ update entityContactId [ ContactInfo_ContactType =. contactType
                                                            , ContactInfo_Contact =. contact
                                                            , ContactInfo_ModifiedDate =. Just now
                                                            ]
                        _ <- updateContact_ xs
                        return ()

--toPersonQL :: Entity Person_ -> (Person Res)
toPersonQL :: (Typeable o, MonadTrans (o ())) => Entity Person_ -> Person o
toPersonQL (Entity personId person) = Person { personId = fromIntegral $ fromSqlKey personId
                                             , firstName = person_FirstName
                                             , lastName = person_LastName
                                             , documentType = person_DocumentType
                                             , documentId = person_DocumentId
                                             , createdDate = fromString $ show person_CreatedDate
                                             , modifiedDate = md
                                             , address = addressResolver personId
                                             , contactInfo = contactInfoResolver personId
                                             }
                                 where
                                  Person_ {..} = person
                                  md = case person_ModifiedDate of
                                        Just d -> Just $ fromString $ show d
                                        Nothing -> Nothing

toContactQL :: Entity ContactInfo_ -> ContactInfo
toContactQL (Entity contactId contact) = ContactInfo { contactId = fromIntegral $ fromSqlKey contactId
                                                     , contact = contactInfo_Contact
                                                     , contactType = contactInfo_ContactType
                                                     , createdDate = fromString $ show contactInfo_CreatedDate
                                                     , modifiedDate = md
                                                     }
                                    where
                                      ContactInfo_ {..} = contact
                                      md = case contactInfo_ModifiedDate of
                                            Just d -> Just $ fromString $ show d
                                            Nothing -> Nothing

toAddressQL :: Entity Address_ -> Address
toAddressQL (Entity addressId address) = Address { addressId = fromIntegral $ fromSqlKey addressId
                                                 , street1 = address_Street1
                                                 , street2 = address_Street2
                                                 , street3 = address_Street3
                                                 , zip = address_Zip
                                                 , city = address_City
                                                 , state = address_State
                                                 , country = address_Country
                                                 , createdDate = fromString $ show address_CreatedDate
                                                 , modifiedDate = md
                                                 }
                                             where
                                                Address_ {..} = address
                                                md = case address_ModifiedDate of
                                                      Just d -> Just $ fromString $ show d
                                                      Nothing -> Nothing

fromPersonQL_ :: PersonArg -> UTCTime -> Maybe UTCTime -> Person_
fromPersonQL_ PersonArg {..} cd md = Person_ { person_FirstName = firstName
                                             , person_LastName = lastName
                                             , person_DocumentType = documentType
                                             , person_DocumentId = documentId
                                             , person_OrgUnitId = (toSqlKey $ fromIntegral $ orgUnitId)
                                             , person_CreatedDate = cd
                                             , person_ModifiedDate =  md
                                             }

fromAddressQL_ :: Person_Id -> AddressArg -> UTCTime -> Maybe UTCTime -> Address_
fromAddressQL_ personId AddressArg {..} cd md = Address_ street1 street2 street3 zip city state country personId cd md

fromContactQL_ :: Person_Id -> ContactInfoArg -> UTCTime -> Maybe UTCTime -> ContactInfo_
fromContactQL_ personId ContactInfoArg {..} cd md = ContactInfo_ contactType contact personId cd md


{-

query {
  persons {
    person(entityId: 16) {
    personId
    firstName
    lastName
    createdDate
    modifiedDate
    address {
      addressId
      city
      country
      state
    }

    contactInfo {
      contactId
      contact
      contactType
    }
    }
  }
}


mutation {
  savePerson(personId:0, firstName: "test", lastName: "sss", documentType: "sss", documentId: "0") {
    personId
    firstName
    lastName
    createdDate
    modifiedDate
    address(addressId: 0, street1: "street1", street2: "street2", street3: "street1", zip:"ss", city: "OR", state: "s", country:"ssss") {
      addressId
      city
      country
      state
    }

    contactInfo(contactInfo: [{contactId: 0, contact: "mss", contactType: "mail"}]) {
      contactId
      contact
      contactType
    }
  }
}
-}
