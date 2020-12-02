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

module Graphql.Session (Session, getUserSessionResolver, toSessionQL) where

import Import
import GHC.Generics ()
import Text.RawString.QQ
import Data.Morpheus.Types (GQLType)
import Database.Persist.Sql (rawSql, toSqlKey, Single(..))
import Prelude as P
import Data.Char (isDigit)
import qualified Data.Text as T

data Session = Session { authId :: Text
                       , username :: Text
                       , email :: Text
                       , firstName :: Text
                       , lastName :: Text
                       , locale :: Text
                       , permissions :: [Text]
                       } deriving (Generic, GQLType)

permissionsSql :: Text
permissionsSql = [r|
select case
           when r.role_id is null
               then concat('DEFAULT_ROLE.', p.name)
           else concat(upper(r.name), '.', p.name) end as permission
from (
         select distinct p.*, rp.role_id
         from t_privilege p
                  join t_role_privilege rp on (rp.privilege_id = p.privilege_id)
         where rp.role_id in (select role_id from t_user_role where user_id = ?)
         union
         select *, null as role_id
         from t_privilege
         where privilege_id in (select t_user_privilege.privilege_id from t_user_privilege where user_id = ?)
     ) p
         left join t_role r on r.role_id = p.role_id
order by r.name, p.name
|]
-- Query Resolvers
getUserSessionResolver :: (MonadTrans t) => () -> t Handler Session
getUserSessionResolver () = lift $ do
                                     muid <- maybeAuthId
--                                     () <- case muid of
--                                              Nothing -> $logWarn   "Test log"
--                                              Just a -> $logWarn   a
                                     let userId = case muid of
                                                    Nothing -> 0
                                                    Just a -> read $ P.filter  (\c -> isDigit c) (T.unpack a) :: Int
                                     let personId = (toSqlKey $ fromIntegral $ userId)::Person_Id
                                     let userKey = User_Key {unUser_Key = personId}
                                     Entity _ user <- runDB $ getJustEntity userKey
                                     let User_ {..} = user
                                     Entity _ person <- runDB $ getJustEntity personId
                                     permissions <- getPermissions personId
                                     return $ toSessionQL userId user person (P.map (\p -> unSingle p) permissions)

getPermissions :: Person_Id -> Handler [Single Text]
getPermissions userId = do
                         permissions <- runDB $ rawSql permissionsSql [toPersistValue userId, toPersistValue userId]
                         return permissions

toSessionQL :: Show a => a -> User_ -> Person_ -> [Text] -> Session
toSessionQL authId User_ {..} Person_ {..} p = Session { authId = T.pack $ show authId
                                                       , username = user_Username
                                                       , email = user_Email
                                                       , firstName = person_FirstName
                                                       , lastName = person_LastName
                                                       , locale = T.pack $ show user_Locale
                                                       , permissions = p
                                                       }
--                                where
--                                  User_ {..} = user
