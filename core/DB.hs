{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module DB
where

import           Control.Monad.Catch
import qualified Data.Text                 as T
import           Database.Selda
import           Database.Selda.Generic
import           Database.Selda.PostgreSQL
import           Database.Selda.SQLite
import           Env
import           GHC.Generics

data UserMD = UserMD
 {
    userId :: T.Text
  , name   :: T.Text
  , email  :: T.Text
 } deriving Generic

users :: GenTable UserMD
users = genTable "users" [userId :- primaryGen]

setup :: SeldaM ()
setup =
  createTable (gen users)

teardown :: SeldaM ()
teardown =
  tryDropTable (gen users)

type DatabaseName = T.Text
type Host = T.Text
type UserName = T.Text
type Password = T.Text

data DatabaseInfo = PG DatabaseName Host UserName Password | SQLite FilePath

runDB (PG dbname host username password) action = do
  let pgConnectInfo = dbname `on` host `auth` (username, password)
  withPostgreSQL pgConnectInfo action

runDB (SQLite filePath) action =
  withSQLite filePath action
