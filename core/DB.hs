{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module DB
where

import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Database.Selda
import           Database.Selda.Generic
import           Database.Selda.PostgreSQL
import           Database.Selda.SQLite
import           DB.Parser
import           GHC.Generics
import           Protolude                 hiding (catch, on, putStrLn, show)

data UserMD = UserMD
 {
    userId :: T.Text
  , name   :: T.Text
  , email  :: T.Text
  , age    :: Int
 } deriving Generic

users :: GenTable UserMD
users = genTable "users" [userId :- primaryGen]

setup :: SeldaM ()
setup =
  tryCreateTable (gen users)

teardown :: SeldaM ()
teardown =
  tryDropTable (gen users)

type DatabaseName = T.Text
type Host = T.Text
type UserName = T.Text
type Password = T.Text
type Port = Int
type Scheme = String
type ConnectionString = String
data DatabaseInfo = PG DatabaseName Host UserName Password Port | SQLite FilePath
  deriving Show

convertPG :: ReaderT [(Text, Text)] Maybe DatabaseInfo
convertPG = do
  x <- asks M.fromList
  let f name = M.lookup name x
  lift $ PG <$> f "dbname" <*> f "host" <*> f "user" <*> f "password" <*> (readMaybe =<< (T.unpack <$> f "port"))

liftEnv :: forall env' env m a. Monad m => ReaderT env' m a -> env' -> ReaderT env m a
liftEnv comp env = lift (runReaderT comp env)

parsePGDatabaseInfo :: Scheme -> ReaderT ConnectionString Maybe [(Text, Text)]
parsePGDatabaseInfo scheme = asks (parseDatabaseUrl' scheme)

parse :: ReaderT ConnectionString Maybe DatabaseInfo
parse = do
  conn <- ask
  scheme <- lift $ getScheme' conn
  case scheme of
    "postgres:" -> liftEnv convertPG =<< parsePGDatabaseInfo scheme
  --  "sqlite3:"  -> liftEnv convertSqlite =<< parseSqliteInfo scheme
    _           -> fail "Fail"

runParser = runReaderT parse

runDB (PG dbname host username password port) action = do
  let pgConnectInfo = (dbname `on` host `auth` (username, password) ) { pgPort = port }
  withPostgreSQL pgConnectInfo action `catch` throwE

runDB (SQLite filePath) action = withSQLite filePath action `catch` throwE


newtype ActionError = ActionError T.Text
  deriving Show

-- runAction :: (IsString e, MonadCatch m, Exception e, MonadSelda m) => ConnectionString -> m () -> ExceptT e m ()
runAction connStr action = runExceptT $ do
  let conn = runParser connStr
  case conn of
    Nothing    -> throwE $ ActionError "Incorrect connection string"
    Just conn_ -> withExceptT toActionError (runDB conn_ action)

toActionError :: SeldaError -> ActionError
toActionError (DbError e)  = ActionError . T.pack  $ e
toActionError (SqlError e) = ActionError . T.pack  $ e
