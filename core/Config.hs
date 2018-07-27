module Config
where

import qualified Data.Text as T
import           DB
import           Env

data DatabaseCfg = MkDatabaseCfg Env DatabaseInfo
type RedisCfg = T.Text

data Config = MkConfig DatabaseCfg RedisCfg
