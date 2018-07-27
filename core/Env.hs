module Env where

data Env = Development | Test | Production
    deriving (Show, Eq)

getEnv "TEST"        = Test
getEnv "DEVELOPMENT" = Development
getEnv "PRODUCTION"  = Production
getEnv _             = Development
