module DB.Parser
where

import           Data.Text
import           Network.URI
import           Prelude
import           System.Environment

-- | read the DATABASE_URL environment variable
-- and return an alist of connection parameters with the following keys:
-- user, password, host, port, dbname
--
-- warning: just calls error if it can't parse correctly
-- conn = "postgres://revskill:Dung123@localhost:5432/revskill"
-- x <- dbConnParams' "DATABASE_URL" (parseDatabaseUrl' "postgres:")
dbConnParams' :: String -> (String -> [(Text, Text)]) -> IO [(Text, Text)]
dbConnParams' envVar parse = parse <$> getEnv envVar

getScheme' conn = uriScheme <$> parseAbsoluteURI conn

parseDatabaseUrl' :: String -> String -> [(Text, Text)]
parseDatabaseUrl' scheme durl =
  let muri = parseAbsoluteURI durl
      (auth, path) = case muri of
                      Nothing ->  error "couldn't parse absolute uri"
                      Just uri -> if uriScheme uri /= scheme
                                    then schemeError uri
                                    else case uriAuthority uri of
                                            Nothing -> invalid
                                            Just a  -> (a, uriPath uri)
      (user,password) = userAndPassword auth
  in     [ (pack "user",     user)
            -- tail not safe, but should be there on Heroku
          , (pack "password", Data.Text.tail password)
          , (pack "host",     pack $ uriRegName auth)
          , (pack "port",     pack $ removeColon $ uriPort auth)
          -- tail not safe but path should always be there
          , (pack "dbname",   pack $ Prelude.tail  path)
          ]
  where
    removeColon (':':port) = port
    removeColon port       = port

    -- init is not safe, but should be there on Heroku
    userAndPassword :: URIAuth -> (Text, Text)
    userAndPassword = breakOn (pack ":") . pack . Prelude.init . uriUserInfo

    schemeError uri = error $ "was expecting a postgres scheme, not: " ++ uriScheme uri ++ "\n" ++ show uri
    -- should be an error
    invalid = error "could not parse heroku DATABASE_URL"
