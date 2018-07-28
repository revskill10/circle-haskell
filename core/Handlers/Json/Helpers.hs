module Handlers.Json.Helpers
where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe
import           Handlers.Types         (User (..))

checkJWT :: MonadIO m => User -> m (Maybe Bool)
checkJWT (User "alice" "alice@gmail.com") = return $ Just True
checkJWT _                                = return Nothing
