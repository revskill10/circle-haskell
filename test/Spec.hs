{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import Lib (app, User(..))
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Servant.Server
import Servant.Auth.Server
import Control.Monad.IO.Class (liftIO)
import Crypto.JOSE.JWK
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.String as DS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Reader

main :: IO ()
main = do
  myKey <- generateKey
  hspec (spec myKey)

headers :: BL.ByteString -> [Header]
headers jwt= [
    (hAuthorization, BC.pack "Bearer " <> BL.toStrict jwt )
  ]

validUser = User "alice" "alice@gmail.com"
invalidUser = User "test" "fdsfd@fdsfd.com"

mkHeaders user jwtCfg = do
  jwtRes <-  makeJWT user jwtCfg Nothing
  case jwtRes of
    Left _ -> return $ headers ""
    Right jwt -> return $ headers jwt

spec :: JWK  -> Spec
spec myKey = do
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
  with (return (app jwtCfg cfg)) $
    describe "GET /ep1" $ do
      it "responds with 401" $ do
        header <- liftIO $ mkHeaders invalidUser jwtCfg
        (request methodGet  "/ep1" header "") `shouldRespondWith` 401
      it "responds with 200" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        (request methodGet  "/ep1" header "") `shouldRespondWith` 200
