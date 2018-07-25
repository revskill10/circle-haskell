{-# LANGUAGE OverloadedStrings, FlexibleContexts, QuasiQuotes #-}
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
import Test.Hspec.Wai.Matcher

main :: IO ()
main = do
  myKey <- generateKey
  hspec (spec "hi" myKey)

bearerType jwt = BC.pack "Bearer " <> BL.toStrict jwt

headers :: BL.ByteString -> [Header]
headers jwt = [
    (hAuthorization,  bearerType jwt)
  ]

jsonHeader = [
    (hContentType, "application/json")
  ]  

validUser = User "alice" "alice@gmail.com"
invalidUser = User "test" "fdsfd@fdsfd.com"

mkHeaders user jwtCfg = do
  jwtRes <-  makeJWT user jwtCfg Nothing
  case jwtRes of
    Left _ -> return $ headers ""
    Right jwt -> return $ headers jwt

spec :: String -> JWK  -> Spec
spec dbConf myKey = do
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
  with (return (app dbConf jwtCfg cfg)) $
    describe "Protected API" $ do
      it "responds with 401" $ do
        header <- liftIO $ mkHeaders invalidUser jwtCfg
        (request methodGet  "/ep1" header "") `shouldRespondWith` 401
      it "responds with 200" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        (request methodGet  "/ep1" header "") `shouldRespondWith` 200
      it "responds with 1797" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        (request methodGet  "/ep1" header "") `shouldRespondWith` "1797"
      it   "responds with 200" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        (request methodGet  "/ep2" header "") `shouldRespondWith` 200
      it "responds with usernamehi" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        let expected = bodyEquals (DS.fromString . show $ (name validUser <> "hi"))
        request methodGet  "/ep2" header "" `shouldRespondWith` (ResponseMatcher 200 [] expected)
      it "succeed login" $ do
        request methodPost "/login" jsonHeader [json|{username: "truong", password: "dung"}|] `shouldRespondWith` 204
      it "fails login" $ do
        request methodPost "/login" jsonHeader [json|{username: "truong1", password: "dung"}|] `shouldRespondWith` 401
      it "fails login with malformed request" $ do
        request methodPost "/login" jsonHeader "" `shouldRespondWith` 400
        request methodPost "/login" jsonHeader [json|{username: "truong1"}|] `shouldRespondWith` 400