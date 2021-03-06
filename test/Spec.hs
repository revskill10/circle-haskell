{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Control.Monad.IO.Class (liftIO)
import           Crypto.JOSE.JWK
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Data.Monoid            ((<>))
import qualified Data.String            as DS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.Lazy         as TL
import           Handlers.Types         (generateAppConfig, _jwtCfg)
import           Lib                    (User (..), app)
import qualified Lucid                  as L
import qualified Lucid.Base             as L
import           Network.HTTP.Types
import           Servant.Auth.Server
import           Servant.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.Hspec.Wai.Matcher

main :: IO ()
main = do
  (cfg, ctx) <- generateAppConfig
  hspec (spec cfg ctx)

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
    Left _    -> return $ headers ""
    Right jwt -> return $ headers jwt

spec cfg ctx =
  with (return (app cfg ctx)) $ do
    let jwtCfg = _jwtCfg cfg
    describe "Protected API" $ do
      it "responds with 401" $ do
        header <- liftIO $ mkHeaders invalidUser jwtCfg
        request methodGet  "/api/ep1" header "" `shouldRespondWith` 401
      it "responds with 200" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        request methodGet  "/api/ep1" header "" `shouldRespondWith` 200
      it "responds with 1797" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        request methodGet  "/api/ep1" header "" `shouldRespondWith` "1797"
      it   "responds with 200" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        request methodGet  "/api/ep2" header "" `shouldRespondWith` 200
      it "responds with usernamehi" $ do
        header <- liftIO $ mkHeaders validUser jwtCfg
        let expected = bodyEquals (DS.fromString . show $ _name validUser)
        request methodGet  "/api/ep2" header "" `shouldRespondWith` ResponseMatcher 200 [] expected
      it "succeed login" $
        request methodPost "/login" jsonHeader [json|{username: "truong", password: "dung"}|] `shouldRespondWith` 204
      it "fails login" $
        request methodPost "/login" jsonHeader [json|{username: "truong1", password: "dung"}|] `shouldRespondWith` 401
      it "fails login with malformed request" $ do
        request methodPost "/login" jsonHeader "" `shouldRespondWith` 400
        request methodPost "/login" jsonHeader [json|{username: "truong1"}|] `shouldRespondWith` 400
      it "renders homepage" $
        get "/" `shouldRespondWith` 200
