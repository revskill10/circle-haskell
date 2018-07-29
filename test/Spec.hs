{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           App
import           Control.Monad.IO.Class (liftIO)
import qualified Data.String            as DS
import           Network.HTTP.Types
import           Spec.Helpers
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.Hspec.Wai.Matcher

main :: IO ()
main = do
  (testApp, jwtCfg) <- generateMockApp
  hspec $ spec (testApp, jwtCfg)

spec (app, jwtCfg) =
  with (return app) $
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
      it "responds with 200" $ do
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
