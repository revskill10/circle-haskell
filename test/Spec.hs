{-# LANGUAGE OverloadedStrings #-}
import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return (app "hi")) $
  describe "GET /ep1" $ do
  it "responds with 200" $
    get "/ep1" `shouldRespondWith` 200
  it "responds with 1797" $
    get "/ep1" `shouldRespondWith` "1797"
  it "responds with 200" $
    get "/ep2" `shouldRespondWith` 200
  it "responds with hi" $
    get "/ep2" `shouldRespondWith` "\"hi\""
