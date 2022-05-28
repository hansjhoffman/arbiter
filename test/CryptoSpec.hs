module CryptoSpec
  ( spec
  ) where

import           Data.Aeson                     ( decode )
import           Nomics                         ( Crypto(..) )
import           RIO
import qualified RIO.ByteString.Lazy           as B
import           Test.Hspec


spec :: Spec
spec = do
  describe "Decoder" $ do
    it "BTC success" $ do
      let
        jsonBS :: B.ByteString
        jsonBS
          = "{\"id\":\"BTC\",\"symbol\":\"BTC\",\"name\":\"Bitcoin\",\"logo_url\":\"https://s3.us-east-2.amazonaws.com/nomics-api/static/images/currencies/btc.svg\"}"

        decoded :: Maybe Crypto
        decoded = decode jsonBS
      isJust decoded `shouldBe` True
    it "BTC failure" $ do
      let
        jsonBS :: B.ByteString
        jsonBS
          = "{\"symbol\":\"BTC\",\"name\":\"Bitcoin\",\"logo_url\":\"https://s3.us-east-2.amazonaws.com/nomics-api/static/images/currencies/btc.svg\"}"

        decoded :: Maybe Crypto
        decoded = decode jsonBS
      isNothing decoded `shouldBe` True
