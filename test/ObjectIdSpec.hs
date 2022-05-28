module ObjectIdSpec
  ( spec
  ) where

import           Import
import           Nomics                         ( Crypto(..) )
import           ObjectId                       ( mkObjectID
                                                , toString
                                                )
import           Test.Hspec


spec :: Spec
spec = do
  describe "Custom Ids" $ do
    it "BTC check" $ do
      let crypto :: Crypto
          crypto = Crypto "BTC" "https://image-url.com" "Bitcoin" "BTC"

          objectId :: Text
          objectId = toString . mkObjectID $ crypto
      objectId `shouldBe` "btc-bitcoin"
    it "USDC check" $ do
      let crypto :: Crypto
          crypto = Crypto "USDC" "https://image-url.com" "USD Coin" "USDC"

          objectId :: Text
          objectId = toString . mkObjectID $ crypto
      objectId `shouldBe` "usdc-usd-coin"
    it "STETH check" $ do
      let crypto :: Crypto
          crypto = Crypto "STETH" "https://image-url.com" "Liquid Staked Ether" "STETH"

          objectId :: Text
          objectId = toString . mkObjectID $ crypto
      objectId `shouldBe` "steth-liquid-staked-ether"

