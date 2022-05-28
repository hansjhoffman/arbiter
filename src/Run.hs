module Run
  ( run
  ) where

import qualified Algolia
import           Import
import           Nomics                         ( Crypto(..) )
-- import qualified Nomics


run :: RIO App ()
run = do
  logInfo "Fetching crypto assets..."
  -- cryptoAssets <- Nomics.fetchAssets
  logInfo "Uploading entries to Alogolia..."
  Algolia.saveObjects [btc, eth]
 where
  btc :: Crypto
  btc = Crypto "btc" "https://image-url.com" "Bitcoin" "BTC"

  eth :: Crypto
  eth = Crypto "eth" "https://image-url.com" "Ethereum" "ETH"
