module Run
  ( run
  ) where

-- import qualified Algolia
import           Import
import qualified Nomics


run :: RIO App ()
run = do
  logInfo "Fetching crypto assets..."
  totalItems <- Nomics.preflight
  let totalPages = ceiling $ fromInteger totalItems / 100
  logInfo $ "Preflight check found " <> displayShow totalItems <> " total assets"
  logInfo $ "Total pages: " <> displayShow totalPages
  results <- sequence [Nomics.fetchAssets]
  -- results2 <- concat <$> sequence [Nomics.fetchAssets]
  -- results <- sequence [Nomics.fetchAssets 1, Nomics.fetchAssets 2, ...]
  -- cryptoAssets <- Nomics.fetchAssets
  -- logInfo "Uploading entries to Alogolia..."
  -- Algolia.saveObjects results
  logInfo "Done!"
