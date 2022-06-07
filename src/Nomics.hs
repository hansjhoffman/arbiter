module Nomics
  ( Crypto(..)
  , fetchAssets
  ) where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                )
import qualified Data.Aeson                    as JSON
import           Import
import           Network.HTTP.Simple            ( JSONException
                                                , Request
                                                , Response
                                                )
import qualified Network.HTTP.Simple           as HTTP
import           Prelude                        ( read )
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T


data Crypto = Crypto
  { cryptoId      :: !Text
  , cryptoLogoUrl :: !Text
  , cryptoName    :: !Text
  , cryptoSymbol  :: !Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON Crypto where
  parseJSON = JSON.withObject "Crypto" $ \obj ->
    Crypto <$> (obj .: "id") <*> (obj .: "logo_url") <*> (obj .: "name") <*> (obj .: "symbol")


data Status
  = Active
  | Dead
  | Inactive
  deriving (Eq, Show)


data Interval
  = Day
  | Hour
  | Week
  | Month
  | Year
  | YTD
  deriving (Eq, Show)


data Fiat = USD


type PageNumber = Integer


encodeStatus :: Status -> ByteString
encodeStatus = T.encodeUtf8 . \case
  Active   -> "active"
  Dead     -> "dead"
  Inactive -> "inactive"


encodeInterval :: Interval -> ByteString
encodeInterval = T.encodeUtf8 . \case
  Day   -> "1d"
  Hour  -> "1h"
  Week  -> "7d"
  Month -> "30d"
  Year  -> "365d"
  YTD   -> "ytd"


encodeFiat :: Fiat -> ByteString
encodeFiat USD = T.encodeUtf8 "usd"


perPage :: PageNumber
perPage = 100


buildRequest :: Text -> PageNumber -> Request
buildRequest nomicsApiKey currentPage =
  HTTP.setRequestHost host
    $ HTTP.setRequestPort 443
    $ HTTP.setRequestSecure True
    $ HTTP.setRequestPath path
    $ HTTP.setRequestQueryString
        [ ("key"     , Just . T.encodeUtf8 $ nomicsApiKey)
        , ("page"    , Just . T.encodeUtf8 $ tshow currentPage)
        , ("per-page", Just . T.encodeUtf8 $ tshow perPage)
        , ("interval", Just $ encodeInterval Day)
        , ("status"  , Just $ encodeStatus Active)
        , ("convert" , Just $ encodeFiat USD)
        ]
        HTTP.defaultRequest
 where
  host = "api.nomics.com"
  path = "v1/currencies/ticker"


-- Nomics API has a rate limit of 1 request per second w/ free API keys. Need to debounce.
fetchAssets :: RIO App (Either JSONException [Crypto])
fetchAssets = do
  env <- ask
  let nomicsApiKey = view nomicsApiKeyL env
  response <- HTTP.httpJSONEither $ buildRequest nomicsApiKey (1 :: Integer)
  return $ HTTP.getResponseBody response


-- Make a single API request to read pagination headers
preflight :: RIO App Integer
preflight = do
  env <- ask
  let nomicsApiKey = view nomicsApiKeyL env
  response <- HTTP.httpNoBody $ buildRequest nomicsApiKey 1
  let totalItems = B.concat . HTTP.getResponseHeader "X-Pagination-Total-Items" $ response
  logInfo $ "Preflight check found " <> displayShow totalItems <> " total assets"
  return (read $ show totalItems :: Integer)


-- google http conduit exception handling
-- use some sort of "state monad" for currentPage and [Crypto]? Or just pass in as params?
-- conduit fold? https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Data-Conduit-Combinators.html#v:fold
-- conduit repeatWhileM? https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Data-Conduit-Combinators.html#v:repeatWhileM

-- 1. fetch assets from Nomics API
-- 2. grab totalCount from headers and determine if we've reached the end ==> totalItems / 100 (round-up) > currentPageNumber
-- 3. append body to previous [assets] and repeat loop from step 1
-- ** should any one of the network requests fail, fail completely and log error

-- 1. make a preflight check that ignores the body, grabs an returns the totalCount from the header
-- 2. use that to calcuate the number of requets that need to be made, describe them, then fold/loop over
-- them?
