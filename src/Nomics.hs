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


request :: Request
request =
  HTTP.setRequestHost host
    $ HTTP.setRequestPort 443
    $ HTTP.setRequestSecure True
    $ HTTP.setRequestPath path HTTP.defaultRequest
 where
  host = "api.nomics.com"
  path = "v1/currencies/ticker"


-- Nomics API has a rate limit of 1 request per second w/ free API keys. Need to debounce.
fetchAssets :: RIO App (Either JSONException [Crypto])
fetchAssets = do
  env <- ask
  let nomicsApiKey = view nomicsApiKeyL env
  response <-
    HTTP.httpJSONEither $ HTTP.setRequestQueryString
      [ ("key"     , Just . T.encodeUtf8 $ nomicsApiKey)
      , ("page"    , Just . T.encodeUtf8 $ tshow (1 :: Integer))
      , ("per-page", Just . T.encodeUtf8 $ tshow (100 :: Integer))
      , ("interval", Just . T.encodeUtf8 $ "1d")
      , ("status"  , Just . T.encodeUtf8 $ "active")
      , ("convert" , Just . T.encodeUtf8 $ "usd")
      ]
      request :: RIO App (Response (Either JSONException [Crypto]))
  let totalItems = HTTP.getResponseHeader "X-Pagination-Total-Items" response
  logInfo $ displayShow totalItems
  logInfo $ displayShow (HTTP.getResponseBody response)
  pure $ HTTP.getResponseBody response


-- fetchAssets' :: RIO App [Crypto]
-- fetchAssets' = fetchAssets'' 1 <$> ask
--  where
--   fetchAssets'' :: Int -> App -> RIO App (Either JSONException [Crypto])
--   fetchAssets'' pageNumber env = do
--     let nomicsApiKey = view nomicsApiKeyL env
--     response <-
--       HTTP.httpJSONEither $ HTTP.setRequestQueryString
--         [ ("key"     , Just . T.encodeUtf8 $ nomicsApiKey)
--         , ("page"    , Just . T.encodeUtf8 $ tshow pageNumber)
--         , ("per-page", Just . T.encodeUtf8 $ "5")
--         , ("interval", Just . T.encodeUtf8 $ "1d")
--         , ("status"  , Just . T.encodeUtf8 $ "active")
--         , ("convert" , Just . T.encodeUtf8 $ "usd")
--         ]
--         request :: RIO App (Response (Either JSONException [Crypto]))
--     case response of
--       Left  _ -> []
--       Right _ -> []
--     logInfo "Got page " <> show pageNumber
--     let totalItems = HTTP.getResponseHeader "X-Pagination-Total-Items" response
--     -- if totalItems / 100 (roundUp) > pageNumber then body ++ recursive_call else done
