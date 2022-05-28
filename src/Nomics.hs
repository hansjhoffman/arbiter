module Nomics
  ( Crypto(..)
  , fetchAssets
  ) where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                )
import qualified Data.Aeson                    as JSON
import           Import
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


fetchAssets :: RIO App ()
fetchAssets = do
  env <- ask
  let nomicsApiKey = view nomicsApiKeyL env
      url          = "https://api.nomics.com"
  nakedRequest <- HTTP.parseRequest (T.unpack url)
  let req =
        HTTP.setRequestMethod "GET"
          $ HTTP.setRequestPath "v1/currencies/ticker"
          $ HTTP.setRequestQueryString
              [ ("key"     , Just . T.encodeUtf8 $ nomicsApiKey)
              , ("page"    , Just . T.encodeUtf8 $ "1")
              , ("per-page", Just . T.encodeUtf8 $ "100")
              , ("interval", Just . T.encodeUtf8 $ "1d")
              , ("status"  , Just . T.encodeUtf8 $ "active")
              , ("convert" , Just . T.encodeUtf8 $ "usd")
              ]
              nakedRequest
  -- res <- HTTP.httpJSON req
  logInfo "What now?"
