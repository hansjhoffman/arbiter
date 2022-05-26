module Run
  ( run
  ) where

import           Data.Aeson
import qualified Data.Aeson                    as JSON
import           Data.Char                      ( toLower )
import           Import
import qualified Network.HTTP.Simple           as HTTP
import           RIO
import qualified RIO.Text                      as T


data Crypto = Crypto
  { cryptoId      :: !Text
  , cryptoLogoUrl :: !Text
  , cryptoName    :: !Text
  , cryptoSymbol  :: !Text
  }
  deriving (Generic, Show)

instance FromJSON Crypto


data UpdateAction = UpdateAction
  { updateAction :: Text
  , updateBody   :: Entry
  }
  deriving (Generic, Show)

instance ToJSON UpdateAction where
  toJSON     = JSON.genericToJSON $ jsonOptions "update"
  toEncoding = JSON.genericToEncoding $ jsonOptions "update"


newtype BatchRequest = BatchRequest
  { requests :: [UpdateAction]
  }
  deriving (Generic, Show)

instance ToJSON BatchRequest


data Entry = Entry
  { entryObjectID :: ObjectID
  , entryLogoUrl  :: Text
  , entryName     :: Text
  , entrySymbol   :: Text
  }
  deriving (Generic, Show)

instance ToJSON Entry where
  toJSON     = JSON.genericToJSON $ jsonOptions "entry"
  toEncoding = JSON.genericToEncoding $ jsonOptions "entry"


newtype ObjectID = ObjectID Text
  deriving (Show)

instance ToJSON ObjectID where
  toJSON (ObjectID id') = toJSON id'


jsonOptions :: String -> JSON.Options
jsonOptions prefix = JSON.defaultOptions
  { JSON.fieldLabelModifier = applyFirst toLower . drop (length prefix)
  }
 where
  applyFirst :: (Char -> Char) -> String -> String
  applyFirst _ []       = []
  applyFirst f [x     ] = [f x]
  applyFirst f (x : xs) = f x : xs


mkEntry :: Crypto -> Entry
mkEntry crypto = Entry { entryObjectID = mkObjectID crypto
                       , entryLogoUrl  = cryptoLogoUrl crypto
                       , entryName     = cryptoName crypto
                       , entrySymbol   = cryptoSymbol crypto
                       }


mkObjectID :: Crypto -> ObjectID
mkObjectID crypto = ObjectID (T.intercalate "-" parts)
 where
  parts :: [Text]
  parts = RIO.map T.toLower $ T.words (cryptoSymbol crypto <> " " <> cryptoName crypto)



saveObjects :: [Crypto] -> RIO App ()
saveObjects cryptos = undefined


saveBatch :: BatchRequest -> RIO App ()
saveBatch batch = do
  env <- ask
  let algoliaIndex  = view algoliaIndexL env
      algoliaAppId  = view algoliaAppIdL env
      algoliaApiKey = view algoliaApiKeyL env
      url           = "https://" <> algoliaAppId <> "-dsn.algolia.net"
  nakedRequest <- HTTP.parseRequest (T.unpack url)
  let req =
        HTTP.setRequestMethod "POST"
          $ HTTP.setRequestPath ("1/indexes/" <> T.encodeUtf8 algoliaIndex <> "/batch")
          $ HTTP.addRequestHeader "X-Algolia-API-Key" (T.encodeUtf8 algoliaApiKey)
          $ HTTP.addRequestHeader "X-Algolia-Application-Id" (T.encodeUtf8 algoliaAppId)
          $ HTTP.setRequestBodyJSON batch nakedRequest
  res <- HTTP.httpNoBody req
  logInfo ("Batch uploaded: " <> displayShow (HTTP.getResponseStatusCode res))


run :: RIO App ()
run = do
  logInfo "Uploading entries to Alogolia..."
  saveBatch batch
 where
  btc :: Crypto
  btc = Crypto "btc" "https://image-url.com" "Bitcoin" "BTC"

  eth :: Crypto
  eth = Crypto "eth" "https://image-url.com" "Ethereum" "ETH"

  objects :: [UpdateAction]
  objects = [UpdateAction "updateObject" (mkEntry btc), UpdateAction "updateObject" (mkEntry eth)]

  batch :: BatchRequest
  batch = BatchRequest objects
