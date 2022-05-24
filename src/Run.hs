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
  deriving (Eq, Generic, Show)

instance FromJSON Crypto


data Entry = Entry
  { entryObjectID :: ObjectID
  , entryLogoUrl  :: Text
  , entryName     :: Text
  , entrySymbol   :: Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON Entry where
  toJSON     = JSON.genericToJSON $ jsonOptions "entry"
  toEncoding = JSON.genericToEncoding $ jsonOptions "entry"


newtype ObjectID = ObjectID Text
  deriving (Eq, Show)

instance ToJSON ObjectID where
  toJSON (ObjectID id') = toJSON id'


jsonOptions :: String -> JSON.Options
jsonOptions prefix = JSON.defaultOptions
  { JSON.fieldLabelModifier = map toLower . drop (length prefix)
  }


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
  parts = RIO.map T.toLower
    $ T.words (cryptoSymbol crypto <> " " <> cryptoName crypto)


upsertEntries :: [Entry] -> RIO App ()
upsertEntries entries = do
  nakedRequest <- HTTP.parseRequest (T.unpack url)
  let req =
        HTTP.setRequestMethod "POST"
          $ HTTP.addRequestHeader "X-Algolia-API-Key" "algoliaApiKey"
          $ HTTP.addRequestHeader "X-Algolia-Application-Id" "algoliaAppId"
          $ HTTP.setRequestBodyJSON entries nakedRequest
  res <- HTTP.httpNoBody req
  logInfo "Done"
 where
  url :: Text
  url =
    "https://"
      <> "alogliaAppId"
      <> "-dsn.alogolia.net"
      <> "/1/"
      <> "algoliaIndex"


run :: RIO App ()
run =
  let btc :: Crypto
      btc = Crypto "btc" "https://someurl.com" "Bitcoin" "BTC"

      eth :: Crypto
      eth = Crypto "eth" "https://someurl.com" "Ethereum" "ETH"

      entries :: [Entry]
      entries = [mkEntry btc, mkEntry eth]
  in  logInfo "Uploading entries to Alogolia"
