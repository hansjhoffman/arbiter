module Algolia
  ( saveObjects
  ) where

import           Data.Aeson
import qualified Data.Aeson                    as JSON
import           Data.Char                      ( toLower )
import           Import
import qualified Network.HTTP.Simple           as HTTP
import           Nomics                         ( Crypto(..) )
import           ObjectId                       ( ObjectID
                                                , mkObjectID
                                                )
import qualified RIO.Text                      as T


data ActionType = UpdateObject
  deriving Show

instance ToJSON ActionType where
  toJSON = \case
    UpdateObject -> "updateObject"


data BatchAction = BatchAction
  { updateAction :: ActionType
  -- ^ API action to peform
  , updateBody   :: Entry
  -- ^ API data object
  }
  deriving (Generic, Show)

instance ToJSON BatchAction where
  toJSON     = JSON.genericToJSON $ jsonOptions "update"
  toEncoding = JSON.genericToEncoding $ jsonOptions "update"


newtype BatchRequest = BatchRequest
  { requests :: [BatchAction]
  -- ^ Shape of body required by API
  }
  deriving (Generic, Show)

instance ToJSON BatchRequest


data Entry = Entry
  { entryObjectID :: ObjectID
  -- ^ Custom formatted Id required by API
  , entryLogoUrl  :: Text
  -- ^ Same as crypto logo_url
  , entryName     :: Text
  -- ^ Same as crypto name
  , entrySymbol   :: Text
  -- ^ Same as crypto symbol
  }
  deriving (Generic, Show)

instance ToJSON Entry where
  toJSON     = JSON.genericToJSON $ jsonOptions "entry"
  toEncoding = JSON.genericToEncoding $ jsonOptions "entry"


mkEntry :: Crypto -> Entry
mkEntry crypto = Entry { entryObjectID = mkObjectID crypto
                       , entryLogoUrl  = cryptoLogoUrl crypto
                       , entryName     = cryptoName crypto
                       , entrySymbol   = cryptoSymbol crypto
                       }


mkBatchAction :: Crypto -> BatchAction
mkBatchAction = BatchAction UpdateObject . mkEntry


jsonOptions :: String -> JSON.Options
jsonOptions prefix = JSON.defaultOptions
  { JSON.fieldLabelModifier = applyFirst toLower . drop (length prefix)
  }
 where
  applyFirst :: (Char -> Char) -> String -> String
  applyFirst _ []       = []
  applyFirst f [x     ] = [f x]
  applyFirst f (x : xs) = f x : xs


-- | https://www.algolia.com/doc/rest-api/search/#batch-write-operations
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


-- | Algolia docs suggest a max batch size of 100K objects.
batchSize :: Int
batchSize = 25000


saveObjects :: [Crypto] -> RIO App ()
saveObjects [] = logInfo "Done!"
saveObjects cs = do
  let actions :: [BatchAction]
      actions = map mkBatchAction $ take batchSize cs
  saveBatch $ BatchRequest actions
  saveObjects (drop batchSize cs)
