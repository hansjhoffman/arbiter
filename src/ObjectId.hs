module ObjectId
  ( ObjectID
  , mkObjectID
  , toString
  ) where

import           Data.Aeson
import           Import
import           Nomics                         ( Crypto(..) )
import qualified RIO.Text                      as T


-- | An opaque type. Do no export.

newtype ObjectID = ObjectID Text
  deriving (Eq, Show)

instance ToJSON ObjectID where
  toJSON (ObjectID objectId) = toJSON objectId


mkObjectID :: Crypto -> ObjectID
mkObjectID crypto = ObjectID (T.intercalate "-" parts)
 where
  parts :: [Text]
  parts = map T.toLower $ T.words (cryptoSymbol crypto <> " " <> cryptoName crypto)


toString :: ObjectID -> Text
toString (ObjectID objectId) = objectId
