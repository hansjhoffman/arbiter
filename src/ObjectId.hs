module ObjectId
  ( ObjectID
  , mkObjectID
  , toString
  ) where

import           Data.Aeson
import           Import
import           Nomics                         ( Crypto(..) )
import qualified RIO.Text                      as T


{- An opaque constructor. Do no export. -}

newtype ObjectID = ObjectID Text
  deriving (Eq, Show)

-- instance Eq ObjectID where
--   ObjectID id1 == ObjectID id2 = id1 == id2
--   T.eqString

instance ToJSON ObjectID where
  toJSON (ObjectID id') = toJSON id'


mkObjectID :: Crypto -> ObjectID
mkObjectID crypto = ObjectID (T.intercalate "-" parts)
 where
  parts :: [Text]
  parts = map T.toLower $ T.words (cryptoSymbol crypto <> " " <> cryptoName crypto)


toString :: ObjectID -> Text
toString (ObjectID objectId) = objectId
