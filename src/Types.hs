module Types where

import           RIO
import           RIO.Process


-- | Command line arguments
newtype Options = Options
  { optionsVerbose :: Bool
  }


data App = App
  { appAlgoliaApiKey  :: !Text
  , appAlgoliaAppId   :: !Text
  , appAlgoliaIndex   :: !Text
  , appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

class HasAlgoliaApiKey env where
   algoliaApiKeyL :: Lens' env Text

instance HasAlgoliaApiKey App where
  algoliaApiKeyL = lens appAlgoliaApiKey (\x y -> x { appAlgoliaApiKey = y })

class HasAlgoliaAppId env where
   algoliaAppIdL :: Lens' env Text

instance HasAlgoliaAppId App where
  algoliaAppIdL = lens appAlgoliaAppId (\x y -> x { appAlgoliaAppId = y })

class HasAlgoliaIndex env where
   algoliaIndexL :: Lens' env Text

instance HasAlgoliaIndex App where
  algoliaIndexL = lens appAlgoliaIndex (\x y -> x { appAlgoliaIndex = y })

