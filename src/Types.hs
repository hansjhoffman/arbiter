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
  , appLogFn          :: !LogFunc
  , appNomicsApiKey   :: !Text
  , appOptions        :: !Options
  , appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens appLogFn (\x y -> x { appLogFn = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

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

class HasNomicsApiKey env where
   nomicsApiKeyL :: Lens' env Text

instance HasNomicsApiKey App where
  nomicsApiKeyL = lens appNomicsApiKey (\x y -> x { appNomicsApiKey = y })
