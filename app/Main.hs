{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import           Import
import           Options.Applicative.Simple
import qualified Paths_arbiter
import           RIO.Process
import qualified RIO.Text                      as T
import           Run
import           System.Environment             ( getEnv )


main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_arbiter.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options <$> switch (long "verbose" <> short 'v' <> help "Verbose output?"))
    empty
  logOpts <- logOptionsHandle stderr (optionsVerbose options)
  processCtx <- mkDefaultProcessContext
  algoliaApiKey <- getEnv "RYKA_ALGOLIA_API_KEY"
  algoliaAppId <- getEnv "RYKA_ALGOLIA_APP_ID"
  algoliaIndex <- getEnv "RYKA_ALGOLIA_INDEX"
  nomicsApiKey <- getEnv "NOMICS_API_KEY"
  withLogFunc logOpts $ \lf ->
    let app = App { appAlgoliaApiKey = T.pack algoliaApiKey
                  , appAlgoliaAppId = T.pack algoliaAppId
                  , appAlgoliaIndex = T.pack algoliaIndex
                  , appLogFunc        = lf
                  , appNomicsApiKey = T.pack nomicsApiKey
                  , appOptions        = options
                  , appProcessContext = processCtx
                  }
    in  runRIO app run
