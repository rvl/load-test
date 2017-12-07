{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable         (fold)
import qualified Data.Text             as T
import           Options.Applicative
import           System.Exit
import           System.IO             (hPutStrLn, stderr)

import Config
import Lib

main :: IO ()
main = getConfig >>= startApp

-- | Parse the command line options. If incorrect options are given exit with
-- 'exitFailure'.
getConfig :: IO Config
getConfig = execParser options

options :: ParserInfo Config
options = info (helper <*> configParser) description
  where
    configParser = Config
      <$> option auto (fold [ long "port"
                            , short 'p'
                            , metavar "PORT"
                            , help "port to listen on"
                            , value 8080
                            , showDefault
                            ]
                      )
      <*> (T.pack <$> strOption (fold [ long "hostname"
                                      , short 'n'
                                      , metavar "HOST"
                                      , help "The hostname of this server"
                                      ]
                                )
          )
      <*> option auto (fold [ long "access-log-level"
                            , short 'a'
                            , metavar "LOG_LEVEL"
                            , help "Level at which to log http accesses"
                            , value Disabled
                            , showDefault
                            ]
                      )
      <*> (T.pack <$> strOption (fold [ long "database"
                                      , short 'e'
                                      , metavar "CONNECTION_STRING"
                                      , help "postgres database connection string, see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
                                      ]
                                )
          )

    description = fold
      [ fullDesc
      , header "load-test-server"
      , progDesc "Something to load test"
      ]
