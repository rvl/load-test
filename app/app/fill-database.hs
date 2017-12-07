{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable         (fold)
import qualified Data.Text             as T
import           Data.Text.Encoding    (encodeUtf8)
import           Options.Applicative
import           System.Exit
import           System.IO             (hPutStrLn, stderr)
import           Control.Monad         (when)
import           Data.Monoid
import Data.Default
import Say
import Database.PostgreSQL.Simple      (Connection, close, connectPostgreSQL, withTransaction)

import Config
import Lib

main :: IO ()
main = getConfig >>= uncurry appInit

appInit :: Verbosity -> Config -> IO ()
appInit v Config{..} = do
  conn <- connectPostgreSQL (encodeUtf8 configDatabaseConnectionString)
  say "Hello there."


-- | Parse the command line options. If incorrect options are given exit with
-- 'exitFailure'.
getConfig :: IO (Verbosity, Config)
getConfig = do
  (v, c) <- execParser options
  pure (v, def { configDatabaseConnectionString = c })

options :: ParserInfo (Verbosity, T.Text)
options = info (helper <*> parser) description
  where
    parser = (,) <$> verbosity <*> connStrParser
    connStrParser = T.pack <$> strOption (fold [ long "database"
                                               , short 'e'
                                               , metavar "CONNECTION_STRING"
                                               , help "postgres database connection string, see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
                                               ]
                                         )

    verbosity = boolToVerbosity <$> switch ( long "verbose" <> short 'v' <> help "Show more information" )
    description = fold
      [ fullDesc
      , header "fill-database"
      , progDesc "Populates database with test data"
      ]

data Verbosity = Verbose | Quiet

verbosityToBool :: Verbosity -> Bool
verbosityToBool Verbose = True
verbosityToBool Quiet = False

boolToVerbosity :: Bool -> Verbosity
boolToVerbosity b = if b then Verbose else Quiet
