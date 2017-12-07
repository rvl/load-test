{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module ServerEnv
  ( Env(..)
  , App(..)
  , runApp
  , newEnv
  , deleteEnv
  , runQueryWithConnection
  , runQueryWithConnectionSingular
  , runUpdateReturningWithConnection
  , runUpdateWithConnection
  , withConnection
  , getHostAndPort
  , liftIO
  , Connection
  , withTransaction
  ) where

import Control.Monad.Except.Extra
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Log
import Control.Monad.Reader
import Data.ByteString.Extra           as BS (readFileMaybe, writeFile, ByteString)
import qualified Data.ByteString.Char8 as S8 (pack)
import Data.ByteString.Lazy            (toStrict)
import Data.Int                        (Int64)
import Data.List                       (find)
import Data.Maybe                      (fromMaybe)
import Data.Pool
import Data.Profunctor.Product.Default (Default)
import Data.Semigroup
import Data.String                     (fromString)
import Data.Text                       (Text, pack)
import Data.Text.Encoding              (encodeUtf8)
import Data.Time.Format
import Database.PostgreSQL.Simple      (Connection, close, connectPostgreSQL, withTransaction)
import Opaleye                         (Query, QueryRunner, Unpackspec,
                                        Table, Column, PGBool,
                                        runQuery, runUpdateReturning, runUpdate, showSql)
import Say
import Servant                         (ServantErr)
import Config
import Log
import Network.Wai.Handler.Warp             (Port)

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

data Env = Env { envConnectionPool      :: Pool Connection
               , envPort                :: Port
               , envHostname            :: HostName
               }

newtype App a = App
  { unApp :: ReaderT Env (ExceptT ServantErr (LogM (WithSeverity LogMessage) IO)) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError ServantErr
           , MonadIO
           , MonadBase IO
           , MonadLog (WithSeverity LogMessage)
           , MonadReader Env
           )

instance MonadBaseControl IO App where
  type StM App a = a
  liftBaseWith f = App $ liftBaseWith $ \runInBase -> f (helpMeImConfused . runInBase . unApp)
  restoreM = App . restoreM . restoreM

helpMeImConfused :: IO (Either ServantErr a) -> IO a
helpMeImConfused action = action >>= \case
  Right a -> return a
  Left err -> fail $ "oh bugger, ServantErr: " ++ show err

-- | Perform an action with a PostgreSQL connection to the DB and
-- return the result
withConnection :: (Connection -> IO a) -> App a
withConnection f = do
  connectionPool <- asks envConnectionPool
  liftIO $ withResource connectionPool f


-- | Evaluate a query in an 'App' value
runQueryWithConnection
  :: Default QueryRunner columns haskells
  => Default Unpackspec columns columns
  => Query columns -> App [haskells]
runQueryWithConnection q = do
  logQuery q
  withConnection (\c -> runQuery c q)

runUpdateReturningWithConnection
  :: Default QueryRunner columnsReturned haskells
  => Table columnsW columnsR
  -> (columnsR -> columnsW)
  -> (columnsR -> Column PGBool)
  -> (columnsR -> columnsReturned)
  -> App [haskells]
runUpdateReturningWithConnection t u p f =
  withConnection (\c -> runUpdateReturning c t u p f)

runUpdateWithConnection
  :: Table columnsW columnsR
  -> (columnsR -> columnsW)
  -> (columnsR -> Column PGBool)
  -> App Int64
runUpdateWithConnection t u p =
  withConnection (\c -> runUpdate c t u p)

-- | Evaluate a query in an 'App' value returning a singular result
runQueryWithConnectionSingular
  :: Default QueryRunner columns haskells
  => Default Unpackspec columns columns =>
     Query columns -> App (Maybe haskells)
runQueryWithConnectionSingular q =
  runQueryWithConnection q >>=
  \case
    []  -> pure Nothing
    [x] -> pure $ Just x
    _   -> do
      logError "Singular query returned multiple results"
      pure Nothing

logQuery
  :: Default Unpackspec columns columns
  => Query columns
  -> App ()
logQuery q =
  let s = fromMaybe "Empty query" $ showSql q
  in logDebug (fromString s)

runApp :: Env -> App a -> ExceptT ServantErr IO a
runApp env = mapExceptT runLog
           . flip runReaderT env
           . unApp
  where
    runLog :: LogM (WithSeverity LogMessage) IO a -> IO a
    runLog = (`runLoggingT` printMessage) . mapLogMessageM timestamp
    printMessage :: WithTimestamp (WithSeverity LogMessage) -> IO ()
    printMessage = print . renderWithTimestamp renderTime (renderWithSeverity render)
    renderTime = formatTime defaultTimeLocale "%b %_d %H:%M:%S"

-- | Get a pool of connections to the database.
getDatabaseConnection :: MonadIO m => Config -> m (Pool Connection)
getDatabaseConnection Config{..} = liftIO $ createPool
    (connectPostgreSQL (encodeUtf8 configDatabaseConnectionString))
    close
    4 10 4

-- | Get the hostname and port for this server separated by a colon
--
-- >>> getHostAndPort
-- "localhost:8080"
getHostAndPort :: App Text
getHostAndPort = do
  hostname <- asks envHostname
  port <- asks envPort
  pure $ hostname <> ":" <> (pack . show $ port)

newEnv :: MonadIO m => Config -> m (Maybe Env)
newEnv c@Config{..} = do
  conn <- getDatabaseConnection c
  pure . Just $ Env
            conn
            configPort
            configHostname

deleteEnv :: MonadIO m => Env -> m ()
deleteEnv Env {..} = liftIO $ destroyAllResources envConnectionPool
