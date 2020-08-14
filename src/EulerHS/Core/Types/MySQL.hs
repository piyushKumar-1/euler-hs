{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}

module EulerHS.Core.Types.MySQL
  (
    -- * Core MySQL
    -- ** Types
    MySQLConfig(..)
  , MySqlOption(..)
    -- ** Methods
  , createMySQLConn
  , closeMySQLConn
    -- ** Defaults
  , defaultMySQLConfig
  ) where

import Prelude
import Data.Word (Word16)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Database.MySQL.Base (MySQLConn, close, ConnectInfo (..), connect, defaultConnectInfoMB4)
import Data.ByteString.UTF8 (fromString)

data MySqlProtocol
  = TCP
  | Socket
  | Pipe
  | Memory
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MySqlOption
  = ConnectTimeout Word
  | Compress
  | NamedPipe
  -- | InitCommand ByteString       -- TODO
  | ReadDefaultFile FilePath
  -- | ReadDefaultGroup ByteString  -- TODO
  | CharsetDir FilePath
  | CharsetName String
  | LocalInFile Bool
  | Protocol MySqlProtocol
  -- | SharedMemoryBaseName ByteString  -- TODO
  | ReadTimeout Word
  | WriteTimeout Word
  -- | UseRemoteConnection
  -- | UseEmbeddedConnection
  -- | GuessConnection
  -- | ClientIP ByteString
  | SecureAuth Bool
  | ReportDataTruncation Bool
  | Reconnect Bool
  -- | SSLVerifyServerCert Bool
  | FoundRows
  | IgnoreSIGPIPE
  | IgnoreSpace
  | Interactive
  | LocalFiles
  | MultiResults
  | MultiStatements
  | NoSchema
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SSLInfo = SSLInfo
  { sslKey     :: FilePath
  , sslCert    :: FilePath
  , sslCA      :: FilePath
  , sslCAPath  :: FilePath
  , sslCiphers :: String
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MySQLConfig = MySQLConfig
  { connectHost     :: String
  , connectPort     :: Word16
  , connectUser     :: String
  , connectPassword :: String
  , connectDatabase :: String
  , connectOptions  :: [MySqlOption]
  , connectPath     :: FilePath
  , connectSSL      :: Maybe SSLInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultMySQLConfig :: MySQLConfig
defaultMySQLConfig = MySQLConfig {
  connectHost = "localhost",
  connectPort = 3306,
  connectUser = "root",
  connectPassword = "",
  connectDatabase = "test",
  connectOptions = [CharsetName "utf8"],
  connectPath = "",
  connectSSL = Nothing
  }

-- | Connect with the given config to the database.
createMySQLConn :: MySQLConfig -> IO MySQLConn
createMySQLConn conf = do
  let dbConf = ConnectInfo {
    ciHost = connectHost conf,
    ciPort = fromIntegral . connectPort $ conf,
    ciDatabase = fromString . connectDatabase $ conf,
    ciUser = fromString . connectUser $ conf,
    ciPassword = fromString . connectPassword $ conf,
    ciCharset = ciCharset defaultConnectInfoMB4
    }
  connect dbConf

-- | Close the given connection.
closeMySQLConn :: MySQLConn -> IO ()
closeMySQLConn = close
