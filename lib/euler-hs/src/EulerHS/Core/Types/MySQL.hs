{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module EulerHS.Core.Types.MySQL where

import           EulerHS.Prelude

import qualified Database.Beam             as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Sqlite      as BS
import qualified Database.MySQL.Base       as MySQL
import qualified Data.Pool as DP
import           Data.Time.Clock (NominalDiffTime)

type Seconds = MySQL.Seconds

data MySqlProtocol
  = TCP
  | Socket
  | Pipe
  | Memory
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data MySqlOption
  = ConnectTimeout Seconds
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
  | ReadTimeout MySQL.Seconds
  | WriteTimeout MySQL.Seconds
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
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SSLInfo = SSLInfo
  { sslKey     :: FilePath
  , sslCert    :: FilePath
  , sslCA      :: FilePath
  , sslCAPath  :: FilePath
  , sslCiphers :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

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
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data MySQLPoolConfig = MySQLPoolConfig
  { mySqlConfig :: MySQLConfig
  , stripes :: Int
  , keepAlive :: NominalDiffTime
  , resourcesPerStripe :: Int
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

createMySQLConn :: MySQLConfig -> IO MySQL.Connection
createMySQLConn = MySQL.connect . toMySQLConnectInfo

closeMySQLConn :: MySQL.Connection -> IO ()
closeMySQLConn = MySQL.close

createMySQLConnPool ::  MySQLPoolConfig -> IO (DP.Pool MySQL.Connection)
createMySQLConnPool MySQLPoolConfig{..} 
  = DP.createPool (createMySQLConn mySqlConfig) closeMySQLConn stripes keepAlive resourcesPerStripe


toMySQLProtocol :: MySqlProtocol -> MySQL.Protocol
toMySQLProtocol TCP    = MySQL.TCP
toMySQLProtocol Socket = MySQL.Socket
toMySQLProtocol Pipe   = MySQL.Pipe
toMySQLProtocol Memory = MySQL.Memory

toMySQLOption :: MySqlOption -> MySQL.Option
toMySQLOption (ConnectTimeout s)       = MySQL.ConnectTimeout s
toMySQLOption Compress                 = MySQL.Compress
toMySQLOption NamedPipe                = MySQL.NamedPipe
-- toMySQLOption (InitCommand s)          = MySQL.InitCommand s     -- TODO
toMySQLOption (ReadDefaultFile s)      = MySQL.ReadDefaultFile s
-- toMySQLOption (ReadDefaultGroup s)     = MySQL.ReadDefaultGroup s    -- TODO
toMySQLOption (CharsetDir s)           = MySQL.CharsetDir s
toMySQLOption (CharsetName s)          = MySQL.CharsetName s
toMySQLOption (LocalInFile s)          = MySQL.LocalInFile s
toMySQLOption (Protocol s)             = MySQL.Protocol $ toMySQLProtocol s
-- toMySQLOption (SharedMemoryBaseName s) = MySQL.SharedMemoryBaseName s    -- TODO
toMySQLOption (ReadTimeout s)          = MySQL.ReadTimeout s
toMySQLOption (WriteTimeout s)         = MySQL.WriteTimeout s
toMySQLOption (SecureAuth s)           = MySQL.SecureAuth s
toMySQLOption (ReportDataTruncation s) = MySQL.ReportDataTruncation s
toMySQLOption (Reconnect s)            = MySQL.Reconnect s
toMySQLOption FoundRows                = MySQL.FoundRows
toMySQLOption IgnoreSIGPIPE            = MySQL.IgnoreSIGPIPE
toMySQLOption IgnoreSpace              = MySQL.IgnoreSpace
toMySQLOption Interactive              = MySQL.Interactive
toMySQLOption LocalFiles               = MySQL.LocalFiles
toMySQLOption MultiResults             = MySQL.MultiResults
toMySQLOption MultiStatements          = MySQL.MultiStatements
toMySQLOption NoSchema                 = MySQL.NoSchema

toMySQLSslInfo :: SSLInfo -> MySQL.SSLInfo
toMySQLSslInfo SSLInfo {..} = MySQL.SSLInfo {..}

toMySQLConnectInfo :: MySQLConfig -> MySQL.ConnectInfo
toMySQLConnectInfo MySQLConfig {..} = MySQL.ConnectInfo
  { MySQL.connectHost     = connectHost
  , MySQL.connectPort     = connectPort
  , MySQL.connectUser     = connectUser
  , MySQL.connectPassword = connectPassword
  , MySQL.connectDatabase = connectDatabase
  , MySQL.connectOptions  = map toMySQLOption connectOptions
  , MySQL.connectPath     = connectPath
  , MySQL.connectSSL      = toMySQLSslInfo <$> connectSSL
  }


defaultMySQLConfig :: MySQLConfig
defaultMySQLConfig = MySQLConfig
  { connectHost     = "localhost"
  , connectPort     = 3306
  , connectUser     = "root"
  , connectPassword = ""
  , connectDatabase = "test"
  , connectOptions  = [CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }
