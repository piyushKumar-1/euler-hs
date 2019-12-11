module Dashboard.Auth.Token
  ( AuthContext(..)
  , lookupToken
  ) where

import Universum hiding (get)

import Data.Aeson (Result(..), Value(..), fromJSON)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Database.Redis (get, runRedis)
import Web.JWT (claims, decodeAndVerifySignature, unClaimsMap, unregisteredClaims)

import Dashboard.Auth.Types (AuthContext(..), Token(..))

lookupToken :: AuthContext -> ByteString -> IO (Maybe Token)
lookupToken (AuthContext rConn secret) uuid = do
  -- First search for the JWT in Redis
  lookupValue  <- runRedis rConn (get uuid)
  let mJwt     = join . rightToMaybe $ lookupValue
      verified = decodeAndVerifySignature secret . decodeUtf8 =<< mJwt
      -- Next, pull out the non-JWT related key/value pairs from the token
      values   = unClaimsMap . unregisteredClaims . claims <$> verified
  -- Finally, try to decode the values as a Token
  return $ unpackToken =<< values
  where
    unpackToken v = case fromJSON (Object . HM.fromList . M.toList $ v) of
                      Success token -> Just token
                      Error _       -> Nothing
