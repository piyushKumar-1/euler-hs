{-# OPTIONS_GHC -Wno-deprecations #-}

module Euler.Encryption
  ( Key (..)
  , EncryptionError (..)
  , decryptEcb
  , encryptEcb
  , decryptRSAOAEP
  , decryptRSAOAEP'
  , encryptRSAOAEP
  , parseRSAPubKey
  , readRSAPubKeyFile
  , readRSAPrivateKeyFile
  , sha256hash
  , sha256hashlazy
  , sha512hash
  , sha512hashlazy
  , hmac256
  , digestToTextWithBase
  , digestToBase16Text
  , digestToBase64Text
  , hashPassword
  , signRSASignature
  , verifyRSASignature
  , verifyRSASignaturePKCS15
  , RSA.generateBlinder
  , AES.AES128
  , AES.AES256
  , module HMAC
  , encryptKMS
  , encryptKMS'
  , decryptKMS
  , decryptKMS'
  ) where

import EulerHS.Prelude hiding (Key, keys)
import EulerHS.Language
import WebService.Language

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Config.Config                  as Config

import           Basement.Block (Block (..))
import           Crypto.Cipher.Types (BlockCipher (..), Cipher (..))
import           Crypto.Error (CryptoError (..), CryptoFailable (..))
import           Crypto.Hash (Context, Digest, hash, hashFinalize, hashInit, hashUpdate, hashlazy)
import           Crypto.Hash.Algorithms (HashAlgorithm, SHA1 (..), SHA256 (..), SHA512)
import           Crypto.MAC.HMAC (HMAC (..), hmac, hmacGetDigest)
import           Crypto.Random (MonadRandom)
import           Data.ByteArray (ByteArray, ByteArrayAccess)
import           Data.ByteArray.Encoding (Base (..), convertToBase)
import           Unsafe.Coerce

import qualified Control.Monad.Trans.AWS as AWS
import qualified Crypto.Cipher.AES as AES (AES128, AES256)
import qualified Crypto.Data.Padding as PAD
import qualified Crypto.MAC.HMAC as HMAC (HMAC (..))
import qualified Crypto.PubKey.RSA as RSA (Blinder, Error, PrivateKey (..), PublicKey (..),
                                           generateBlinder)
import qualified Crypto.PubKey.RSA.OAEP as OAEP
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import qualified Crypto.PubKey.RSA.PSS as PSS
import qualified Crypto.Store.X509 as CStore (readPubKeyFile, readPubKeyFileFromMemory)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as Text (decodeUtf8)
import qualified Data.X509 as CStore (PrivKey (..), PubKey (..))
import qualified Data.X509.File as CStore (readKeyFile)
import qualified Network.AWS.KMS.Decrypt as AWS
import qualified Network.AWS.KMS.Encrypt as AWS



data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

data EncryptionError = CryptoErr CryptoError | PaddingErr Text
  deriving (Show,Eq)

instance Exception EncryptionError

initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either EncryptionError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left (CryptoErr e)
  CryptoPassed a -> Right a

decryptEcb :: forall c a. (BlockCipher c, ByteArray a) => Key c a -> a -> Either EncryptionError a
decryptEcb secretKey msg =
  case initCipher secretKey of
    Left e -> Left  e
    Right c -> maybeToRight (PaddingErr $ "Can't unpad") $ PAD.unpad (PAD.PKCS7 bsize) $ ecbDecrypt c msg
  where
    bsize = blockSize @c undefined

encryptEcb :: forall c a. (BlockCipher c, ByteArray a) => Key c a -> a -> Either EncryptionError a
encryptEcb secretKey msg =
  case initCipher secretKey of
    Left e  -> Left e
    Right c -> Right $ ecbEncrypt c $ PAD.pad (PAD.PKCS7 bsize) msg
  where
    bsize = blockSize @c undefined

decryptRSAOAEP' :: HashAlgorithm hash
        => Maybe RSA.Blinder
        -> OAEP.OAEPParams hash ByteString ByteString
        -> RSA.PrivateKey
        -> ByteString
        -> Either RSA.Error ByteString
decryptRSAOAEP' = OAEP.decrypt

decryptRSAOAEP :: RSA.PrivateKey -> ByteString -> Either RSA.Error ByteString
decryptRSAOAEP = OAEP.decrypt Nothing (OAEP.defaultOAEPParams SHA1)

encryptRSAOAEP :: (MonadRandom m)
        => RSA.PublicKey
        -> ByteString
        -> m (Either RSA.Error ByteString)
encryptRSAOAEP = OAEP.encrypt (OAEP.defaultOAEPParams SHA1)

readRSAPubKeyFile :: FilePath -> IO (Either Text RSA.PublicKey)
readRSAPubKeyFile fp = do
  keys <- CStore.readPubKeyFile fp
  case keys of
    (CStore.PubKeyRSA pubKey:_) -> pure $ Right pubKey
    _                           -> pure $ Left "Unrecognized key format"

parseRSAPubKey :: ByteString -> Either Text RSA.PublicKey
parseRSAPubKey bs = case CStore.readPubKeyFileFromMemory bs of
  (CStore.PubKeyRSA pubKey:_) -> Right pubKey
  _                           -> Left "Unrecognized key format"

readRSAPrivateKeyFile :: FilePath -> IO (Either Text RSA.PrivateKey)
readRSAPrivateKeyFile fp = do
  keys <- CStore.readKeyFile fp
  case keys of
    (CStore.PrivKeyRSA privKey:_) -> pure $ Right privKey
    _                             -> pure $ Left "Unrecognized key format"

sha256hash :: (ByteArrayAccess ba) => ba -> Digest SHA256
sha256hash = hash

sha256hashlazy :: BSL.ByteString -> Digest SHA256
sha256hashlazy = hashlazy

sha512hash :: (ByteArrayAccess ba) => ba -> Digest SHA512
sha512hash = hash

sha512hashlazy :: BSL.ByteString -> Digest SHA512
sha512hashlazy = hashlazy

hmac256 :: (ByteArrayAccess key, ByteArrayAccess message)
  => key
  -> message
  -> HMAC SHA256
hmac256 = hmac

-- newtype Digest a = Digest (Block Word8)
digestToTextWithBase :: Base -> Digest a -> Text
digestToTextWithBase b d = digestToTextWithBase' b (unsafeCoerce d)

digestToTextWithBase' :: Base -> Block Word8 -> Text
digestToTextWithBase' base bs = Text.decodeUtf8 $ convertToBase base bs

digestToBase16Text :: Digest a -> Text
digestToBase16Text = digestToTextWithBase Base16

digestToBase64Text :: Digest a -> Text
digestToBase64Text = digestToTextWithBase Base64

hashPassword :: (ByteArrayAccess p, ByteArrayAccess s) => p -> s -> Digest SHA256
hashPassword password salt =
  let (ihash :: Context SHA256) = hashInit
      phash = hashUpdate ihash password
      fhash = hashUpdate phash salt
  in hashFinalize fhash

-- | Verify signature using PSS scheme and SHA256 hash function
verifyRSASignature
  :: ByteString -- ^ payload.
  -> ByteString -- ^ signature
  -> RSA.PublicKey
  -> Bool
verifyRSASignature payload sign pubkey = PSS.verify (PSS.defaultPSSParams SHA256) pubkey payload sign

-- | Verify signature using PKCS15 scheme and SHA256 hash function
verifyRSASignaturePKCS15
  :: ByteString -- ^ payload.
  -> ByteString -- ^ signature
  -> RSA.PublicKey
  -> Bool
verifyRSASignaturePKCS15 payload sign pubkey = PKCS15.verify (Just SHA256) pubkey payload sign

signRSASignature :: (MonadRandom m) => RSA.PrivateKey -> ByteString -> m (Either RSA.Error ByteString)
signRSASignature pk bs = PSS.sign Nothing (PSS.defaultPSSParams SHA256) pk bs

-- | Amazon KMS encryption
encryptKMS' :: ByteString -> IO (Either Text (Maybe ByteString))
encryptKMS' str = fmap (bimap show id) $ try @_ @SomeException $ do
  env <- AWS.newEnv AWS.Discover <&> AWS.envRegion .~ Config.awsRegion
  AWS.runResourceT . AWS.runAWST env $ do
    encResp <- AWS.send $ AWS.encrypt Config.kmsKeyId $ str
    pure $ encResp ^. AWS.ersCiphertextBlob

encryptKMS :: ByteString -> Flow ByteString
encryptKMS value = do
  eitherEncValue <- runIO' "encryptKMS" $! encryptKMS' value
  case eitherEncValue of
    Right (Just v) -> pure v
    Right Nothing -> do
      logErrorT "encryptKMS" "ersCiphertextBlob returned as Nothing"
      throwException Errs.internalError
    Left err -> do
      logErrorT "encryptKMS" err
      throwException Errs.internalError

-- | Amazon KMS decryption
decryptKMS' :: ByteString -> IO (Either Text (Maybe ByteString))
decryptKMS' value = fmap (bimap show id) $ try @_ @SomeException $ do
    env <- AWS.newEnv AWS.Discover <&> AWS.envRegion .~ Config.awsRegion
    AWS.runResourceT . AWS.runAWST env $ do
      decResp <- AWS.send $ AWS.decrypt value
      pure $ decResp ^. AWS.drsPlaintext

decryptKMS :: ByteString -> Flow ByteString
decryptKMS value = do
  eitherDecValue <- runIO' "decryptKMS" $! decryptKMS' value
  case eitherDecValue of
    Right (Just v) -> pure v
    Right Nothing -> do
      logErrorT "decryptKMS" "drsPlaintext returned as Nothing"
      throwException Errs.internalError
    Left err -> do
      logErrorT "decryptKMS" err
      throwException Errs.internalError
