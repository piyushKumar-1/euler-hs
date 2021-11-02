{-
{-#OPTIONS_GHC  -Wno-redundant-constraints #-}
{-#OPTIONS_GHC  -Wno-unused-imports #-}
-}
{-#OPTIONS_GHC -fclear-plugins            #-}
{-#OPTIONS_GHC -Wno-unused-top-binds      #-}
{-#OPTIONS_GHC -Wno-redundant-constraints #-}
{-#LANGUAGE DerivingStrategies            #-}
{-#LANGUAGE DerivingVia                   #-}
{-#LANGUAGE RankNTypes                    #-}
{-#LANGUAGE OverloadedStrings             #-}
{-#LANGUAGE QuantifiedConstraints         #-}
--{-#LANGUAGE IncoherentInstances   #-}

module EulerHS.Extra.Secret
  (
    -- * some re-exports
    Given
  , give
    -- * main stuff
  , Secret -- do not export Secret's data Constructor for security reasons
  , SecretContext(..)
    -- construction
  , makeSecret
  , toBuilder
    -- extracting
  , unsafeExtractSecret -- prefer extractSecret
  , ExtractSecrets
  , extractSecret
  , elimExtractSecrets
    -- sequence-like transformations
  , sequenceSecretMaybe
  , sequenceMaybeSecret
  , sequenceSecretEither
  , sequenceEitherSecret
    -- servant stuff
  , ProtectedJSON
  , DiscloseSecret (..)
  ) where

import Data.Aeson hiding (object)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as AE
import Data.ByteString.Builder (Builder, string8)
import Data.Functor.Contravariant (contramap)
import Data.Functor.Identity (Identity (Identity))
import qualified Data.HashMap.Strict as HM
import Data.Kind (Type)
import Data.Reflection (Given, give, given)
import Data.Store (Store (..))
import Data.String (IsString)
import Network.HTTP.Media ((//))
import Prelude hiding (print)
import Servant.API (Accept(..), MimeRender(..))
import Test.QuickCheck (Arbitrary, Arbitrary1, arbitrary, arbitrary1, liftArbitrary)
import Unsafe.Coerce (unsafeCoerce)
import Web.FormUrlEncoded
import Web.HttpApiData

-- Secret context to annotate risky/safely usage of Secret type
data SecretContext = RiskyShowSecrets | SafelyHideSecrets

-- Secret data type used to hide any sensitive data stored in fields

-- We refused use the Traversable instance intentionally.
-- It also implies Foldable instance leading to extraction data without 'unsafeExtractSecret'.
-- Eg. 'traverse print (Secret password)' or
-- sneaky :: Show a => Secret a -> String
-- sneaky = foldMap show
newtype Secret (a :: Type) = Secret a
  deriving (Eq, Ord) via a
  deriving (Functor, Applicative, Monad) via Identity

instance Arbitrary a => Arbitrary (Secret a) where
  arbitrary = arbitrary1

instance Arbitrary1 Secret where
  liftArbitrary = fmap Secret

instance (Show a, Given SecretContext) => Show (Secret a) where
  show (Secret value) = case given of
    SafelyHideSecrets -> "***secret***"
    RiskyShowSecrets  -> show value

instance (ToJSON a, Given SecretContext) => ToJSON (Secret a) where
  toJSON (Secret value) = case given of
    SafelyHideSecrets -> A.String "***secret***"
    RiskyShowSecrets  -> toJSON value

  toEncoding (Secret value) = case given of
    SafelyHideSecrets -> AE.string "***secret***"
    RiskyShowSecrets  -> toEncoding value

instance FromJSON a => FromJSON (Secret a) where
  parseJSON (String "***secret***") = Prelude.error "FromJSON (Secret a): trying to decode secret data"
  parseJSON value = makeSecret <$> parseJSON value

instance FromForm a => FromForm (Secret a) where
  fromForm form = if HM.null $ HM.filter (elem "***secret***") $ unForm form
    then makeSecret <$> fromForm form
    else Left "FromForm (Secret a): trying to decode secret data"

instance FromHttpApiData a => FromHttpApiData (Secret a) where
  parseUrlPiece "***secret***" = Left "FromHttpApiData (Secret a): trying to decode secret data"
  parseUrlPiece val = makeSecret <$> parseUrlPiece val

-- SecretContext here used just to annotate unwrapping of Secret values with Store operations.
instance (Store a, Given SecretContext) => Store (Secret a) where
    size = case given of
      SafelyHideSecrets -> Prelude.error "Store.size (Secret a): trying to size secret data"
      RiskyShowSecrets  -> contramap (\(Secret x) -> x) size
    poke (Secret x) = case given of
      SafelyHideSecrets -> Prelude.error "Store.poke (Secret a): trying to poke secret data"
      RiskyShowSecrets  -> poke x
    peek = Secret <$> peek

makeSecret :: a -> Secret a
makeSecret = Secret

-- unsafeExtractSecret used to extract sensitive data as a last resort.
-- Avoid to use it as as long as possible! Better add help function here!
-- it is NOT for show or logs!
-- Please, annotate all uses of `unsafeExtractSecre` with a comment that says on of
-- 'DB conversion',
-- 'API type conversion for response' or
-- 'API type conversion for service'

-- For changing Secret data use Secret monad without extraction.
-- f :: Secret Int -> Secret String
-- f secret = do
--   x <- secret
--   pure $ show $ x + 1

-- Edsko de Vries suggested to add a constrain to unsafeExtractSecret.
-- It will additionaly marks all functions where sensitive data extraction occurs.
-- Example https://well-typed.com/blog/2015/07/checked-exceptions/
unsafeExtractSecret :: Secret a -> a
unsafeExtractSecret (Secret a) = a

toBuilder :: Given SecretContext => Secret a -> (a -> Builder) -> Builder
toBuilder (Secret b) f = case given of
  SafelyHideSecrets -> string8 "***secret***"
  RiskyShowSecrets  -> f b

sequenceSecretMaybe :: Secret (Maybe a) -> Maybe (Secret a)
sequenceSecretMaybe (Secret (Just a)) = Just $ makeSecret a
sequenceSecretMaybe _                 = Nothing

sequenceMaybeSecret :: Maybe (Secret a) -> Secret (Maybe a)
sequenceMaybeSecret (Just (Secret a)) = makeSecret $ Just a
sequenceMaybeSecret _                 = makeSecret Nothing

sequenceSecretEither :: (Given SecretContext, IsString e) => Secret (Either e a) -> Either e (Secret a)
sequenceSecretEither (Secret (Right a)) = Right $ makeSecret a
sequenceSecretEither (Secret (Left e)) = case given of
  SafelyHideSecrets -> Left "***secret***"
  RiskyShowSecrets  -> Left e

sequenceEitherSecret :: Either e (Secret a) -> Secret (Either e a)
sequenceEitherSecret (Right (Secret a)) = makeSecret $ Right a
sequenceEitherSecret (Left e)           = makeSecret $ Left e

---

class ExtractSecrets

extractSecret :: ExtractSecrets => Secret a -> a
extractSecret (Secret s) = s

newtype Wrap a = Wrap { unWrap :: ExtractSecrets => a}

elimExtractSecrets :: (ExtractSecrets => a) -> a
elimExtractSecrets a = unsafeCoerce {-@(Wrap a) @(() -> a)-} (Wrap a) ()

-- Some servant stuff

-- We need a promoted type constructor to use instead of the regular JSON kind
data ProtectedJSON

instance Accept ProtectedJSON where
  contentType _ = "application" // "json"

newtype DiscloseSecret a = DiscloseSecret a

instance (Given SecretContext => ToJSON a) => MimeRender ProtectedJSON (DiscloseSecret a) where
  mimeRender _ (DiscloseSecret v) = give RiskyShowSecrets $ encode v










-- leave here to test example:
-- import           Data.Text (Text, pack)
-- import           Euler.Types.Utils.JSONUtils (eitherDecodeT)
-- import Euler.Types.Utils.JSONUtils (encodeT)
-- import           GHC.Generics
-- import           Data.Reflection (give) -- leave here to test example
-- import qualified Data.Store as Binary -- leave here to test example


{-
test :: Secret a -> a
test s = extracter $ unsafeExtractSecretConstraint s
-}

{-   Usage of JSON, FromForm and Show instancies of Secret
data X = X {name :: Secret String, b :: Y}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

deriving instance Given SecretContext => ToJSON X
deriving instance Given SecretContext => Show X
deriving instance Given SecretContext => Store X

data Y = Y {c :: Int, money :: Secret Int}
  deriving stock (Generic)
  deriving anyclass (FromJSON, FromForm)

deriving instance Given SecretContext => ToJSON Y
deriving instance Given SecretContext => Show Y
deriving instance Given SecretContext => Store Y

exampleX :: X
exampleX = X { name = makeSecret "a", b = exampleY }

exampleY :: Y
exampleY = Y {c = 10, money = makeSecret 20}

main :: IO ()
main = do
  let secretJson = A.decode @X $ give SafelyHideSecrets $ A.encode exampleX
  -- print $ give RiskyShowSecrets $ show secretJson
  print $ give SafelyHideSecrets (A.encode exampleX)
  print $ give RiskyShowSecrets (A.encode exampleX)
  print $ give SafelyHideSecrets (show exampleX :: String)
  print $ give RiskyShowSecrets (show exampleX :: String)
  P.putStrLn $ give RiskyShowSecrets $ show (urlDecodeAsForm @Y "money=***secret***&c=10")
  P.putStrLn $ give RiskyShowSecrets $ show (urlDecodeAsForm @Y "money=20&c=10")
  P.putStrLn $ give SafelyHideSecrets $ show (urlDecodeAsForm @Y "money=20&c=10")
  P.putStrLn "Store examples: "
  let binRisk = give RiskyShowSecrets $ Binary.encode exampleX
  P.putStrLn $ give RiskyShowSecrets $ show (Binary.decode @X binRisk)
  P.putStrLn $ give SafelyHideSecrets $ show (Binary.decode @X binRisk)
  let binSafe = give SafelyHideSecrets $ Binary.encode exampleX
  P.putStrLn $ give RiskyShowSecrets $ show (Binary.decode @X binSafe)
  P.putStrLn $ give SafelyHideSecrets $ show (Binary.decode @X binSafe)

-- "*** Exception: FromJSON (Secret a): trying to decode secret data
-- "{\"name\":\"***secret***\",\"b\":{\"money\":\"***secret***\",\"c\":10}}"
-- "{\"name\":\"a\",\"b\":{\"money\":20,\"c\":10}}"
-- "X {name = ***secret***, b = Y {c = 10, money = ***secret***}}"
-- "X {name = \"a\", b = Y {c = 10, money = 20}}"
-- Left "FromHttpApiData (Secret a): trying to decode secret data"
-- Right (Y {c = 10, money = 20})
-- Right (Y {c = 10, money = ***secret***})
-- Store examples:
-- Right (X {name = "a", b = Y {c = 10, money = 20}})
-- Right (X {name = ***secret***, b = Y {c = 10, money = ***secret***}})
-- Right (X {name = "a", b = Y {c = 10, money = 20}})
-- *** Exception: Store.size (Secret a): trying to size secret data
-- *** Exception: Store.size (Secret a): trying to size secret data
-}
