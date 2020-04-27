module Euler.Tests.Encryption.EncryptionSpec where

import           EulerHS.Prelude hiding (Key)

import           Euler.Encryption
import           GHC.Generics
import           Test.Hspec

import qualified Crypto.PubKey.RSA      as RSA (PrivateKey(..), PublicKey(..))
import qualified Data.Aeson             as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Char8  as BC

data P = P
    { dimension :: String
    , value :: String
    } deriving (Eq, Show, Generic)

instance FromJSON P
instance ToJSON P

testP :: P
testP = P
  { dimension = "someDimension"
  , value = "someValue"
  }

decodedP :: P
decodedP  = P
  { dimension = "card_number"
  , value = "5264190200963394"
  }

testJsonedP :: ByteString
testJsonedP = BSL.toStrict $ A.encode testP

testAES256KeyInBase16 :: ByteString
testAES256KeyInBase16 = "bd3222130d110b9684dac9cc0903ce111b25e97ae93ddc2925f89c4ed6e41bab"

testAESKey :: Key AES256 ByteString
testAESKey = Key $ fst $ B16.decode testAES256KeyInBase16

encodedAesMsgInBase64 :: ByteString
encodedAesMsgInBase64 = "gss8Mo6p6VNqkfqTddJUktxGHZQDOfkI6JuP6WZvhAWOAkDUM7+PCtYj1L68fRh2bXwsTnOwghrKuPHnFF8/Yc/edlr5opppUblDgvR4izA="

aesMsgBS :: ByteString
aesMsgBS = "\n[\n  {\n    \"dimension\": \"card_number\",\n    \"value\": \"5264190200963394\"\n  }\n]"

encodedAesMsg :: Either String ByteString
encodedAesMsg = B64.decode encodedAesMsgInBase64

testByteStringMessageSha256Hash :: Text
testByteStringMessageSha256Hash = "597960812d4d0d4d87c649a3c4ac01e6d46a9cbfb715773367c924f787318f5c"

testByteStringMessageSha512Hash :: Text
testByteStringMessageSha512Hash = "e2c9e1e65e031f1651e43e2028c96addcb3bfe3f726211bc1c46017c6385bd12375f6051d9eb1c6ab4ecdeb01c5871e692a521e4225c18ca823225df40a03dfe"

testStrictByteStringMessage :: ByteString
testStrictByteStringMessage = "TestByteStringMessage"

testLazyByteStringMessage :: BSL.ByteString
testLazyByteStringMessage = BSL.fromStrict testStrictByteStringMessage

testPubKey = RSA.PublicKey
  { public_size = 256
  , public_n = 24415584346264653648670200571242258474773521651776592471018551191716744974578213870840374056403453432702887308424782095404455364926322788787347979558434437654166137347045144457521060001673228715516141993373950646321692703210433076229344293185509126648101003313398368526938207077405528170615132025948492866387129938478721931628329722571082394786521550789887740173950414695602744298136739426339326151087213505473251011801365989333253348003729456456288906702241187311281431590620448383817351785467172370622196206993252209353340464779617808513994120530856359499188210766115894727319791024642274028885550302607524882968847
  , public_e = 65537
  }

testPrivKey = RSA.PrivateKey
  { private_pub = testPubKey
  , private_d = 21676621987387412798081627481235497647965231202353190303587506310786865531620815326835942512959624913893182447177868169447918510280270556564869343891351568654464755810272391630084728873725633044117166209690149356183952422849673444625240668614658109678803681550728353033246861754411106040972907776215161777785326400280891024046576813419008065243871113300186749690662988996852504453988842194167987451571382294233048646146484441513522235596901097390609622987166448553007367606588370365432946825154842082440045083506325481607066435491897719643602241937752043206999605256029065675678805746575910829097195311354210448698753
  , private_p = 166371864241333208668347647978212590080442010070725772740694300539504813525374217730392031714423445446757861457405426749659656378912186986062077877683350082041932714542394667275149738749251935087034485055369337004532933083807795092972245720225160273604638224031739082572576103422095629034812100348244011610433
  , private_q = 146753085070010760634054805521788591773562980137732550612825836556363410003360553127497432707263181670365125552310638316657079880998291732177797945294667360399697366531228441310114118898501589494545847782102979110304257951159315607448614866067210987083389854718393383219984669207710100348411329006365340270159
  , private_dP = 87589124188148979945435386090182171834925167057849021726233661161092735112470614070703820288428706505477615604389267728817418008782934641196574042353306191627520399924565408929223056228778240021788468160968738798959363263356887774735209028257758287992749632173689573309757744424867868358150691949821675276481
  , private_dQ = 35418062263337659657122615605507273083638946806208946900881429678074981400173243645843216704011195881413021512441786567199673962460136721666481988811293675930268600125493084154021011316013849291777647349886671965266680623365227199338003590896517634049437376323005754648374162882926448528477379051431719289151
  , private_qinv = 128637609917657173237901166692196053666427667875330370597848791087918643082927222350592315940892091208854571573142949444760950568496520012268064505454596141157261465625080858661855061489543924461620916454665167474136020521849652055356692824007800280066158668648501971062154293306328956416018980790247450690117
  }

spec :: Spec
spec =
  describe "Euler encryption tests" $ do

    it "SHA256 hash" $  do
      let hashFromStrict = digestToBase16Text $ sha256hash testStrictByteStringMessage
      hashFromStrict `shouldBe` testByteStringMessageSha256Hash

    it "SHA256 hash lazy" $  do
      let hashFromLazy = digestToBase16Text $ sha256hashlazy testLazyByteStringMessage
      hashFromLazy `shouldBe` testByteStringMessageSha256Hash

    it "SHA512 hash" $  do
      let hashFromStrict = digestToBase16Text $ sha512hash testStrictByteStringMessage
      hashFromStrict `shouldBe` testByteStringMessageSha512Hash

    it "SHA512 hash lazy" $  do
      let hashFromLazy = digestToBase16Text $ sha512hashlazy testLazyByteStringMessage
      hashFromLazy `shouldBe` testByteStringMessageSha512Hash

    it "readRSAPubKeyFile" $ do
      pubKey <- readRSAPubKeyFile "./test/Euler/TestData/Keys/pub.pem"
      pubKey `shouldBe` Right testPubKey

    it "readRSAPrivKeyFile" $ do
      privKey <- readRSAPrivateKeyFile "./test/Euler/TestData/Keys/pk.pem"
      privKey `shouldBe` Right testPrivKey

    it "Encrypt/Decrypt RSA OAEP" $ do
      encbs1 <- encryptRSAOAEP testPubKey testStrictByteStringMessage
      encbs2 <- encryptRSAOAEP testPubKey testStrictByteStringMessage
      let decbs1 = decryptRSAOAEP testPrivKey =<< encbs1
      let decbs2 = decryptRSAOAEP testPrivKey =<< encbs2
      isRight encbs1 `shouldBe` True
      isRight decbs1 `shouldBe` True
      isRight encbs2 `shouldBe` True
      isRight decbs2 `shouldBe` True
      encbs1 `shouldNotBe` encbs2
      decbs1 `shouldBe` Right testStrictByteStringMessage
      decbs2 `shouldBe` Right testStrictByteStringMessage

--    it "hmac256" $ do
--      pure ()

--    it "hashPassword" $ do
--      pure ()

    it "decryptEcb" $ do
      let dmsg = case encodedAesMsg of
            Right emsg ->  decryptEcb testAESKey emsg
            Left err ->  Right "wrong msg"
      let p = A.decode . BSL.fromStrict <$> dmsg
      dmsg `shouldBe` Right aesMsgBS
      p `shouldBe` Right (Just [decodedP])

    it "encryptEcb" $ do
      let emsg = B64.encode <$> encryptEcb testAESKey aesMsgBS
      emsg `shouldBe` Right encodedAesMsgInBase64

    it "RSA sign/verify" $ do
      sig <- signRSASignature testPrivKey "Hello!"
      let isVerified = case sig of
            Left e -> Left e
            Right s -> Right $ verifyRSASignature "Hello!" s testPubKey
      isVerified `shouldBe` Right True

    it "hmac" $ do
      let hm = hmac256 ("aed123" :: ByteString) ("Hello" :: ByteString)
      (digestToBase16Text $ hmacGetDigest hm) `shouldBe` "066b7ccfe17ed25ba8d65a2e28ad24974796b40579d7b98cb4f9271ab42d158a"

    it "password hash" $ do
      let h = hashPassword ("password" :: ByteString) ("salt" :: ByteString)
      digestToBase16Text h `shouldBe` "7a37b85c8918eac19a9089c0fa5a2ab4dce3f90528dcdeec108b23ddf3607b99"
