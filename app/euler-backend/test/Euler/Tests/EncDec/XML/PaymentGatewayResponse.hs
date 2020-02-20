{-# LANGUAGE QuasiQuotes #-}

module Euler.Tests.EncDec.XML.PaymentGatewayResponse
  ( spec
  )
  where

import EulerHS.Prelude

import           Data.Coerce
import           Test.Hspec
import           Text.RawString.QQ
import           Xmlbf
import           Xmlbf.Xeno

import qualified Data.Binary.Builder   as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Map.Strict       as Map
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T

import Euler.Common.Types.PaymentGatewayResponseXml


spec :: Spec
spec =
  describe "PaymentGatewayResponseXml encoding decoding" $ do

    it "LHS (linked-hash-map) decoding from raw bytestring" $  do
      let res = decodePGRXml $ BC.pack rawLinkedHashMapXML
      res `shouldBe` (Right testLHM)

    it "LHS (linked-hash-map) find entry: SUCCESS" $  do
      let res = findEntry "SomeTextEntry" "" (decodePGRXml $ BC.pack rawLinkedHashMapXML)
      res `shouldBe` "TextValue"

    it "LHS (linked-hash-map) find entry: FAIL" $  do
      let res = findEntry "SomeTextEntryWrong" "default" (decodePGRXml $ BC.pack rawLinkedHashMapXML)
      res `shouldBe` "default"

    it "LHS (linked-hash-map) encode/decode" $  do
      let nodes = toXml testLHM
      let lbsXml = BB.toLazyByteString $ encode nodes
      let decodedNodes = fromRawXml $ BSL.toStrict lbsXml
      let resFromNodes = runParser fromXml nodes
      let resFromDecodedNodes = runParser fromXml =<< decodedNodes
      resFromNodes `shouldBe` (Right testLHM)
      resFromDecodedNodes `shouldBe` (Right testLHM)

    it "LHS (linked-hash-map) with complex string value" $ do
      let res = decodePGRXml $ BC.pack rawLinkedHashMapXML2
      res `shouldBe` (Right $ PGRLhm $ LHM $ Map.fromList $ coerce
        [Entry ("soap:Envelope", EText (TLE.decodeUtf8 $ BSL.fromStrict $ BC.pack rawXML2TextValue))])

    it "groovy hash map from raw string" $ do
      let res = decodePGRXml $ BC.pack groovyHMS
      res `shouldBe` (Right $ testGroovyHM)

    it "groovy hash map: find entry: SUCCESS" $  do
      let res = findEntry "checksum" "" (decodePGRXml $ BC.pack groovyHMS)
      res `shouldBe` "b43ac9dcfb502b3e8e0a5578e40e18d0db8dbf19d88f419146aa12ca1771d47a"

    it "groovy hash map: find entry: FAIL" $  do
      let res = findEntry "SomeTextEntryWrong" "default" (decodePGRXml $ BC.pack groovyHMS)
      res `shouldBe` "default"

    it "groovy hash map enc/dec" $ do
      let nodes = toXml testGroovyHM
      let lbsXml = BB.toLazyByteString $ encode nodes
      let decodedNodes = fromRawXml $ BSL.toStrict lbsXml
      let resFromNodes = runParser fromXml nodes
      let resFromDecodedNodes = runParser fromXml =<< decodedNodes
      resFromNodes `shouldBe` (Right testGroovyHM)
      resFromDecodedNodes `shouldBe` (Right testGroovyHM)

    it "PRGXml from raw string" $ do
      let groovyXML = decodePGRXml $ BC.pack groovyHMS
      let lhmXML = decodePGRXml $ BC.pack rawLinkedHashMapXML
      let mapXML = decodePGRXml $ BC.pack rawXmlMapXML
      groovyXML `shouldBe` (Right testGroovyHM)
      lhmXML `shouldBe` (Right testLHM)
      mapXML `shouldBe` (Right testXmlMap)

    it "XmlMap hash map: find entry: SUCCESS" $  do
      let res = findEntry "SomeTextEntry" "" (decodePGRXml $ BC.pack rawXmlMapXML)
      res `shouldBe` "TextValue"

    it "XmlMap hash map: find entry: FAIL" $  do
      let res = findEntry "SomeTextEntryWrong" "default" (decodePGRXml $ BC.pack rawXmlMapXML)
      res `shouldBe` "default"


testLHM :: PGRXml
testLHM = PGRLhm $ LHM $ Map.fromList $ coerce [ Entry ("SomeTextEntry", EText "TextValue")
              , Entry ("SomeBoolEntry", EBool True)
              , Entry ("SomeIntEntry", EInt 5)
              ]

rawLinkedHashMapXML :: String
rawLinkedHashMapXML = [r|<linked-hash-map>
  <entry>
    <string>SomeTextEntry</string>
    <string>TextValue</string>
  </entry>
  <entry>
    <string>SomeBoolEntry</string>
    <boolean>true</boolean>
  </entry>
  <entry>
    <string>SomeIntEntry</string>
    <int>5</int>
  </entry>
</linked-hash-map>|]

rawLinkedHashMapXML2 :: String
rawLinkedHashMapXML2 = [r|
<linked-hash-map>
  <entry>
    <string>soap:Envelope</string>
    <string>
      <xmlns:xsi>http://www.w3.org/2001/XMLSchema-instance</xmlns:xsi>
      <xmlns:xsd>http://www.w3.org/2001/XMLSchema</xmlns:xsd>
      <xmlns:soap>http://schemas.xmlsoap.org/soap/envelope/</xmlns:soap>
      <soap:Body>
        <AuthTransactionResponse>
          <xmlns>http://in.worldline.com/</xmlns>
          <AuthTransactionResult>
            <Responsecode>E3</Responsecode>
            <RequestID>bajaj_9069_65123</RequestID>
            <OrderNo>bajaj_9069_65</OrderNo>
            <Key>4962575618567464</Key>
            <Errordescription>Transaction Status : Failed [E3].  Reason : SCHEME NOT AVAILABLE.</Errordescription>
            <DEALID>914214224048</DEALID>
          </AuthTransactionResult>
        </AuthTransactionResponse>
      </soap:Body>
    </string>
  </entry>
</linked-hash-map>
|]

rawXML2TextValue :: String
rawXML2TextValue = [r|
      <xmlns:xsi>http://www.w3.org/2001/XMLSchema-instance</xmlns:xsi>
      <xmlns:xsd>http://www.w3.org/2001/XMLSchema</xmlns:xsd>
      <xmlns:soap>http://schemas.xmlsoap.org/soap/envelope/</xmlns:soap>
      <soap:Body>
        <AuthTransactionResponse>
          <xmlns>http://in.worldline.com/</xmlns>
          <AuthTransactionResult>
            <Responsecode>E3</Responsecode>
            <RequestID>bajaj_9069_65123</RequestID>
            <OrderNo>bajaj_9069_65</OrderNo>
            <Key>4962575618567464</Key>
            <Errordescription>Transaction Status : Failed [E3].  Reason : SCHEME NOT AVAILABLE.</Errordescription>
            <DEALID>914214224048</DEALID>
          </AuthTransactionResult>
        </AuthTransactionResponse>
      </soap:Body>
    |]

<<<<<<< HEAD
testGroovyHM :: PGRXml
testGroovyHM = PGRGhm $ GroovyHM (Map.fromList
=======
testGroovyHM :: GroovyHM
testGroovyHM = GroovyHM (Map.fromList
>>>>>>> aded to/from xml instances for PaymentGatewayResponseXml
  [("checksum",EText "b43ac9dcfb502b3e8e0a5578e40e18d0db8dbf19d88f419146aa12ca1771d47a")
  ,("customResponse",EText "{}")
  ,("payload",EGroovyHM (GroovyHM (Map.fromList [("amount",EText "1.00")
                                            ,("anotherHM",EGroovyHM (GroovyHM (Map.fromList [("deep string",EText "deep string value")])))
                                            ,("gatewayResponseCode",EText "ZA")
                                            ,("gatewayResponseMessage",EText "TRANSACTION HAS DECLINED")
                                            ,("gatewayTransactionId",EText "AXISJUSPAYc4b572555c954b04abc3835fc")
                                            ,("merchantChannelId",EText "JPMERCHANT")
                                            ,("merchantId",EText "JPMERCHANT")
                                            ,("merchantRequestId",EText "aupi28")
                                            ,("transactionTimestamp",EText "2017-05-15T15:11:06+00:00")
                                            ])))
  ,("responseCode",EText "SUCCESS")
  ,("responseMessage",EText "SUCCESS")
  ,("udfParameters",EText "{}")])

groovyHMS :: String
groovyHMS = [r|<org.codehaus.groovy.grails.web.json.JSONObject>
  <myHashMap>
    <entry>
      <string>responseMessage</string>
      <string>SUCCESS</string>
    </entry>
    <entry>
      <string>responseCode</string>
      <string>SUCCESS</string>
    </entry>
    <entry>
      <string>udfParameters</string>
      <string>{}</string>
    </entry>
    <entry>
      <string>payload</string>
      <org.codehaus.groovy.grails.web.json.JSONObject>
        <myHashMap>
          <entry>
            <string>anotherHM</string>
            <org.codehaus.groovy.grails.web.json.JSONObject>
              <myHashMap>
                <entry>
                  <string>deep string</string>
                  <string>deep string value</string>
                </entry>
              </myHashMap>
            </org.codehaus.groovy.grails.web.json.JSONObject>
          </entry>
          <entry>
            <string>amount</string>
            <string>1.00</string>
          </entry>
          <entry>
            <string>gatewayTransactionId</string>
            <string>AXISJUSPAYc4b572555c954b04abc3835fc</string>
          </entry>
          <entry>
            <string>gatewayResponseMessage</string>
            <string>TRANSACTION HAS DECLINED</string>
          </entry>
          <entry>
            <string>transactionTimestamp</string>
            <string>2017-05-15T15:11:06+00:00</string>
          </entry>
          <entry>
            <string>merchantId</string>
            <string>JPMERCHANT</string>
          </entry>
          <entry>
            <string>gatewayResponseCode</string>
            <string>ZA</string>
          </entry>
          <entry>
            <string>merchantRequestId</string>
            <string>aupi28</string>
          </entry>
          <entry>
            <string>merchantChannelId</string>
            <string>JPMERCHANT</string>
          </entry>
        </myHashMap>
      </org.codehaus.groovy.grails.web.json.JSONObject>
    </entry>
    <entry>
      <string>checksum</string>
      <string>b43ac9dcfb502b3e8e0a5578e40e18d0db8dbf19d88f419146aa12ca1771d47a</string>
    </entry>
    <entry>
      <string>customResponse</string>
      <string>{}</string>
    </entry>
  </myHashMap>
</org.codehaus.groovy.grails.web.json.JSONObject>|]

<<<<<<< HEAD
testXmlMap :: PGRXml
testXmlMap = PGRMap $ XmlMap $ Map.fromList $ coerce [ Entry ("SomeTextEntry", EText "TextValue")
=======
testXmlMap :: XmlMap
testXmlMap = XmlMap $ Map.fromList $ coerce [ Entry ("SomeTextEntry", EText "TextValue")
>>>>>>> aded to/from xml instances for PaymentGatewayResponseXml
              , Entry ("SomeBoolEntry", EBool True)
              , Entry ("SomeIntEntry", EInt 5)
              ]

rawXmlMapXML :: String
rawXmlMapXML = [r|<map>
  <entry>
    <string>SomeTextEntry</string>
    <string>TextValue</string>
  </entry>
  <entry>
    <string>SomeBoolEntry</string>
    <boolean>true</boolean>
  </entry>
  <entry>
    <string>SomeIntEntry</string>
    <int>5</int>
  </entry>
<<<<<<< HEAD
</map>|]
=======
</map>|]
>>>>>>> aded to/from xml instances for PaymentGatewayResponseXml
