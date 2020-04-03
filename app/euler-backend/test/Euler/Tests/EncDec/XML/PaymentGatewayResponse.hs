{-# LANGUAGE QuasiQuotes #-}

module Euler.Tests.EncDec.XML.PaymentGatewayResponse
  ( spec
  )
  where

import           EulerHS.Prelude

import           Data.Coerce
import           Test.Hspec
import           Text.RawString.QQ
import           Xmlbf
import           Xmlbf.Xeno

import qualified Data.Binary.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import           Euler.Common.Types.PaymentGatewayResponseXml
import           Euler.Common.Types.Gateway
import           Euler.Product.OLTP.Order.OrderStatus (findPayerVpaByGateway)


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

    it "findPayerVpaByGateway,xml is Nothing" $  do
      let res = findPayerVpaByGateway (Just PAYU) Nothing
      res `shouldBe` ""

    it "findPayerVpaByGateway, gateway is AXIS_UPI" $  do
      let res = findPayerVpaByGateway (Just AXIS_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is HDFC_UPI" $  do
      let res = findPayerVpaByGateway (Just HDFC_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is INDUS_UPI" $  do
      let res = findPayerVpaByGateway (Just INDUS_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is KOTAK_UPI" $  do
      let res = findPayerVpaByGateway (Just KOTAK_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is SBI_UPI" $  do
      let res = findPayerVpaByGateway (Just SBI_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is ICICI_UPI" $  do
      let res = findPayerVpaByGateway (Just ICICI_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is HSBC_UPI" $  do
      let res = findPayerVpaByGateway (Just HSBC_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is VIJAYA_UPI" $  do
      let res = findPayerVpaByGateway (Just VIJAYA_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is HSBC_UPI" $  do
      let res = findPayerVpaByGateway (Just YESBANK_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is HSBC_UPI" $  do
      let res = findPayerVpaByGateway (Just YESBANK_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is PAYTM_UPI" $  do
      let res = findPayerVpaByGateway (Just PAYTM_UPI) $ Just rawXmlForPayersVpa
      res `shouldBe` "debopriya.das1001-1@okicici"

    it "findPayerVpaByGateway, gateway is PAYU" $  do
      let res = findPayerVpaByGateway (Just PAYU) $ Just rawXmlForField3
      res `shouldBe` "8771989042073390"

    it "findPayerVpaByGateway, gateway is RAZORPAY" $  do
      let res = findPayerVpaByGateway (Just RAZORPAY) $ Just rawXmlForVpa
      res `shouldBe` "9962779655@upi"

    -- Symbol Singla: PAYTM_V2 gateway was supposed to send this key in response, but they never did.
    -- So, There’s no xml with VPA inside
    it "findPayerVpaByGateway, gateway is PAYTM_V2" $  do
      let res = findPayerVpaByGateway (Just PAYTM_V2) $ Just rawXmlForVPA
      res `shouldBe` "" -- empty text is OK.

    it "findPayerVpaByGateway, gateway is GOCASHFREE" $  do
      let res = findPayerVpaByGateway (Just GOCASHFREE) $ Just rawXmlForPayersVPA
      res `shouldBe` "7602476670@paytm"

    it "findPayerVpaByGateway, gateway is other" $  do
      let res = findPayerVpaByGateway (Just TPSL) $ Just rawXmlForField3
      res `shouldBe` ""



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

testGroovyHM :: PGRXml
testGroovyHM = PGRGhm $ GroovyHM (Map.fromList
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

testXmlMap :: PGRXml
testXmlMap = PGRMap $ XmlMap $ Map.fromList $ coerce [ Entry ("SomeTextEntry", EText "TextValue")
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
</map>|]


rawXmlForField3 :: Text
rawXmlForField3 = [r|
<linked-hash-map>
  <entry>
    <string>phone</string>
    <string>9999999999</string>
  </entry>
  <entry>
    <string>mode</string>
    <string>DC</string>
  </entry>
  <entry>
    <string>mihpayid</string>
    <string>6583311051</string>
  </entry>
  <entry>
    <string>firstname</string>
    <string>Firstname</string>
  </entry>
  <entry>
    <string>email</string>
    <string>juspay_test_user_101@gmail.com</string>
  </entry>
  <entry>
    <string>amount</string>
    <string>10.00</string>
  </entry>
  <entry>
    <string>bankcode</string>
    <string>MAST</string>
  </entry>
  <entry>
    <string>unmappedstatus</string>
    <string>failed</string>
  </entry>
  <entry>
    <string>bank_ref_num</string>
    <string>8771989042073390</string>
  </entry>
  <entry>
    <string>error_Message</string>
    <string>Bank denied transaction on the card.</string>
  </entry>
  <entry>
    <string>PG_TYPE</string>
    <string>HDFCPG</string>
  </entry>
  <entry>
    <string>status</string>
    <string>failure</string>
  </entry>
  <entry>
    <string>discount</string>
    <string>0.00</string>
  </entry>
  <entry>
    <string>net_amount_debit</string>
    <string>0.00</string>
  </entry>
  <entry>
    <string>error</string>
    <string>E324</string>
  </entry>
  <entry>
    <string>field3</string>
    <string>8771989042073390</string>
  </entry>
  <entry>
    <string>field4</string>
    <string>8771989042073390</string>
  </entry>
  <entry>
    <string>field5</string>
    <string>DENIED BY RISK</string>
  </entry>
  <entry>
    <string>field7</string>
    <string>Y</string>
  </entry>
  <entry>
    <string>field9</string>
    <string>DENIED BY RISK</string>
  </entry>
  <entry>
    <string>cardCategory</string>
    <string>domestic</string>
  </entry>
</linked-hash-map>
|]

rawXmlForPayersVpa :: Text
rawXmlForPayersVpa = [r|
<linked-hash-map>
    <entry>
        <string>upiRequestId</string>
        <string>AXISSDKV35b5c3e4c813141f28ac4806523</string>
    </entry>
    <entry>
        <string>updatedAt</string>
        <string>2019-08-31T18:32:08.731Z</string>
    </entry>
    <entry>
        <string>type</string>
        <string>COLLECT</string>
    </entry>
    <entry>
        <string>transactionSource</string>
        <string>MERCHANT</string>
    </entry>
    <entry>
        <string>transactionRef</string>
        <string>eulv1zoJ4N1t3ZQiSrR</string>
    </entry>
    <entry>
        <string>transactionAt</string>
        <string>2019-08-31T18:32:08.731Z</string>
    </entry>
    <entry>
        <string>status</string>
        <string>SUCCESS</string>
    </entry>
    <entry>
        <string>selfInitiated</string>
        <string>true</string>
    </entry>
    <entry>
        <string>remarks</string>
        <string>olacabs</string>
    </entry>
    <entry>
        <string>payerVpa</string>
        <string>debopriya.das1001-1@okicici</string>
    </entry>
    <entry>
        <string>payeeVpa</string>
        <string>ola.money@axisbank</string>
    </entry>
    <entry>
        <string>mode</string>
        <string>UPI</string>
    </entry>
    <entry>
        <string>id</string>
        <string>A3fcd7b059584c8083f02cedae55382</string>
    </entry>
    <entry>
        <string>expiry</string>
        <string>2019-08-31T19:01:05.000Z</string>
    </entry>
    <entry>
        <string>custRef</string>
        <string>924400083153</string>
    </entry>
    <entry>
        <string>currency</string>
        <string>INR</string>
    </entry>
    <entry>
        <string>createdAt</string>
        <string>2019-08-31T18:31:05.198Z</string>
    </entry>
    <entry>
        <string>callbackUrl</string>
        <string>https://api.juspay.in/v2/pay/webhooks/olacabs/axis_upi</string>
    </entry>
    <entry>
        <string>amount</string>
        <string>400.00</string>
    </entry>
    <entry>
        <string>PayeeVpaId</string>
        <string>Ab2f9910bce948c797f1ec02988f9f6</string>
    </entry>
    <entry>
        <string>OrderId</string>
        <string>875b4f1034e4a308b943498a9d32b2</string>
    </entry>
    <entry>
        <string>CustomerId</string>
        <string>A376c403fe124046a4039356794ff08</string>
    </entry>
    <entry>
        <string>AgencyId</string>
        <string>Adeb26cfbe7140f3aa3e4c9f085994c</string>
    </entry>
</linked-hash-map>
|]

rawXmlForVpa :: Text
rawXmlForVpa = [r|
<linked-hash-map>
  <entry>
    <string>vpa</string>
    <string>9962779655@upi</string>
  </entry>
  <entry>
    <string>tax</string>
    <string>0</string>
  </entry>
  <entry>
    <string>status</string>
    <string>captured</string>
  </entry>
  <entry>
    <string>order_id</string>
    <string>order_EZKpkOXIxbaxWe</string>
  </entry>
  <entry>
    <string>notes</string>
    <string>
      <transaction_id>razorpay_uat-5799624706-1</transaction_id>
      <txn_uuid>cjcdx3fjurui801t</txn_uuid>
    </string>
  </entry>
  <entry>
    <string>method</string>
    <string>upi</string>
  </entry>
  <entry>
    <string>international</string>
    <string>false</string>
  </entry>
  <entry>
    <string>id</string>
    <string>pay_EZKpkc1R3Mdomq</string>
  </entry>
  <entry>
    <string>fee</string>
    <string>2</string>
  </entry>
  <entry>
    <string>entity</string>
    <string>payment</string>
  </entry>
  <entry>
    <string>email</string>
    <string>cjabgonvpjse@example.com</string>
  </entry>
  <entry>
    <string>currency</string>
    <string>INR</string>
  </entry>
  <entry>
    <string>created_at</string>
    <string>1585768887</string>
  </entry>
  <entry>
    <string>contact</string>
    <string>+919570202425</string>
  </entry>
  <entry>
    <string>captured</string>
    <string>true</string>
  </entry>
  <entry>
    <string>amount_refunded</string>
    <string>0</string>
  </entry>
  <entry>
    <string>amount</string>
    <string>100</string>
  </entry>
</linked-hash-map>
|]

rawXmlForVPA :: Text
rawXmlForVPA = [r|<linked-hash-map><entry><string>txTime</string><string>2020-03-23 07:57:54</string></entry><entry><string>txStatus</string><string>FAILED</string></entry><entry><string>txMsg</string><string>ZM::INVALID UPI PIN</string></entry><entry><string>status</string><string>OK</string></entry><entry><string>referenceId</string><string>106936653</string></entry><entry><string>paymentMode</string><string>UPI</string></entry><entry><string>paymentDetails</string><string><utr>008307784654</utr><paymentMode/><payersVPA>7602476670@paytm</payersVPA><cardScheme/><cardNumber/><cardCountry/><bankName/><authIdCode/></string></entry><entry><string>orderStatus</string><string>ACTIVE</string></entry><entry><string>orderExpiryTime</string><string>2020-04-22 07:57:54</string></entry><entry><string>orderCurrency</string><string>INR</string></entry><entry><string>orderAmount</string><string>8.00</string></entry></linked-hash-map>|]

rawXmlForPayersVPA :: Text
rawXmlForPayersVPA = [r|<linked-hash-map><entry><string>txTime</string><string>2020-03-23 07:57:54</string></entry><entry><string>txStatus</string><string>FAILED</string></entry><entry><string>txMsg</string><string>ZM::INVALID UPI PIN</string></entry><entry><string>status</string><string>OK</string></entry><entry><string>referenceId</string><string>106936653</string></entry><entry><string>paymentMode</string><string>UPI</string></entry><entry><string>paymentDetails</string><string><utr>008307784654</utr><paymentMode/><payersVPA>7602476670@paytm</payersVPA><cardScheme/><cardNumber/><cardCountry/><bankName/><authIdCode/></string></entry><entry><string>orderStatus</string><string>ACTIVE</string></entry><entry><string>orderExpiryTime</string><string>2020-04-22 07:57:54</string></entry><entry><string>orderCurrency</string><string>INR</string></entry><entry><string>orderAmount</string><string>8.00</string></entry></linked-hash-map>|]
