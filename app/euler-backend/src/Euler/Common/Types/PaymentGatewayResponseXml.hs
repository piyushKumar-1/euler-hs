{-# LANGUAGE TupleSections #-}

module Euler.Common.Types.PaymentGatewayResponseXml
  ( LHM (..)
  , XmlMap(..)
  , GroovyHM(..)
  , Entry(..)
  , EValue(..)
  , PGRXml(..)
 -- , OpusPGResponse(..)
  )
  where

import EulerHS.Prelude

import Xmlbf
import Data.Coerce

import qualified Data.Binary.Builder     as BB
import qualified Data.Map.Strict         as Map
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Text.Read
import qualified Xmlbf                   as XML


readEither' :: Read a => TL.Text -> Either String a
readEither' = Text.Read.readEither . TL.unpack

data PGRXml = PGRLhm LHM | PGRMap XmlMap | PGRGhm GroovyHM
  deriving (Eq, Show, Generic)

instance FromXml PGRXml where
  fromXml =   PGRLhm <$> fromXml
          <|> PGRMap <$> fromXml
          <|> PGRGhm <$> fromXml

instance ToXml PGRXml where
  toXml (PGRLhm m) = toXml m
  toXml (PGRMap m) = toXml m
  toXml (PGRGhm m) = toXml m



-- lookupEntry :: TL.Text -> PGRXml -> Maybe EValue
-- lookupEntry name m = Map.lookup name $ coerce m

-- not shure that this is used in current euler
--data OpusPGResponse = OpusPGResponse
--  { strRespCode    :: Maybe Text
--  , strRespMessage :: Maybe Text
--  , strTxnId       :: Maybe Text
--  , strEPGTxnId    :: Maybe Text
--  , strAuthIdCode  :: Maybe Text
--  , strRRN         :: Maybe Text
--  , strCVRespCode  :: Maybe Text
--  , strTxnType     :: Maybe Text
--  , strTxnDateTime :: Maybe Text
--  }
--  deriving (Eq, Show, Generic)
--
--instance FromXml OpusPGResponse where
--  fromXml = pElement "com.opus.epg.sfa.java.PGResponse" p
--    where
--      p = do
--        cs <- pChildren
--        let (csMap :: Map Text TL.Text) = Map.fromList $ catMaybes $ nodeToTuple <$> cs
--        let strRespCode    = TL.toStrict <$> Map.lookup "strRespCode" csMap
--        let strRespMessage = TL.toStrict <$> Map.lookup "strRespMessage" csMap
--        let strTxnId       = TL.toStrict <$> Map.lookup "strRespMessage" csMap
--        let strEPGTxnId    = TL.toStrict <$> Map.lookup "strRespMessage" csMap
--        let strAuthIdCode  = TL.toStrict <$> Map.lookup "strRespMessage" csMap
--        let strRRN         = TL.toStrict <$> Map.lookup "strRespMessage" csMap
--        let strCVRespCode  = TL.toStrict <$> Map.lookup "strRespMessage" csMap
--        let strTxnType     = TL.toStrict <$> Map.lookup "strTxnType" csMap
--        let strTxnDateTime = TL.toStrict <$> Map.lookup "strTxnDateTime" csMap
--        pure OpusPGResponse{..}
--
--nodeToTuple :: Node -> Maybe (Text, TL.Text)
--nodeToTuple (Element n as cs) = Just (n, t)
--  where
--    t = case cs of
--      ((Text lt) : ns) ->  lt
--      l@((Element _ _ _) : ns') -> TLE.decodeUtf8 $ BB.toLazyByteString $ XML.encode l
--      _ -> mempty
--
--nodeToTuple (Text t) = Nothing
--
--
--instance ToXml OpusPGResponse where
--  toXml OpusPGResponse {..} =
--    element "com.opus.epg.sfa.java.PGResponse" mempty $ concat
--      [ element "strRespCode" mempty $ text $ TL.fromStrict $ fromMaybe mempty strRespCode
--      , element "strRespMessage" mempty $ text $ TL.fromStrict $ fromMaybe mempty strRespMessage
--      , element "strTxnId" mempty $ text $ TL.fromStrict $ fromMaybe mempty strTxnId
--      , element "strEPGTxnId" mempty $ text $ TL.fromStrict $ fromMaybe mempty strEPGTxnId
--      , element "strAuthIdCode" mempty $ text $ TL.fromStrict $ fromMaybe mempty strAuthIdCode
--      , element "strRRN" mempty $ text $ TL.fromStrict $ fromMaybe mempty strRRN
--      , element "strCVRespCode" mempty $ text $ TL.fromStrict $ fromMaybe mempty strCVRespCode
--      , element "strTxnType" mempty $ text $ TL.fromStrict $ fromMaybe mempty strTxnType
--      , element "strTxnDateTime" mempty $ text $ TL.fromStrict $ fromMaybe mempty strTxnDateTime
--      ]

data EValue = EText TL.Text
            | EInt Int
            | EBool Bool
            | EGroovyHM GroovyHM
  deriving (Eq, Show, Generic)

newtype Entry = Entry (TL.Text, EValue)
  deriving (Eq, Show, Generic)

instance FromXml Entry where
  fromXml = pElement "entry" p
    where
      p = do
        n <- pElement "string" pText
        v <- pAnyElement nt
        pure $ Entry (n,v)
      nt = do
        vType <- pName
        case vType of
          "string" -> do
            tv <- pOneText  <|> pCText
            pure $ EText tv
          "int" -> do
            i <- pText
            either fail (pure . EInt) $ readEither' i
          "boolean" -> do
            b <- pText
            either fail (pure . EBool) $ readEither' $ TL.toTitle b
          "org.codehaus.groovy.grails.web.json.JSONObject" -> do
            groovyHM <- ghmParser
            pure $ EGroovyHM groovyHM
          _ -> fail $ "Unknown type : " <> show vType


      pCText = (TLE.decodeUtf8 . BB.toLazyByteString . encode) <$> pChildren
      pOneText = do
        t <- pText
        pEndOfInput
        pure t

instance ToXml Entry where
  toXml (Entry (n, EText v)) = element "entry" mempty $ concat
    [ element "string" mempty $ text n
    , element "string" mempty $ text v
    ]
  toXml (Entry (n, EInt v)) = element "entry" mempty $ concat
    [ element "string" mempty $ text n
    , element "int" mempty $ text $ show v
    ]
  toXml (Entry (n, EBool v)) = element "entry" mempty $ concat
    [ element "string" mempty $ text n
    , element "boolean" mempty $ text $ TL.toLower $ show v
    ]
  toXml (Entry (n, EGroovyHM v)) = element "entry" mempty $ concat
    [ element "string" mempty $ text n
    , toXml v
    ]





newtype LHM = LHM ( Map.Map TL.Text EValue)
  deriving (Eq, Show, Generic)

instance FromXml LHM where
  fromXml = pElement "linked-hash-map" p
    where
      p = do
        cs <- pChildren
        let (l :: [Either String Entry]) = (runParser (fromXml) . pure)  <$> cs
        pure $ LHM $ Map.fromList $ coerce $ rights l

instance ToXml LHM where
  toXml (LHM m) =
    element "linked-hash-map" mempty $ concat $
      toXml <$> coerce @_ @[Entry] (Map.toList m)


newtype XmlMap = XmlMap ( Map.Map TL.Text EValue)
  deriving (Eq, Show, Generic)

instance FromXml XmlMap where
  fromXml = pElement "map" p
    where
      p = do
        cs <- pChildren
        let (l :: [Either String Entry]) = (runParser (fromXml) . pure)  <$> cs
        pure $ XmlMap $ Map.fromList $ coerce $ rights l

instance ToXml XmlMap where
  toXml (XmlMap m) =
    element "map" mempty $ concat $
      toXml <$> coerce @_ @[Entry] (Map.toList m)


data GroovyHM = GroovyHM ( Map.Map TL.Text EValue)
  deriving (Eq, Show, Generic)

instance FromXml GroovyHM where
  fromXml = pElement "org.codehaus.groovy.grails.web.json.JSONObject" ghmParser

ghmParser :: Parser GroovyHM
ghmParser = pElement "myHashMap" p
  where
    p = do
        cs <- pChildren
        let (l :: [Either String Entry]) = (runParser (fromXml) . pure)  <$> cs
        pure $ GroovyHM $ Map.fromList $ coerce $ rights l

instance ToXml GroovyHM where
  toXml (GroovyHM m) =
    element "org.codehaus.groovy.grails.web.json.JSONObject" mempty $
      element "myHashMap" mempty $ concat $ toXml <$> coerce @_ @[Entry] (Map.toList m)

{-


<org.codehaus.groovy.grails.web.json.JSONObject>
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
</org.codehaus.groovy.grails.web.json.JSONObject>


<com.opus.epg.sfa.java.PGResponse>
  <strRespCode>0</strRespCode>
  <strRespMessage>TransactionSuccessful</strRespMessage>
  <strTxnId>hoopos-1344705444-001</strTxnId>
  <strEPGTxnId>epg_id</strEPGTxnId>
  <strAuthIdCode>auth_id_code</strAuthIdCode>
  <strRRN>rpn</strRRN>
</com.opus.epg.sfa.java.PGResponse>


<com.opus.epg.sfa.java.PGResponse>
  <strRespCode>1</strRespCode>
  <strRespMessage>unauthorized usage</strRespMessage>
  <strTxnId>hoopos-1344883786-001</strTxnId>
  <strEPGTxnId>201208142439589</strEPGTxnId>
  <strAuthIdCode>000000</strAuthIdCode>
  <strRRN>000143754156</strRRN>
  <strCVRespCode>C</strCVRespCode>
</com.opus.epg.sfa.java.PGResponse>


<com.opus.epg.sfa.java.PGResponse>
  <strRespCode>2</strRespCode>
  <strRespMessage>Error while encrypting data. Transaction cannot be processed.</strRespMessage>
  <strTxnId>hoopos-1344893059-001</strTxnId>
</com.opus.epg.sfa.java.PGResponse>

<com.opus.epg.sfa.java.PGResponse>
  <strRespCode>1</strRespCode>
  <strRespMessage>unauthorized usage</strRespMessage>
  <strTxnId>hoopos-1344885440-001</strTxnId>
  <strEPGTxnId>201208142441544</strEPGTxnId>
  <strAuthIdCode>000000</strAuthIdCode>
  <strRRN>000090017129</strRRN>
  <strCVRespCode>C</strCVRespCode>
</com.opus.epg.sfa.java.PGResponse>

<com.opus.epg.sfa.java.PGResponse>
  <strRespCode>0</strRespCode>
  <strRespMessage>TransactionSuccessful</strRespMessage>
  <strTxnId>redbus-1347388948-001</strTxnId>
  <strEPGTxnId>epg_id</strEPGTxnId>
  <strAuthIdCode>auth_id_code</strAuthIdCode>
  <strRRN>rpn</strRRN>
</com.opus.epg.sfa.java.PGResponse>


<com.opus.epg.sfa.java.PGResponse>
  <strRespCode>1</strRespCode>
  <strRespMessage>unauthorized usage</strRespMessage>
  <strTxnId>spicinemas-spi_11223344_1-1</strTxnId>
  <strEPGTxnId>201309104156481</strEPGTxnId>
  <strAuthIdCode>000000</strAuthIdCode>
  <strRRN>000015691192</strRRN>
  <strTxnType>Sale</strTxnType>
  <strTxnDateTime>10/09/2013 20:00:53</strTxnDateTime>
  <strCVRespCode>C</strCVRespCode>
</com.opus.epg.sfa.java.PGResponse>


<linked-hash-map>
  <entry>
    <string>vpc_AVS_StateProv</string>
    <string>null</string>
  </entry>
  <entry>
    <string>vpc_AVSRequestCode</string>
    <string>Z</string>
  </entry>
  <entry>
    <string>vpc_BatchNo</string>
    <string>20121125</string>
  </entry>
  <entry>
    <string>vpc_Card</string>
    <string>MC</string>
  </entry>
  <entry>
    <string>vpc_Version</string>
    <string>1</string>
  </entry>
  <entry>
    <string>vpc_AcqAVSRespCode</string>
    <string>S</string>
  </entry>
  <entry>
    <string>vpc_AVSResultCode</string>
    <string>S</string>
  </entry>
  <entry>
    <string>vpc_Merchant</string>
    <string>ONEREDBUS</string>
  </entry>
  <entry>
    <string>vpc_Amount</string>
    <string>200</string>
  </entry>
  <entry>
    <string>vpc_SecureHash</string>
    <string>F7D4D9449F5221A2F39922377A311379</string>
  </entry>
  <entry>
    <string>vpc_AVS_Country</string>
    <string>IND</string>
  </entry>
  <entry>
    <string>vpc_3DSstatus</string>
    <string>Y</string>
  </entry>
  <entry>
    <string>vpc_MerchTxnRef</string>
    <string>redbus-1353844636-001</string>
  </entry>
  <entry>
    <string>vpc_CSCResultCode</string>
    <string>N</string>
  </entry>
  <entry>
    <string>vpc_VerToken</string>
    <string>jKBgYzVpDbCwCBEAxXDqADQAAAA=</string>
  </entry>
  <entry>
    <string>vpc_OrderInfo</string>
    <string>1353844636</string>
  </entry>
  <entry>
    <string>vpc_Command</string>
    <string>pay</string>
  </entry>
  <entry>
    <string>vpc_3DSenrolled</string>
    <string>Y</string>
  </entry>
  <entry>
    <string>vpc_AcqResponseCode</string>
    <string>05</string>
  </entry>
  <entry>
    <string>vpc_VerType</string>
    <string>3DS</string>
  </entry>
  <entry>
    <string>vpc_VerSecurityLevel</string>
    <string>05</string>
  </entry>
  <entry>
    <string>vpc_TransactionNo</string>
    <string>2001648588</string>
  </entry>
  <entry>
    <string>vpc_ReceiptNo</string>
    <string>233020611404</string>
  </entry>
  <entry>
    <string>vpc_Message</string>
    <string>Declined</string>
  </entry>
  <entry>
    <string>vpc_3DSXID</string>
    <string>3m45yD/uU7I6zoAv4i5jZXhVk+c=</string>
  </entry>
  <entry>
    <string>vpc_AVS_City</string>
    <string>null</string>
  </entry>
  <entry>
    <string>vpc_3DSECI</string>
    <string>02</string>
  </entry>
  <entry>
    <string>vpc_Locale</string>
    <string>en_AU</string>
  </entry>
  <entry>
    <string>vpc_AcqCSCRespCode</string>
    <string>N</string>
  </entry>
  <entry>
    <string>vpc_VerStatus</string>
    <string>Y</string>
  </entry>
  <entry>
    <string>vpc_TxnResponseCode</string>
    <string>2</string>
  </entry>
  <entry>
    <string>vpc_AVS_Street01</string>
    <string>null</string>
  </entry>
  <entry>
    <string>vpc_AVS_PostCode</string>
    <string>null</string>
  </entry>
</linked-hash-map>


<linked-hash-map>
  <entry>
    <string>txTime</string>
    <string>2018-10-11 20:20:24</string>
  </entry>
  <entry>
    <string>txStatus</string>
    <string>SUCCESS</string>
  </entry>
  <entry>
    <string>txMsg</string>
    <string>OK</string>
  </entry>
  <entry>
    <string>signature</string>
    <string>ch7+UrsjTv9diBs5wGh5e9iQcwhgWgjPfAksQrwKy/0=</string>
  </entry>
  <entry>
    <string>referenceId</string>
    <string>42330</string>
  </entry>
  <entry>
    <string>paymentMode</string>
    <string>sim</string>
  </entry>
  <entry>
    <string>orderId</string>
    <string>test_9069_385</string>
  </entry>
  <entry>
    <string>orderAmount</string>
    <string>1.98</string>
  </entry>
</linked-hash-map>

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

<linked-hash-map>
  <entry>
    <string>transactions</string>
    <string>
      <soft_descriptor>PAYPAL *NA</soft_descriptor>
      <shipping_address/>
      <related_resources>
      <sale>
      <update_time>2019-08-01T11:25:36Z</update_time>
      <transaction_fee>
      <value>0.04</value>
      <currency>INR</currency>
      </transaction_fee>
      <state>completed</state>
      <soft_descriptor>PAYPAL *NA</soft_descriptor>
      <reason_code/>
      <protection_eligibility_type>ITEM_NOT_RECEIVED_ELIGIBLE,UNAUTHORIZED_PAYMENT_ELIGIBLE</protection_eligibility_type>
      <protection_eligibility>ELIGIBLE</protection_eligibility>
      <payment_mode>INSTANT_TRANSFER</payment_mode>
      <parent_payment>PAYID-LVBMXXI1AW89865NA9825521</parent_payment>
      <links>
        <rel>self</rel>
        <method>GET</method>
        <href>https://api.sandbox.paypal.com/v1/payments/sale/5M028363RC295292B</href>
        <rel>refund</rel
        ><method>POST</method>
        <href>https://api.sandbox.paypal.com/v1/payments/sale/5M028363RC295292B/refund</href>
        <rel>parent_payment</rel>
        <method>GET</method>
        <href>https://api.sandbox.paypal.com/v1/payments/payment/PAYID-LVBMXXI1AW89865NA9825521</href>
      </links>
      <id>5M028363RC295292B</id>
      <create_time>2019-08-01T11:25:36Z</create_time>
      <amount>
        <total>1.00</total>
        <details>
          <tax>0.00</tax>
          <subtotal>1.00</subtotal>
          <shipping_discount>0.00</shipping_discount>
          <shipping>0.00</shipping>
          <insurance>0.00</insurance>
          <handling_fee>0.00</handling_fee>
        </details>
        <currency>INR</currency>
      </amount>
      </sale>
      <refund/>
      </related_resources>
      <payee>
        <merchant_id>KZ24F8QK5NKTE</merchant_id>
        <email>testmerchant-juspay@juspay.com</email>
      </payee>
      <item_list>
        <shipping_phone_number>+917892077933</shipping_phone_number>
        <shipping_address>
          <state>state</state>
          <recipient_name> hello</recipient_name>
          <postal_code/>
          <phone/>
          <line2>line2</line2>
          <line1>line1</line1>
          <country_code>IN</country_code>
          <city>Mumbai</city>
        </shipping_address>
        <items>
          <tax>0.00</tax>
          <sku>1</sku>
          <quantity>1</quantity>
          <price>1.00</price>
          <name/>
          <currency>INR</currency>
        </items>
      </item_list>
      <invoice_number>record_test_38</invoice_number>
      <description/>
      <custom>record_test_38</custom>
      <amount>
        <total>1.00</total>
        <details>
          <tax>0.00</tax>
          <subtotal>1.00</subtotal>
          <shipping_discount>0.00</shipping_discount>
          <shipping>0.00</shipping>
          <insurance>0.00</insurance>
          <handling_fee>0.00</handling_fee>
        </details>
        <currency>INR</currency>
      </amount>
    </string>
  </entry>
  <entry>
    <string>state</string>
    <string>approved</string>
  </entry>
  <entry>
    <string>sale_id</string>
    <string>5M028363RC295292B</string>
  </entry>
  <entry>
    <string>paymentId</string>
    <string>PAYID-LVBMXXI1AW89865NA9825521</string>
  </entry>
  <entry>
    <string>payer</string>
    <string>
      <status>VERIFIED</status>
      <payment_method>paypal</payment_method>
      <payer_info>
      <state/>
      <shipping_address>
      <state>state</state>
      <recipient_name> hello</recipient_name>
      <postal_code/>
      <phone/>
      <line2>line2</line2
      ><line1>line1</line1>
      <country_code>IN</country_code>
      <city>Mumbai</city>
      </shipping_address>
      <phone/>
      <payer_id>J3KSSFAWJL694</payer_id>
      <last_name>India</last_name>
      <first_name>Amrit</first_name>
      <email>amritin@gmail.com</email>
      <country_code>IN</country_code>
      </payer_info>
    </string>
  </entry>
  <entry>
    <string>links</string>
    <string>
      <rel>self</rel>
      <method>GET</method>
      <href>https://api.sandbox.paypal.com/v1/payments/payment/PAYID-LVBMXXI1AW89865NA9825521</href>
    </string>
  </entry>
  <entry>
    <string>invoice_number</string>
    <string>record_test_38</string>
  </entry>
  <entry>
    <string>intent</string>
    <string>sale</string>
  </entry>
  <entry>
    <string>id</string>
    <string>PAYID-LVBMXXI1AW89865NA9825521</string>
  </entry>
  <entry>
    <string>create_time</string>
    <string>2019-08-01T11:24:12Z</string>
  </entry>
  <entry>
    <string>cart</string>
    <string>00G24483JW2630253</string>
  </entry>
</linked-hash-map>



<map>
  <entry>
    <string>vpc_AuthorizeId</string>
    <string>021392</string>
  </entry>
  <entry>
    <string>vpc_AVSRequestCode</string>
    <string>Z</string>
  </entry>
  <entry>
    <string>vpc_BatchNo</string>
    <string>36</string>
  </entry>
  <entry>
    <string>vpc_Version</string>
    <string>1</string>
  </entry>
  <entry>
    <string>vpc_Card</string>
    <string>AE</string>
  </entry>
  <entry>
    <string>vpc_AcqAVSRespCode</string>
    <string>Unsupported</string>
  </entry>
  <entry>
    <string>vpc_AVSResultCode</string>
    <string>Unsupported</string>
  </entry>
  <entry>
    <string>vpc_Merchant</string>
    <string>TEST9820005858</string>
  </entry>
  <entry>
    <string>vpc_Amount</string>
    <string>500</string>
  </entry>
  <entry>
    <string>vpc_3DSstatus</string>
    <string>Y</string>
  </entry>
  <entry>
    <string>vpc_MerchTxnRef</string>
    <string>amex_test-1366275275204-1</string>
  </entry>
  <entry>
    <string>vpc_CSCResultCode</string>
    <string>M</string>
  </entry>
  <entry>
    <string>vpc_VerToken</string>
    <string>gIGCg4SFhoeIiYqLjI2Oj5CRkpM=</string>
  </entry>
  <entry>
    <string>vpc_Command</string>
    <string>pay</string>
  </entry>
  <entry>
    <string>vpc_OrderInfo</string>
    <string>1366275275204</string>
  </entry>
  <entry>
    <string>vpc_3DSenrolled</string>
    <string>Y</string>
  </entry>
  <entry>
    <string>vpc_CSCRequestCode</string>
    <string>S</string>
  </entry>
  <entry>
    <string>vpc_AcqResponseCode</string>
    <string>000</string>
  </entry>
  <entry>
    <string>vpc_VerType</string>
    <string>3DS</string>
  </entry>
  <entry>
    <string>vpc_VerSecurityLevel</string>
    <string>05</string>
  </entry>
  <entry>
    <string>vpc_TransactionNo</string>
    <string>104</string>
  </entry>
  <entry>
    <string>vpc_ReceiptNo</string>
    <string>130418139</string>
  </entry>
  <entry>
    <string>vpc_Message</string>
    <string>Approved</string>
  </entry>
  <entry>
    <string>vpc_AcquirerResponseAdvice</string>
    <string>Approved </string>
  </entry>
  <entry>
    <string>vpc_3DSXID</string>
    <string>iAkVgXL8ONkJ5x+h597VLDEp7wo=</string>
  </entry>
  <entry>
    <string>vpc_3DSECI</string>
    <string>05</string>
  </entry>
  <entry>
    <string>vpc_AcqCSCRespCode</string>
    <string>Y</string>
  </entry>
  <entry>
    <string>vpc_Locale</string>
    <string>en_IN</string>
  </entry>
  <entry>
    <string>vpc_VerStatus</string>
    <string>Y</string>
  </entry>
  <entry>
    <string>vpc_TxnResponseCode</string>
    <string>0</string>
  </entry>
</map>


<error_code_tag>CM90004</error_code_tag>
<error_service_tag>PARESLOG</error_service_tag>
<error_text>!ERROR!-PARESLOG: CM90004-Duplicate found error.</error_text>
<paymentid>429000561523361</paymentid>
<trackid>hdfctesting1354367240001</trackid>
<udf1>121</udf1>
<udf2>121</udf2>
<udf3>1212</udf3>
<udf4>121</udf4>
<udf5>1212</udf5>


<result>CAPTURED</result>
<auth>999999</auth>
<ref>233698356478</ref>
<avr>N</avr>
<postdate>1202</postdate>
<paymentid>1561646011623361</paymentid>
<tranid>1561646011623361</tranid>
<trackid>hdfctesting1354362757001</trackid>
<udf1>121</udf1>
<udf2>121</udf2>
<udf3>1212</udf3>
<udf4>121</udf4>
<udf5>1212</udf5>





<com.paypal.api.payments.Payment>
  <id>PAY-9NY48399UV2019343KGBXD3I</id>
  <createTime>2013-05-03T08:14:37Z</createTime>
  <updateTime>2013-05-03T08:14:40Z</updateTime>
  <state>approved</state>
  <intent>sale</intent>
  <payer>
    <paymentMethod>credit_card</paymentMethod>
    <fundingInstruments>
      <com.paypal.api.payments.FundingInstrument>
        <creditCard>
          <type>mastercard</type>
          <number>xxxxxxxxxxxx5285</number>
          <expireMonth>12</expireMonth>
          <expireYear>2015</expireYear>
          <firstName>undefined</firstName>
        </creditCard>
      </com.paypal.api.payments.FundingInstrument>
    </fundingInstruments>
  </payer>
  <transactions>
    <com.paypal.api.payments.Transaction>
      <amount>
        <total>10.00</total>
        <currency>USD</currency>
        <details>
          <subtotal>10.00</subtotal>
        </details>
      </amount>
      <description>This is the payment transaction description.</description>
      <relatedResources>
        <com.paypal.api.payments.SubTransaction>
          <sale>
            <id>4XJ38564VM409423H</id>
            <createTime>2013-05-03T08:14:37Z</createTime>
            <updateTime>2013-05-03T08:14:40Z</updateTime>
            <state>completed</state>
            <amount>
              <total>10.00</total>
              <currency>USD</currency>
            </amount>
            <parentPayment>PAY-9NY48399UV2019343KGBXD3I</parentPayment>
            <links>
              <com.paypal.api.payments.Link>
                <href>https://api.sandbox.paypal.com/v1/payments/sale/4XJ38564VM409423H</href>
                <rel>self</rel>
                <method>GET</method>
              </com.paypal.api.payments.Link>
              <com.paypal.api.payments.Link>
                <href>https://api.sandbox.paypal.com/v1/payments/sale/4XJ38564VM409423H/refund</href>
                <rel>refund</rel>
                <method>POST</method>
              </com.paypal.api.payments.Link>
              <com.paypal.api.payments.Link>
                <href>https://api.sandbox.paypal.com/v1/payments/payment/PAY-9NY48399UV2019343KGBXD3I</href>
                <rel>parent_payment</rel>
                <method>GET</method>
              </com.paypal.api.payments.Link>
            </links>
          </sale>
        </com.paypal.api.payments.SubTransaction>
      </relatedResources>
    </com.paypal.api.payments.Transaction>
  </transactions>
  <links>
    <com.paypal.api.payments.Link>
      <href>https://api.sandbox.paypal.com/v1/payments/payment/PAY-9NY48399UV2019343KGBXD3I</href>
      <rel>self</rel>
      <method>GET</method>
    </com.paypal.api.payments.Link>
  </links>
</com.paypal.api.payments.Payment>


<org.codehaus.groovy.grails.web.json.JSONObject>
  <myHashMap>
    <entry>
      <string>status</string>
      <string>captured</string>
    </entry>
    <entry>
      <string>entity</string>
      <string>payment</string>
    </entry>
    <entry>
      <string>captured</string>
      <boolean>true</boolean>
    </entry>
    <entry>
      <string>error_code</string>
      <org.codehaus.groovy.grails.web.json.JSONObject_-Null/>
    </entry>
    <entry>
      <string>service_tax</string>
      <int>1</int>
    </entry>
    <entry>
      <string>contact</string>
      <string>8041111111</string>
    </entry>
    <entry>
      <string>currency</string>
      <string>INR</string>
    </entry>
    <entry>
      <string>fee</string>
      <int>3</int>
    </entry>
    <entry>
      <string>id</string>
      <string>pay_4cCX31jlMYiu6w</string>
    </entry>
    <entry>
      <string>amount</string>
      <int>100</int>
    </entry>
    <entry>
      <string>email</string>
      <string>razor_pay_user_101@gmail.com</string>
    </entry>
    <entry>
      <string>amount_refunded</string>
      <int>0</int>
    </entry>
    <entry>
      <string>description</string>
      <org.codehaus.groovy.grails.web.json.JSONObject_-Null reference="../../entry[4]/org.codehaus.groovy.grails.web.json.JSONObject_-Null"/>
    </entry>
    <entry>
      <string>created_at</string>
      <int>1451023806</int>
    </entry>
    <entry>
      <string>method</string>
      <string>card</string>
    </entry>
    <entry>
      <string>refund_status</string>
      <org.codehaus.groovy.grails.web.json.JSONObject_-Null reference="../../entry[4]/org.codehaus.groovy.grails.web.json.JSONObject_-Null"/>
    </entry>
    <entry>
      <string>error_description</string>
      <org.codehaus.groovy.grails.web.json.JSONObject_-Null reference="../../entry[4]/org.codehaus.groovy.grails.web.json.JSONObject_-Null"/>
    </entry>
    <entry>
      <string>notes</string>
      <org.codehaus.groovy.grails.web.json.JSONObject>
        <myHashMap>
          <entry>
            <string>transaction_id</string>
            <string>1451024435</string>
          </entry>
        </myHashMap>
      </org.codehaus.groovy.grails.web.json.JSONObject>
    </entry>
  </myHashMap>
</org.codehaus.groovy.grails.web.json.JSONObject>

<org.codehaus.groovy.grails.web.json.JSONObject>
  <myHashMap>
    <entry>
      <string>error</string>
      <org.codehaus.groovy.grails.web.json.JSONObject>
        <myHashMap>
          <entry>
            <string>field</string>
            <string>amount</string>
          </entry>
          <entry>
            <string>description</string>
            <string>The amount field is required.</string>
          </entry>
          <entry>
            <string>code</string>
            <string>BAD_REQUEST_ERROR</string>
          </entry>
        </myHashMap>
      </org.codehaus.groovy.grails.web.json.JSONObject>
    </entry>
  </myHashMap>
</org.codehaus.groovy.grails.web.json.JSONObject>

<org.codehaus.groovy.grails.web.json.JSONObject>
  <myHashMap>
    <entry>
      <string>balanceamount</string>
      <string>8970</string>
    </entry>
    <entry>
      <string>status</string>
      <string>SUCCESS</string>
    </entry>
    <entry>
      <string>refId</string>
      <string>1803346902</string>
    </entry>
    <entry>
      <string>statusdescription</string>
      <string>Amount Debited</string>
    </entry>
    <entry>
      <string>checksum</string>
      <string>8a83a09a3221375e97c0d0536c792cb490ebca756e5331fa5c666c6fff581ee3</string>
    </entry>
    <entry>
      <string>orderid</string>
      <string>temp107</string>
    </entry>
    <entry>
      <string>messagecode</string>
      <string>503</string>
    </entry>
    <entry>
      <string>statuscode</string>
      <string>0</string>
    </entry>
    <entry>
      <string>debitedamount</string>
      <string>1.0</string>
    </entry>
  </myHashMap>
</org.codehaus.groovy.grails.web.json.JSONObject>


<org.codehaus.groovy.grails.web.json.JSONObject>
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
</org.codehaus.groovy.grails.web.json.JSONObject>

<org.codehaus.groovy.grails.web.json.JSONObject>
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
            <string>amount</string>
            <string>1.00</string>
          </entry>
          <entry>
            <string>merchantCustomerId</string>
            <string>juspay_test_user_101</string>
          </entry>
          <entry>
            <string>gatewayTransactionId</string>
            <string>AXISSDKV3c6928bb5b4f044db8c563895e1</string>
          </entry>
          <entry>
            <string>customerVpa</string>
            <string>7795296049@upi</string>
          </entry>
          <entry>
            <string>gatewayResponseMessage</string>
            <string>Failure</string>
          </entry>
          <entry>
            <string>transactionTimestamp</string>
            <string>2017-06-14T11:24:00+00:00</string>
          </entry>
          <entry>
            <string>merchantId</string>
            <string>JPMERCHANT</string>
          </entry>
          <entry>
            <string>gatewayResponseCode</string>
            <string>T04</string>
          </entry>
          <entry>
            <string>merchantRequestId</string>
            <string>juspay_test-1497445488-2</string>
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
      <string>361f638a15fcb7874749cdbeb5017f46ba0f3a70cf561f7960f909d6aa8ebcf0</string>
    </entry>
    <entry>
      <string>customResponse</string>
      <string>{}</string>
    </entry>
  </myHashMap>
</org.codehaus.groovy.grails.web.json.JSONObject>





-}