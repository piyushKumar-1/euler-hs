module Euler.Common.Types.RMSIDResult
  ( RMSIDResult (..)
  , Output (..)
  , decodeRMSIDResult
  , decodeOutput
  )
  where

import           EulerHS.Prelude

import qualified Data.Text.Lazy as TL
import           Xmlbf
import           Xmlbf.Xeno


decodeRMSIDResult :: ByteString -> Either String RMSIDResult
decodeRMSIDResult bs = runParser fromXml =<< (fromRawXml bs)

decodeOutput :: ByteString -> Either String Output
decodeOutput bs = either Left (Right . output) $ runParser fromXml =<< (fromRawXml bs)


data RMSIDResult = RMSIDResult
  { referenceNo         :: Text
  , txnLogID            :: Text
  , templateID          :: Text
  , createdOn           :: Text
  , txnDate             :: Text
  , riskLevel           :: Text
  , riskPercentage      :: Text
  , paymentStatus       :: Text
  , amount              :: Text
  , deviceID            :: Text
  , deviceProfileStatus :: Text
  , accuracy            :: Text
  , deviceType          :: Text
  , origin              :: Text
  , output              :: Output
  , affectedRules       :: Text
  , responseTime        :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON RMSIDResult where
    parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON RMSIDResult where
    toJSON = genericToJSON stripLensPrefixOptions

instance FromXml RMSIDResult where
  fromXml = pElement "RMSIDResult" p
    where
      p = do
        referenceNo <- pElement "ReferenceNo" pMText
        txnLogID <- pElement "TxnLogID" pMText
        templateID <- pElement "TemplateID" pMText
        createdOn <- pElement "CreatedOn" pMText
        txnDate <- pElement "TxnDate" pMText
        riskLevel <- pElement "RiskLevel" pMText
        riskPercentage <- pElement "RiskPercentage" pMText
        paymentStatus <- pElement "PaymentStatus" pMText
        amount <- pElement "Amount" pMText
        deviceID <- pElement "DeviceID" pMText
        deviceProfileStatus <- pElement "DeviceProfileStatus" pMText
        accuracy <- pElement "Accuracy" pMText
        deviceType <- pElement "DeviceType" pMText
        origin <- pElement "Origin" pMText
        output <- fromXml
        affectedRules <- pElement "AffectedRules" pMText
        responseTime <- pElement "ResponseTime" pMText
        pure RMSIDResult{..}

instance ToXml RMSIDResult where
  toXml RMSIDResult {..} =
    element "RMSIDResult" mempty $ concat
      [ element "ReferenceNo" mempty $ text $ TL.fromStrict referenceNo
      , element "TxnLogID" mempty $ text $ TL.fromStrict txnLogID
      , element "TemplateID" mempty $ text $ TL.fromStrict templateID
      , element "CreatedOn" mempty $ text $ TL.fromStrict createdOn
      , element "TxnDate" mempty $ text $ TL.fromStrict txnDate
      , element "RiskLevel" mempty $ text $ TL.fromStrict riskLevel
      , element "RiskPercentage" mempty $ text $ TL.fromStrict riskPercentage
      , element "PaymentStatus" mempty $ text $ TL.fromStrict paymentStatus
      , element "Amount" mempty $ text $ TL.fromStrict amount
      , element "DeviceID" mempty $ text $ TL.fromStrict deviceID
      , element "DeviceProfileStatus" mempty $ text $ TL.fromStrict deviceProfileStatus
      , element "Accuracy" mempty $ text $ TL.fromStrict accuracy
      , element "DeviceType" mempty $ text $ TL.fromStrict deviceType
      , element "Origin" mempty $ text $ TL.fromStrict origin
      , toXml  output
      , element "AffectedRules" mempty $ text $ TL.fromStrict affectedRules
      , element "ResponseTime" mempty $ text $ TL.fromStrict responseTime
      ]


data Output = Output
  { ipcity               :: Text
  , ipregion             :: Text
  , ipcountry            :: Text
  , ipisp                :: Text
  , iporg                :: Text
  , iplatitude           :: Text
  , iplongitude          :: Text
  , iphost               :: Text
  , proxyscore           :: Text
  , proxyfound           :: Text
  , proxytype            :: Text
  , spamscore            :: Text
  , binname              :: Text
  , bincity              :: Text
  , binregion            :: Text
  , bincountry           :: Text
  , billdistance         :: Text
  , billpostalcity       :: Text
  , billpostalregion     :: Text
  , billpostalcountry    :: Text
  , shipbilldistance     :: Text
  , shipdistance         :: Text
  , shippostalcity       :: Text
  , shippostalregion     :: Text
  , shippostalcountry    :: Text
  , freeemail            :: Text
  , emaildomain          :: Text
  , emaildomaincountry   :: Text
  , phoneserviceprovider :: Text
  , phonetype            :: Text
  , phonecity            :: Text
  , phoneregion          :: Text
  , phonecountry         :: Text
  , tsdeltamins          :: Text
  , tzdelta              :: Text
  , tzcountry            :: Text
  , ipaddress            :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Output where
    parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Output where
    toJSON = genericToJSON stripLensPrefixOptions

instance ToXml Output where
  toXml Output{..} =
    element "Output" mempty $ concat
      [ element "Ipcity" mempty $ text $ TL.fromStrict ipcity
      , element "Ipregion" mempty $ text $ TL.fromStrict ipregion
      , element "Ipcountry" mempty $ text $ TL.fromStrict ipcountry
      , element "Ipisp" mempty $ text $ TL.fromStrict ipisp
      , element "Iporg" mempty $ text $ TL.fromStrict iporg
      , element "Iplatitude" mempty $ text $ TL.fromStrict iplatitude
      , element "Iplongitude" mempty $ text $ TL.fromStrict iplongitude
      , element "Iphost" mempty $ text $ TL.fromStrict iphost
      , element "Proxyscore" mempty $ text $ TL.fromStrict proxyscore
      , element "Proxyfound" mempty $ text $ TL.fromStrict proxyfound
      , element "Proxytype" mempty $ text $ TL.fromStrict proxytype
      , element "Spamscore" mempty $ text $ TL.fromStrict spamscore
      , element "Binname" mempty $ text $ TL.fromStrict binname
      , element "Bincity" mempty $ text $ TL.fromStrict bincity
      , element "Binregion" mempty $ text $ TL.fromStrict binregion
      , element "Bincountry" mempty $ text $ TL.fromStrict bincountry
      , element "Billdistance" mempty $ text $ TL.fromStrict billdistance
      , element "Billpostalcity" mempty $ text $ TL.fromStrict billpostalcity
      , element "Billpostalregion" mempty $ text $ TL.fromStrict billpostalregion
      , element "Billpostalcountry" mempty $ text $ TL.fromStrict billpostalcountry
      , element "Shipbilldistance" mempty $ text $ TL.fromStrict shipbilldistance
      , element "Shipdistance" mempty $ text $ TL.fromStrict shipdistance
      , element "Shippostalcity" mempty $ text $ TL.fromStrict shippostalcity
      , element "Shippostalregion" mempty $ text $ TL.fromStrict shippostalregion
      , element "Shippostalcountry" mempty $ text $ TL.fromStrict shippostalcountry
      , element "Freeemail" mempty $ text $ TL.fromStrict freeemail
      , element "Emaildomain" mempty $ text $ TL.fromStrict emaildomain
      , element "Emaildomaincountry" mempty $ text $ TL.fromStrict emaildomaincountry
      , element "Phoneserviceprovider" mempty $ text $ TL.fromStrict phoneserviceprovider
      , element "Phonetype" mempty $ text $ TL.fromStrict phonetype
      , element "Phonecity" mempty $ text $ TL.fromStrict phonecity
      , element "Phoneregion" mempty $ text $ TL.fromStrict phoneregion
      , element "Phonecountry" mempty $ text $ TL.fromStrict phonecountry
      , element "Tsdeltamins" mempty $ text $ TL.fromStrict tsdeltamins
      , element "Tzdelta" mempty $ text $ TL.fromStrict tzdelta
      , element "Tzcountry" mempty $ text $ TL.fromStrict tzcountry
      , element "Ipaddress" mempty $ text $ TL.fromStrict ipaddress
      ]

instance FromXml Output where
  fromXml = pElement "Output" p
    where
      p = do
        ipcity <- pElement "Ipcity" pMText
        ipregion <- pElement "Ipregion" pMText
        ipcountry <- pElement "Ipcountry" pMText
        ipisp <- pElement "Ipisp" pMText
        iporg <- pElement "Iporg" pMText
        iplatitude <- pElement "Iplatitude" pMText
        iplongitude <- pElement "Iplongitude" pMText
        iphost <- pElement "Iphost" pMText
        proxyscore <- pElement "Proxyscore" pMText
        proxyfound <- pElement "Proxyfound" pMText
        proxytype <- pElement "Proxytype" pMText
        spamscore <- pElement "Spamscore" pMText
        binname <- pElement "Binname" pMText
        bincity <- pElement "Bincity" pMText
        binregion <- pElement "Binregion" pMText
        bincountry <- pElement "Bincountry" pMText
        billdistance <- pElement "Billdistance" pMText
        billpostalcity <- pElement "Billpostalcity" pMText
        billpostalregion <- pElement "Billpostalregion" pMText
        billpostalcountry <- pElement "Billpostalcountry" pMText
        shipbilldistance <- pElement "Shipbilldistance" pMText
        shipdistance <- pElement "Shipdistance" pMText
        shippostalcity <- pElement "Shippostalcity" pMText
        shippostalregion <- pElement "Shippostalregion" pMText
        shippostalcountry <- pElement "Shippostalcountry" pMText
        freeemail <- pElement "Freeemail" pMText
        emaildomain <- pElement "Emaildomain" pMText
        emaildomaincountry <- pElement "Emaildomaincountry" pMText
        phoneserviceprovider <- pElement "Phoneserviceprovider" pMText
        phonetype <- pElement "Phonetype" pMText
        phonecity <- pElement "Phonecity" pMText
        phoneregion <- pElement "Phoneregion" pMText
        phonecountry <- pElement "Phonecountry" pMText
        tsdeltamins <- pElement "Tsdeltamins" pMText
        tzdelta <- pElement "Tzdelta" pMText
        tzcountry <- pElement "Tzcountry" pMText
        ipaddress <- pElement "Ipaddress" pMText
        pure Output {..}

pMText :: Parser Text
pMText = do
  c <- pChildren
  pure $ TL.toStrict $ fromRight mempty $ runParser pText c


{-
<RMSIDResult foo ="hey" bar ="baz" >
	<ReferenceNo>888904076</ReferenceNo>
	<TxnLogID>1043-614755</TxnLogID>
	<TemplateID>54</TemplateID>
	<CreatedOn>2020-02-12 09:39:39</CreatedOn>
	<TxnDate>2020-02-12 15:09:39</TxnDate>
	<RiskLevel>Green</RiskLevel>
	<RiskPercentage>0</RiskPercentage>
	<PaymentStatus>Attempt</PaymentStatus>
	<Amount>21300.0</Amount>
	<DeviceID></DeviceID>
	<DeviceProfileStatus></DeviceProfileStatus>
	<Accuracy></Accuracy>
	<DeviceType></DeviceType>
	<Origin></Origin>
	<Output>
		<Ipcity></Ipcity>
		<Ipregion></Ipregion>
		<Ipcountry></Ipcountry>
		<Ipisp></Ipisp>
		<Iporg></Iporg>
		<Iplatitude></Iplatitude>
		<Iplongitude></Iplongitude>
		<Iphost></Iphost>
		<Proxyscore></Proxyscore>
		<Proxyfound></Proxyfound>
		<Proxytype></Proxytype>
		<Spamscore></Spamscore>
		<Binname>BANCO DEL PICHINCHA C.A.</Binname>
		<Bincity></Bincity>
		<Binregion></Binregion>
		<Bincountry>Ecuador</Bincountry>
		<Billdistance></Billdistance>
		<Billpostalcity></Billpostalcity>
		<Billpostalregion></Billpostalregion>
		<Billpostalcountry></Billpostalcountry>
		<Shipbilldistance></Shipbilldistance>
		<Shipdistance></Shipdistance>
		<Shippostalcity></Shippostalcity>
		<Shippostalregion></Shippostalregion>
		<Shippostalcountry></Shippostalcountry>
		<Freeemail>No</Freeemail>
		<Emaildomain>yopmail.com</Emaildomain>
		<Emaildomaincountry></Emaildomaincountry>
		<Phoneserviceprovider></Phoneserviceprovider>
		<Phonetype></Phonetype>
		<Phonecity></Phonecity>
		<Phoneregion></Phoneregion>
		<Phonecountry></Phonecountry>
		<Tsdeltamins></Tsdeltamins>
		<Tzdelta></Tzdelta>
		<Tzcountry></Tzcountry>
		<Ipaddress></Ipaddress>
	</Output>
	<AffectedRules>
	</AffectedRules>
	<ResponseTime>1666</ResponseTime>
</RMSIDResult>
-}
