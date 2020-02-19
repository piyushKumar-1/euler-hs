module Euler.Common.Types.RMSIDResult
  ( RMSIDResult (..)
  , Output (..)
  )
  where

import EulerHS.Prelude

import qualified Data.Text.Lazy as TL
import Xmlbf

data RMSIDResult = RMSIDResult
  { _ReferenceNo         :: Text
  , _TxnLogID            :: Text
  , _TemplateID          :: Text
  , _CreatedOn           :: Text
  , _TxnDate             :: Text
  , _RiskLevel           :: Text
  , _RiskPercentage      :: Text
  , _PaymentStatus       :: Text
  , _Amount              :: Text
  , _DeviceID            :: Text
  , _DeviceProfileStatus :: Text
  , _Accuracy            :: Text
  , _DeviceType          :: Text
  , _Origin              :: Text
  , _Output              :: Output
  , _AffectedRules       :: Text
  , _ResponseTime        :: Text
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
        _ReferenceNo <- pElement "ReferenceNo" pMText
        _TxnLogID <- pElement "TxnLogID" pMText
        _TemplateID <- pElement "TemplateID" pMText
        _CreatedOn <- pElement "CreatedOn" pMText
        _TxnDate <- pElement "TxnDate" pMText
        _RiskLevel <- pElement "RiskLevel" pMText
        _RiskPercentage <- pElement "RiskPercentage" pMText
        _PaymentStatus <- pElement "PaymentStatus" pMText
        _Amount <- pElement "Amount" pMText
        _DeviceID <- pElement "DeviceID" pMText
        _DeviceProfileStatus <- pElement "DeviceProfileStatus" pMText
        _Accuracy <- pElement "Accuracy" pMText
        _DeviceType <- pElement "DeviceType" pMText
        _Origin <- pElement "Origin" pMText
        _Output <- fromXml
        _AffectedRules <- pElement "AffectedRules" pMText
        _ResponseTime <- pElement "ResponseTime" pMText
        pure RMSIDResult{..}

instance ToXml RMSIDResult where
  toXml RMSIDResult {..} =
    element "RMSIDResult" mempty $ concat
      [ element "ReferenceNo" mempty $ text $ TL.fromStrict _ReferenceNo
      , element "TxnLogID" mempty $ text $ TL.fromStrict _TxnLogID
      , element "TemplateID" mempty $ text $ TL.fromStrict _TemplateID
      , element "CreatedOn" mempty $ text $ TL.fromStrict _CreatedOn
      , element "TxnDate" mempty $ text $ TL.fromStrict _TxnDate
      , element "RiskLevel" mempty $ text $ TL.fromStrict _RiskLevel
      , element "RiskPercentage" mempty $ text $ TL.fromStrict _RiskPercentage
      , element "PaymentStatus" mempty $ text $ TL.fromStrict _PaymentStatus
      , element "Amount" mempty $ text $ TL.fromStrict _Amount
      , element "DeviceID" mempty $ text $ TL.fromStrict _DeviceID
      , element "DeviceProfileStatus" mempty $ text $ TL.fromStrict _DeviceProfileStatus
      , element "Accuracy" mempty $ text $ TL.fromStrict _Accuracy
      , element "DeviceType" mempty $ text $ TL.fromStrict _DeviceType
      , element "Origin" mempty $ text $ TL.fromStrict _Origin
      , toXml  _Output
      , element "AffectedRules" mempty $ text $ TL.fromStrict _AffectedRules
      , element "ResponseTime" mempty $ text $ TL.fromStrict _ResponseTime
      ]


data Output = Output
  { _Ipcity               :: Text
  , _Ipregion             :: Text
  , _Ipcountry            :: Text
  , _Ipisp                :: Text
  , _Iporg                :: Text
  , _Iplatitude           :: Text
  , _Iplongitude          :: Text
  , _Iphost               :: Text
  , _Proxyscore           :: Text
  , _Proxyfound           :: Text
  , _Proxytype            :: Text
  , _Spamscore            :: Text
  , _Binname              :: Text
  , _Bincity              :: Text
  , _Binregion            :: Text
  , _Bincountry           :: Text
  , _Billdistance         :: Text
  , _Billpostalcity       :: Text
  , _Billpostalregion     :: Text
  , _Billpostalcountry    :: Text
  , _Shipbilldistance     :: Text
  , _Shipdistance         :: Text
  , _Shippostalcity       :: Text
  , _Shippostalregion     :: Text
  , _Shippostalcountry    :: Text
  , _Freeemail            :: Text
  , _Emaildomain          :: Text
  , _Emaildomaincountry   :: Text
  , _Phoneserviceprovider :: Text
  , _Phonetype            :: Text
  , _Phonecity            :: Text
  , _Phoneregion          :: Text
  , _Phonecountry         :: Text
  , _Tsdeltamins          :: Text
  , _Tzdelta              :: Text
  , _Tzcountry            :: Text
  , _Ipaddress            :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Output where
    parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Output where
    toJSON = genericToJSON stripLensPrefixOptions

instance ToXml Output where
  toXml Output{..} =
    element "Output" mempty $ concat
      [ element "Ipcity" mempty $ text $ TL.fromStrict _Ipcity
      , element "Ipregion" mempty $ text $ TL.fromStrict _Ipregion
      , element "Ipcountry" mempty $ text $ TL.fromStrict _Ipcountry
      , element "Ipisp" mempty $ text $ TL.fromStrict _Ipisp
      , element "Iporg" mempty $ text $ TL.fromStrict _Iporg
      , element "Iplatitude" mempty $ text $ TL.fromStrict _Iplatitude
      , element "Iplongitude" mempty $ text $ TL.fromStrict _Iplongitude
      , element "Iphost" mempty $ text $ TL.fromStrict _Iphost
      , element "Proxyscore" mempty $ text $ TL.fromStrict _Proxyscore
      , element "Proxyfound" mempty $ text $ TL.fromStrict _Proxyfound
      , element "Proxytype" mempty $ text $ TL.fromStrict _Proxytype
      , element "Spamscore" mempty $ text $ TL.fromStrict _Spamscore
      , element "Binname" mempty $ text $ TL.fromStrict _Binname
      , element "Bincity" mempty $ text $ TL.fromStrict _Bincity
      , element "Binregion" mempty $ text $ TL.fromStrict _Binregion
      , element "Bincountry" mempty $ text $ TL.fromStrict _Bincountry
      , element "Billdistance" mempty $ text $ TL.fromStrict _Billdistance
      , element "Billpostalcity" mempty $ text $ TL.fromStrict _Billpostalcity
      , element "Billpostalregion" mempty $ text $ TL.fromStrict _Billpostalregion
      , element "Billpostalcountry" mempty $ text $ TL.fromStrict _Billpostalcountry
      , element "Shipbilldistance" mempty $ text $ TL.fromStrict _Shipbilldistance
      , element "Shipdistance" mempty $ text $ TL.fromStrict _Shipdistance
      , element "Shippostalcity" mempty $ text $ TL.fromStrict _Shippostalcity
      , element "Shippostalregion" mempty $ text $ TL.fromStrict _Shippostalregion
      , element "Shippostalcountry" mempty $ text $ TL.fromStrict _Shippostalcountry
      , element "Freeemail" mempty $ text $ TL.fromStrict _Freeemail
      , element "Emaildomain" mempty $ text $ TL.fromStrict _Emaildomain
      , element "Emaildomaincountry" mempty $ text $ TL.fromStrict _Emaildomaincountry
      , element "Phoneserviceprovider" mempty $ text $ TL.fromStrict _Phoneserviceprovider
      , element "Phonetype" mempty $ text $ TL.fromStrict _Phonetype
      , element "Phonecity" mempty $ text $ TL.fromStrict _Phonecity
      , element "Phoneregion" mempty $ text $ TL.fromStrict _Phoneregion
      , element "Phonecountry" mempty $ text $ TL.fromStrict _Phonecountry
      , element "Tsdeltamins" mempty $ text $ TL.fromStrict _Tsdeltamins
      , element "Tzdelta" mempty $ text $ TL.fromStrict _Tzdelta
      , element "Tzcountry" mempty $ text $ TL.fromStrict _Tzcountry
      , element "Ipaddress" mempty $ text $ TL.fromStrict _Ipaddress
      ]

instance FromXml Output where
  fromXml = pElement "Output" p
    where
      p = do
        _Ipcity <- pElement "Ipcity" pMText
        _Ipregion <- pElement "Ipregion" pMText
        _Ipcountry <- pElement "Ipcountry" pMText
        _Ipisp <- pElement "Ipisp" pMText
        _Iporg <- pElement "Iporg" pMText
        _Iplatitude <- pElement "Iplatitude" pMText
        _Iplongitude <- pElement "Iplongitude" pMText
        _Iphost <- pElement "Iphost" pMText
        _Proxyscore <- pElement "Proxyscore" pMText
        _Proxyfound <- pElement "Proxyfound" pMText
        _Proxytype <- pElement "Proxytype" pMText
        _Spamscore <- pElement "Spamscore" pMText
        _Binname <- pElement "Binname" pMText
        _Bincity <- pElement "Bincity" pMText
        _Binregion <- pElement "Binregion" pMText
        _Bincountry <- pElement "Bincountry" pMText
        _Billdistance <- pElement "Billdistance" pMText
        _Billpostalcity <- pElement "Billpostalcity" pMText
        _Billpostalregion <- pElement "Billpostalregion" pMText
        _Billpostalcountry <- pElement "Billpostalcountry" pMText
        _Shipbilldistance <- pElement "Shipbilldistance" pMText
        _Shipdistance <- pElement "Shipdistance" pMText
        _Shippostalcity <- pElement "Shippostalcity" pMText
        _Shippostalregion <- pElement "Shippostalregion" pMText
        _Shippostalcountry <- pElement "Shippostalcountry" pMText
        _Freeemail <- pElement "Freeemail" pMText
        _Emaildomain <- pElement "Emaildomain" pMText
        _Emaildomaincountry <- pElement "Emaildomaincountry" pMText
        _Phoneserviceprovider <- pElement "Phoneserviceprovider" pMText
        _Phonetype <- pElement "Phonetype" pMText
        _Phonecity <- pElement "Phonecity" pMText
        _Phoneregion <- pElement "Phoneregion" pMText
        _Phonecountry <- pElement "Phonecountry" pMText
        _Tsdeltamins <- pElement "Tsdeltamins" pMText
        _Tzdelta <- pElement "Tzdelta" pMText
        _Tzcountry <- pElement "Tzcountry" pMText
        _Ipaddress <- pElement "Ipaddress" pMText
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