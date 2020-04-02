{-# LANGUAGE QuasiQuotes #-}

module Euler.Tests.EncDec.XML.RMSIDResult
  ( spec
  )
  where

import           EulerHS.Prelude

import           Test.Hspec
import           Text.RawString.QQ
import           Xmlbf
import           Data.Time

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T

import           Euler.Common.Types.RMSIDResult
import           Euler.Product.Domain.TxnRiskCheck
import           Euler.Product.OLTP.Order.OrderStatus (makeRisk)


spec :: Spec
spec =
  describe "RMSIDResult XML encoding decoding" $ do

    it "filled data" $  do
      let filledRMSIDResultXML = toXml filledRMSIDResult
      let fromFilledXML = runParser fromXml filledRMSIDResultXML

      fromFilledXML `shouldBe` (Right filledRMSIDResult)

    it "empty data" $  do
      let filledRMSIDResultXML = toXml emptyRMSIDResult
      let fromFilledXML = runParser fromXml filledRMSIDResultXML

      fromFilledXML `shouldBe` (Right emptyRMSIDResult)

    it "1, makeRisk, provider is ebs" $  do
      let r1 = makeRisk (Just "ebs") $ txnRiskCheck $ T.pack rawRMSIDResult1

      r1 `shouldBe` risk1

    it "1, makeRisk, provider is not ebs" $  do
      let rNotEbs = makeRisk (Just "not ebs") $ txnRiskCheck $ T.pack rawRMSIDResult1

      rNotEbs `shouldBe` riskNotEbs

    it "2, makeRisk, provider is ebs" $  do
      let r2 = makeRisk (Just "ebs") $ txnRiskCheck $ T.pack rawRMSIDResult2

      r2 `shouldBe` risk2

    it "2, makeRisk, provider is not ebs" $  do
      let rNotEbs = makeRisk (Just "not ebs") $ txnRiskCheck $ T.pack rawRMSIDResult2

      rNotEbs `shouldBe` riskNotEbs


emptyOutput :: Output
emptyOutput = Output
  { ipcity               = "" -- :: Text
  , ipregion             = "" -- :: Text
  , ipcountry            = "" -- :: Text
  , ipisp                = "" -- :: Text
  , iporg                = "" -- :: Text
  , iplatitude           = "" -- :: Text
  , iplongitude          = "" -- :: Text
  , iphost               = "" -- :: Text
  , proxyscore           = "" -- :: Text
  , proxyfound           = "" -- :: Text
  , proxytype            = "" -- :: Text
  , spamscore            = "" -- :: Text
  , binname              = "" -- :: Text
  , bincity              = "" -- :: Text
  , binregion            = "" -- :: Text
  , bincountry           = "" -- :: Text
  , billdistance         = "" -- :: Text
  , billpostalcity       = "" -- :: Text
  , billpostalregion     = "" -- :: Text
  , billpostalcountry    = "" -- :: Text
  , shipbilldistance     = "" -- :: Text
  , shipdistance         = "" -- :: Text
  , shippostalcity       = "" -- :: Text
  , shippostalregion     = "" -- :: Text
  , shippostalcountry    = "" -- :: Text
  , freeemail            = "" -- :: Text
  , emaildomain          = "" -- :: Text
  , emaildomaincountry   = "" -- :: Text
  , phoneserviceprovider = "" -- :: Text
  , phonetype            = "" -- :: Text
  , phonecity            = "" -- :: Text
  , phoneregion          = "" -- :: Text
  , phonecountry         = "" -- :: Text
  , tsdeltamins          = "" -- :: Text
  , tzdelta              = "" -- :: Text
  , tzcountry            = "" -- :: Text
  , ipaddress            = "" -- :: Text
  }

filledOutput :: Output
filledOutput = Output
  { ipcity               = "someIpcity" -- :: Text
  , ipregion             = "someIpregion" -- :: Text
  , ipcountry            = "someIpcountry" -- :: Text
  , ipisp                = "someIpisp" -- :: Text
  , iporg                = "someIporg" -- :: Text
  , iplatitude           = "someIplatitude" -- :: Text
  , iplongitude          = "someIplongitude" -- :: Text
  , iphost               = "someIphost" -- :: Text
  , proxyscore           = "someProxyscore" -- :: Text
  , proxyfound           = "someProxyfound" -- :: Text
  , proxytype            = "someProxytype" -- :: Text
  , spamscore            = "someSpamscore" -- :: Text
  , binname              = "someBinname" -- :: Text
  , bincity              = "someBincity" -- :: Text
  , binregion            = "someBinregion" -- :: Text
  , bincountry           = "someBincountry" -- :: Text
  , billdistance         = "someBilldistance" -- :: Text
  , billpostalcity       = "someBillpostalcity" -- :: Text
  , billpostalregion     = "someBillpostalregion" -- :: Text
  , billpostalcountry    = "someBillpostalcountry" -- :: Text
  , shipbilldistance     = "someShipbilldistance" -- :: Text
  , shipdistance         = "someShipdistance" -- :: Text
  , shippostalcity       = "someShippostalcity" -- :: Text
  , shippostalregion     = "someShippostalregion" -- :: Text
  , shippostalcountry    = "someShippostalcountry" -- :: Text
  , freeemail            = "someFreeemail" -- :: Text
  , emaildomain          = "someEmaildomain" -- :: Text
  , emaildomaincountry   = "someEmaildomaincountry" -- :: Text
  , phoneserviceprovider = "somePhoneserviceprovider" -- :: Text
  , phonetype            = "somePhonetype" -- :: Text
  , phonecity            = "somePhonecity" -- :: Text
  , phoneregion          = "somePhoneregion" -- :: Text
  , phonecountry         = "somePhonecountry" -- :: Text
  , tsdeltamins          = "someTsdeltamins" -- :: Text
  , tzdelta              = "someTzdelta" -- :: Text
  , tzcountry            = "someTzcountry" -- :: Text
  , ipaddress            = "someIpaddress" -- :: Text
  }

emptyRMSIDResult :: RMSIDResult
emptyRMSIDResult = RMSIDResult
  { referenceNo         = "" -- :: Text
  , txnLogID            = "" -- :: Text
  , templateID          = "" -- :: Text
  , createdOn           = "" -- :: Text
  , txnDate             = "" -- :: Text
  , riskLevel           = "" -- :: Text
  , riskPercentage      = "" -- :: Text
  , paymentStatus       = "" -- :: Text
  , amount              = "" -- :: Text
  , deviceID            = "" -- :: Text
  , deviceProfileStatus = "" -- :: Text
  , accuracy            = "" -- :: Text
  , deviceType          = "" -- :: Text
  , origin              = "" -- :: Text
  , output              = emptyOutput -- :: Output
  , affectedRules       = "" -- :: Text
  , responseTime        = "" -- :: Text
  }

filledRMSIDResult :: RMSIDResult
filledRMSIDResult = RMSIDResult
  { referenceNo         = "someReferenceNo" -- :: Text
  , txnLogID            = "someTxnLogID" -- :: Text
  , templateID          = "someTemplateID" -- :: Text
  , createdOn           = "someCreatedOn" -- :: Text
  , txnDate             = "someTxnDate" -- :: Text
  , riskLevel           = "someRiskLevel" -- :: Text
  , riskPercentage      = "someRiskPercentage" -- :: Text
  , paymentStatus       = "somePaymentStatus" -- :: Text
  , amount              = "someAmount" -- :: Text
  , deviceID            = "someDeviceID" -- :: Text
  , deviceProfileStatus = "someDeviceProfileStatus" -- :: Text
  , accuracy            = "someAccuracy" -- :: Text
  , deviceType          = "someDeviceType" -- :: Text
  , origin              = "someOrigin" -- :: Text
  , output              = filledOutput -- :: Output
  , affectedRules       = "someAffectedRules" -- :: Text
  , responseTime        = "someResponseTime" -- :: Text
  }

txnRiskCheck :: Text -> TxnRiskCheck
txnRiskCheck completeResp = TxnRiskCheck
  { id                              = TxnRiskCheckPId 1
  , completeResponse                = completeResp
  , dateCreated                     = LocalTime (fromGregorian 2020 1 12) (TimeOfDay 2 13 0)
  , flagged                         = Just True
  , lastUpdated                     = LocalTime (fromGregorian 2020 3 22) (TimeOfDay 15 3 10)
  , recommendedAction               = Just "recommendedAction"
  , resultJson                      = Just "resultJson"
  , riskManagementAccountId         = 2
  , txnDetailId                     = 9
  , message                         = Just "message"
  , status                          = Just "status"
  , riskStatus                      = Just "riskStatus"
  , domestic                        = Just False
  , invocationMode                  = Just "invocationMode"
  , paymentStatusUpdateResponseCode = Just "paymentStatusUpdateResponseCode"
  , paymentStatusUpdated            = Just True
  }


rawRMSIDResult1 :: String
rawRMSIDResult1 = [r|
<RMSIDResult>
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
|]

risk1 :: Risk
risk1 = Risk
  { provider = Just "ebs"
  , status = Just "status"
  , message = Just "message"
  , flagged = Just True
  , recommendedAction = Just "recommendedAction"
  , ebsRiskLevel = "Green"
  , ebsPaymentStatus = Just "riskStatus"
  , ebsBinCountry = "Ecuador"
  , ebsRiskPercentage = "0"
  }

riskNotEbs :: Risk
riskNotEbs = Risk
  { provider = Just "not ebs"
  , status = Just "status"
  , message = Just "message"
  , flagged = Just True
  , recommendedAction = Just "recommendedAction"
  , ebsRiskLevel = ""
  , ebsPaymentStatus = Nothing
  , ebsBinCountry = ""
  , ebsRiskPercentage = ""
  }


rawRMSIDResult2 :: String
rawRMSIDResult2 = [r|
<RMSIDResult>
	<ReferenceNo>888904060</ReferenceNo>
	<TxnLogID>1043-614753</TxnLogID>
	<TemplateID>54</TemplateID>
	<CreatedOn>2020-02-12 09:31:45</CreatedOn>
	<TxnDate>2020-02-12 15:01:45</TxnDate>
	<RiskLevel>Green</RiskLevel>
	<RiskPercentage>0</RiskPercentage>
	<PaymentStatus>Attempt</PaymentStatus>
	<Amount>82000.0</Amount>
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
		<Binname></Binname>
		<Bincity></Bincity>
		<Binregion></Binregion>
		<Bincountry>United States</Bincountry>
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
	<ResponseTime>2814</ResponseTime>
</RMSIDResult>
|]

risk2 :: Risk
risk2 = Risk
  { provider = Just "ebs"
  , status = Just "status"
  , message = Just "message"
  , flagged = Just True
  , recommendedAction = Just "recommendedAction"
  , ebsRiskLevel = "Green"
  , ebsPaymentStatus = Just "riskStatus"
  , ebsBinCountry = "United States"
  , ebsRiskPercentage = "0"
  }

