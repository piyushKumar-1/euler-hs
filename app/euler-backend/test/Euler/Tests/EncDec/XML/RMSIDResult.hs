module Euler.Tests.EncDec.XML.RMSIDResult
  ( spec

  )
  where

import           EulerHS.Prelude

import           Test.Hspec
import           Xmlbf

import           Euler.Common.Types.RMSIDResult

emptyOutput :: Output
emptyOutput = Output
  { _Ipcity               = "" -- :: Text
  , _Ipregion             = "" -- :: Text
  , _Ipcountry            = "" -- :: Text
  , _Ipisp                = "" -- :: Text
  , _Iporg                = "" -- :: Text
  , _Iplatitude           = "" -- :: Text
  , _Iplongitude          = "" -- :: Text
  , _Iphost               = "" -- :: Text
  , _Proxyscore           = "" -- :: Text
  , _Proxyfound           = "" -- :: Text
  , _Proxytype            = "" -- :: Text
  , _Spamscore            = "" -- :: Text
  , _Binname              = "" -- :: Text
  , _Bincity              = "" -- :: Text
  , _Binregion            = "" -- :: Text
  , _Bincountry           = "" -- :: Text
  , _Billdistance         = "" -- :: Text
  , _Billpostalcity       = "" -- :: Text
  , _Billpostalregion     = "" -- :: Text
  , _Billpostalcountry    = "" -- :: Text
  , _Shipbilldistance     = "" -- :: Text
  , _Shipdistance         = "" -- :: Text
  , _Shippostalcity       = "" -- :: Text
  , _Shippostalregion     = "" -- :: Text
  , _Shippostalcountry    = "" -- :: Text
  , _Freeemail            = "" -- :: Text
  , _Emaildomain          = "" -- :: Text
  , _Emaildomaincountry   = "" -- :: Text
  , _Phoneserviceprovider = "" -- :: Text
  , _Phonetype            = "" -- :: Text
  , _Phonecity            = "" -- :: Text
  , _Phoneregion          = "" -- :: Text
  , _Phonecountry         = "" -- :: Text
  , _Tsdeltamins          = "" -- :: Text
  , _Tzdelta              = "" -- :: Text
  , _Tzcountry            = "" -- :: Text
  , _Ipaddress            = "" -- :: Text
  }

filledOutput :: Output
filledOutput = Output
  { _Ipcity               = "someIpcity" -- :: Text
  , _Ipregion             = "someIpregion" -- :: Text
  , _Ipcountry            = "someIpcountry" -- :: Text
  , _Ipisp                = "someIpisp" -- :: Text
  , _Iporg                = "someIporg" -- :: Text
  , _Iplatitude           = "someIplatitude" -- :: Text
  , _Iplongitude          = "someIplongitude" -- :: Text
  , _Iphost               = "someIphost" -- :: Text
  , _Proxyscore           = "someProxyscore" -- :: Text
  , _Proxyfound           = "someProxyfound" -- :: Text
  , _Proxytype            = "someProxytype" -- :: Text
  , _Spamscore            = "someSpamscore" -- :: Text
  , _Binname              = "someBinname" -- :: Text
  , _Bincity              = "someBincity" -- :: Text
  , _Binregion            = "someBinregion" -- :: Text
  , _Bincountry           = "someBincountry" -- :: Text
  , _Billdistance         = "someBilldistance" -- :: Text
  , _Billpostalcity       = "someBillpostalcity" -- :: Text
  , _Billpostalregion     = "someBillpostalregion" -- :: Text
  , _Billpostalcountry    = "someBillpostalcountry" -- :: Text
  , _Shipbilldistance     = "someShipbilldistance" -- :: Text
  , _Shipdistance         = "someShipdistance" -- :: Text
  , _Shippostalcity       = "someShippostalcity" -- :: Text
  , _Shippostalregion     = "someShippostalregion" -- :: Text
  , _Shippostalcountry    = "someShippostalcountry" -- :: Text
  , _Freeemail            = "someFreeemail" -- :: Text
  , _Emaildomain          = "someEmaildomain" -- :: Text
  , _Emaildomaincountry   = "someEmaildomaincountry" -- :: Text
  , _Phoneserviceprovider = "somePhoneserviceprovider" -- :: Text
  , _Phonetype            = "somePhonetype" -- :: Text
  , _Phonecity            = "somePhonecity" -- :: Text
  , _Phoneregion          = "somePhoneregion" -- :: Text
  , _Phonecountry         = "somePhonecountry" -- :: Text
  , _Tsdeltamins          = "someTsdeltamins" -- :: Text
  , _Tzdelta              = "someTzdelta" -- :: Text
  , _Tzcountry            = "someTzcountry" -- :: Text
  , _Ipaddress            = "someIpaddress" -- :: Text
  }

emptyRMSIDResult :: RMSIDResult
emptyRMSIDResult = RMSIDResult
  { _ReferenceNo         = "" -- :: Text
  , _TxnLogID            = "" -- :: Text
  , _TemplateID          = "" -- :: Text
  , _CreatedOn           = "" -- :: Text
  , _TxnDate             = "" -- :: Text
  , _RiskLevel           = "" -- :: Text
  , _RiskPercentage      = "" -- :: Text
  , _PaymentStatus       = "" -- :: Text
  , _Amount              = "" -- :: Text
  , _DeviceID            = "" -- :: Text
  , _DeviceProfileStatus = "" -- :: Text
  , _Accuracy            = "" -- :: Text
  , _DeviceType          = "" -- :: Text
  , _Origin              = "" -- :: Text
  , _Output              = emptyOutput -- :: Output
  , _AffectedRules       = "" -- :: Text
  , _ResponseTime        = "" -- :: Text
  }

filledRMSIDResult :: RMSIDResult
filledRMSIDResult = RMSIDResult
  { _ReferenceNo         = "someReferenceNo" -- :: Text
  , _TxnLogID            = "someTxnLogID" -- :: Text
  , _TemplateID          = "someTemplateID" -- :: Text
  , _CreatedOn           = "someCreatedOn" -- :: Text
  , _TxnDate             = "someTxnDate" -- :: Text
  , _RiskLevel           = "someRiskLevel" -- :: Text
  , _RiskPercentage      = "someRiskPercentage" -- :: Text
  , _PaymentStatus       = "somePaymentStatus" -- :: Text
  , _Amount              = "someAmount" -- :: Text
  , _DeviceID            = "someDeviceID" -- :: Text
  , _DeviceProfileStatus = "someDeviceProfileStatus" -- :: Text
  , _Accuracy            = "someAccuracy" -- :: Text
  , _DeviceType          = "someDeviceType" -- :: Text
  , _Origin              = "someOrigin" -- :: Text
  , _Output              = filledOutput -- :: Output
  , _AffectedRules       = "someAffectedRules" -- :: Text
  , _ResponseTime        = "someResponseTime" -- :: Text
  }

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