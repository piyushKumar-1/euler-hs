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
