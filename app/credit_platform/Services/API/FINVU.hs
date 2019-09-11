{-

  AA  - Account Aggregator
  FIP - Financial Information Providers
  FIU - Financial Information Users 

  Account Aggregator Ecosystem API Specifications
  https://api.rebit.org.in/list

  Account Aggregator API description
  https://swagger-ui.rebit.org.in/?url=https://s3.ap-south-1.amazonaws.com/api-spec-prod/api_specifications/account_aggregator/AA_1_1_1.yaml#/

  Financial Information Provider API description
  https://swagger-ui.rebit.org.in/?url=https://s3.ap-south-1.amazonaws.com/api-spec-prod/api_specifications/account_aggregator/FIP_1_1_1.yaml

  FIU Callback API description
  https://swagger-ui.rebit.org.in/?url=https://s3.ap-south-1.amazonaws.com/api-spec-prod/api_specifications/account_aggregator/FIU_1_1_1.yaml

-}


module Services.API.FINVU where

import EulerHS.Prelude

import Data.Aeson (Options(..), defaultOptions)
import Xmlbf

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy      as LT

aaAesonOptions :: Options
aaAesonOptions = defaultOptions { fieldLabelModifier = drop 1 }

class ToCDATAText a where
  toCDATAText :: a -> Text

-- ### POST FIP Data - Adding User Data ###
-- Req in xml
type UserAccountInfo = [UserAccount]

instance ToXml [UserAccount] where
  toXml uas = 
    Xmlbf.element "UserAccountInfo"  (HM.empty)  (concatMap toXml uas)

data UserAccount = UserAccount
  {  _accountRefNo    :: Text -- "RefAXIS0001",
  ,  _accountNo       :: Text -- "AXIS11111111",
  ,  _accountTypeEnum :: Text -- "SAVINGS",
  ,  _FIType          :: Text -- "DEPOSIT",
  , _Identifiers      :: Identifiers
  } deriving (Generic, Show, Eq)

instance FromJSON UserAccount where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON UserAccount where
    toJSON = genericToJSON aaAesonOptions

instance ToXml UserAccount where
  toXml UserAccount{..} =
    Xmlbf.element "UserAccount"  (HM.singleton "accountRefNo" _accountRefNo
    <> HM.singleton "accountNo" _accountNo
    <> HM.singleton "accountTypeEnum" _accountTypeEnum
    <> HM.singleton "FIType" _FIType
    <> HM.empty)  (toXml _Identifiers)

data Identifiers = Identifiers
  { _pan    :: Text --"BIYPS2601E",
  , _mobile :: Text --"9620902139",
  , _email  :: Text --"magizhan.selvan@juspay.in",
  , _aadhar :: Text --"830692432388"
  } deriving (Generic, Show, Eq)

instance FromJSON Identifiers where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON Identifiers where
    toJSON = genericToJSON aaAesonOptions

instance ToXml Identifiers where
  toXml Identifiers {..} = 
    Xmlbf.element "Identifiers"  (HM.singleton "pan" _pan
    <> HM.singleton "mobile" _mobile
    <> HM.singleton "email" _email
    <> HM.singleton "aadhar" _aadhar
    <> HM.empty)  []

-- Resp
-- In xml text
{-

User account(s) added.

	Account Reference Number : RefAXIS0001
	Account Number : AXIS11111111
	(It use in Account Discovery.)


	Account Reference Number : RefKOTK0002
	Account Number : KOTK22222222
	(It use in Account Discovery.)


	Account Reference Number : RefICIC0001
	Account Number : ICIC11111111
	(It use in Account Discovery.)

 Please save this information for FI request API calls.

-}


-- ### POST FIP Data - Adding User Transaction ###
-- Req xml with CDATA
data UserAccountTrans = UserAccountTrans
  { _UserAccount :: UserAccountT
  } deriving (Generic, Show, Eq)

instance ToXml UserAccountTrans where
  toXml UserAccountTrans{..} =
    Xmlbf.element "UserAccountTrans"  (HM.empty) $ toXml _UserAccount

data UserAccountT = UserAccountT
  { _accountRefNo    :: Text
  , _accountNo       :: Text
  , _FIType          :: Text
  , _accountTypeEnum :: Text
  , _accountData     :: Account -- CDATA
  } deriving (Generic, Show, Eq)

instance ToXml UserAccountT where
  toXml UserAccountT{..} =
    Xmlbf.element "UserAccount"  HM.empty
      ( (Xmlbf.element "accountRefNo" HM.empty (text $ LT.fromStrict _accountRefNo))
      <> (Xmlbf.element "accountNo" HM.empty ( text $ LT.fromStrict _accountNo ))
      <> (Xmlbf.element "FIType" HM.empty ( text $ LT.fromStrict _FIType ))
      <> (Xmlbf.element "accountTypeEnum" HM.empty ( text $ LT.fromStrict _accountTypeEnum))
      <> (Xmlbf.element "accountData" HM.empty ( text $ LT.fromStrict ("<![CDATA[" <> toCDATAText _accountData <> "]]>")))
      )

data Account = Account
  { _id           :: Text
  , _type         :: Text
  , _summary      :: Summary
  , _transactions :: Transactions
  } deriving (Generic, Show, Eq)

instance ToCDATAText Account where
  toCDATAText Account{..} =
    "<Account " <> accountXMLCDATALinks
    <> " id=" <> _id
    <> "\" type=" <> _type
    <> "\">"
    <> toCDATAText _summary
    <> toCDATAText _transactions
    <> "</Account>"
    where
      accountXMLCDATALinks :: Text
      accountXMLCDATALinks = "xmlns=\"http://api.rebit.org.in/FISchema/deposit\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://api.rebit.org.in/FISchema/deposit ../FISchema/deposit.xsd\""

data Summary = Summary
  { _type           :: Text --"savings"
  , _openingDate    :: Text -- "2002-09-24"
  , _ifscCode       :: Text -- "AIRP0000001"
  , _micrCode       :: Text -- ""
  , _branch         :: Text -- ""
  , _facility       :: Text -- "OD"
  , _currentBalance :: Text -- "100000"
  , _currentODLimit :: Text -- ""
  , _Holders        :: Holders
  } deriving (Generic, Show, Eq)

instance ToCDATAText Summary where
  toCDATAText Summary{..} =
    "<Summary type=" <> _type
    <> "\" openingDate=" <> _openingDate
    <> "\" ifscCode=" <> _ifscCode
    <> "\" micrCode=" <> _micrCode
    <> "\" branch=" <> _branch
    <> "\" facility=" <> _facility
    <> "\" currentBalance=" <> _currentBalance
    <> "\" currentODLimit=" <> _currentODLimit
    <> "\">"
    <> toCDATAText _Holders
    <> "</Summary>"

data Holders = Holders
  { _type    :: Text -- "single"
  , _holders :: [Holder]
  } deriving (Generic, Show, Eq)

instance ToCDATAText Holders where
  toCDATAText Holders{..} =
    "<Holders type=" <> _type 
    <>  "\">"
    <> (foldl1 (<>) $ toCDATAText <$> _holders)
    <> "</Holders>"

data Holder = Holder
    { _order    :: Text -- "1|2" 
    , _name     :: Text -- "test" 
    , _aadhaar  :: Text -- "830692432388" 
    , _mobile   :: Text -- "9620902139" 
    , _landline :: Text -- "" 
    , _address  :: Text -- "Bangalore" 
    , _email    :: Text -- "magizhan.selvan@juspay.in"
    , _pan      :: Text -- "BIYPS2601E"
    } deriving (Generic, Show, Eq)

instance ToCDATAText Holder where
  toCDATAText Holder{..} =
    "<Holder order=" <> _order
    <> "\" name=" <> _name
    <> "\" aadhaar=" <> _aadhaar
    <> "\" mobile=" <> _mobile
    <> "\" landline=" <> _landline
    <> "\" address=" <> _address
    <> "\" email=" <> _email
    <> "\" pan=" <> _pan
    <> "\"/>"

data Transactions = Transactions
    { _startDate    :: Text -- "2002-09-24"
    , _endDate      :: Text -- "2002-09-24"
    , _Transactions :: [Transaction]
    } deriving (Generic, Show, Eq)

instance ToCDATAText Transactions where
  toCDATAText Transactions{..} =
    "<Transactions startDate=" <> _startDate
    <> "\" endDate=" <> _endDate
    <> "\">"
    <> (foldl1 (<>) $ toCDATAText <$> _Transactions)
    <> "</Transactions>"

data Transaction = Transaction
  { _id        :: Text -- "bf58d0e8-cad0-4659-87cf-3c97642fd6d6"
  , _date      :: Text --"2002-09-24"
  , _narration :: Text --"MMT/Ref815116555662/68012604153"
  , _reference :: Text --"12222222"
  , _amount    :: Text -- "800"
  , _balance   :: Text --"100000"
  } deriving (Generic, Show, Eq)

instance ToCDATAText Transaction where
  toCDATAText Transaction{..} = 
    "<Transaction id=" <> _id
     <>"\" date=" <> _date
     <>"\" narration=" <> _narration
     <>"\" reference=" <> _reference
     <>"\" amount=" <> _amount
     <>"\" balance=" <> _balance
     <> "\"/>"



-- Resp
-- xml text "User account transation added."

-- ### POST Discover Account ###
--Req
data Customer = Customer
  { _id          :: Text -- "cust123"
  , _Identifiers :: [DiscoverIdentifier]
  } deriving (Generic, Show, Eq)

instance FromJSON Customer where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON Customer where
    toJSON = genericToJSON aaAesonOptions

data DiscoverIdentifier = DiscoverIdentifier
  { _category :: Text -- "STRONG"
  , _type     :: Text -- "MOBILE"
  , _value    :: Text -- "9620902139"
  } deriving (Generic, Show, Eq)

instance FromJSON DiscoverIdentifier where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DiscoverIdentifier where
    toJSON = genericToJSON aaAesonOptions

data DiscoverAccountRequest = DiscoverAccountRequest
  { _ver       :: Text --"1.0",
  , _timestamp :: Text --"2019-04-29T04:04:44.955Z",
  , _txnid     :: Text --"8399263c-4a15-11e8-bcd1-0277a9fbfasc"
  , _Customer  :: Customer
  , _FITypes   :: [Text] --"DEPOSIT"
  } deriving (Generic, Show, Eq)

instance FromJSON DiscoverAccountRequest where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DiscoverAccountRequest where
    toJSON = genericToJSON aaAesonOptions

--Resp
data DiscoveredAccount = DiscoveredAccount
  { _FIType          :: Text -- "DEPOSIT"
  , _accType         :: Text -- "SAVINGS"
  , _accRefNumber    :: Text -- "RefAXIS0001"
  , _maskedAccNumber :: Text -- "XXXXXXXX1111"
  } deriving (Generic, Show, Eq)

instance FromJSON DiscoveredAccount where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DiscoveredAccount where
    toJSON = genericToJSON aaAesonOptions

data DiscoverAccountResponse = DiscoverAccountResponse
  { _ver                :: Text -- "1.0"
  , _timestamp          :: Text -- "2019-09-09T06:30:23.749+0000"
  , _txnid              :: Text -- "8399263c-4a15-11e8-bcd1-0277a9fbfasc"
  , _DiscoveredAccounts :: [DiscoveredAccount]
  } deriving (Generic, Show, Eq)

instance FromJSON DiscoverAccountResponse where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DiscoverAccountResponse where
    toJSON = genericToJSON aaAesonOptions

-- ### POST Account Linking ###
-- Req

data AccLinkingRequest = AccLinkingRequest
  { _ver       :: Text -- "1.0"
  , _timestamp :: Text -- "2018-05-09T17:51:18.412Z"
  , _txnid     :: Text -- "8399263c-4a15-11e8-bcd1-0277a9fbfasc"
  , _Customer  :: LinkCustomer
  } deriving (Generic, Show, Eq)

instance FromJSON AccLinkingRequest where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON AccLinkingRequest where
    toJSON = genericToJSON aaAesonOptions

data LinkCustomer = LinkCustomer
  { _id       :: Text -- "cust123"
  , _Accounts ::[LinkAccount]
  } deriving (Generic, Show, Eq)

instance FromJSON LinkCustomer where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON LinkCustomer where
    toJSON = genericToJSON aaAesonOptions

data LinkAccount = LinkAccount
  { _FIType          :: Text -- "DEPOSIT"
  , _accType         :: Text -- "SAVINGS"
  , _accRefNumber    :: Text -- "RefHDFC0001"
  , _maskedAccNumber :: Text -- "XXXXXXXX1111"
  } deriving (Generic, Show, Eq)

instance FromJSON LinkAccount where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON LinkAccount where
    toJSON = genericToJSON aaAesonOptions

-- Resp
data AccLinkingResponse = AccLinkingResponse
  { _ver               :: Text -- "1.0"
  , _timestamp         :: Text -- "2019-09-09T06:57:41.542+0000"
  , _txnid             :: Text -- "8399263c-4a15-11e8-bcd1-0277a9fbfasc"
  , _AuthenticatorType :: Text -- "TOKEN"
  , _RefNumber         :: Text -- "ceb16e3a-185d-489c-8d1f-fb45119b2e5b"
  } deriving (Generic, Show, Eq)

instance FromJSON AccLinkingResponse where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON AccLinkingResponse where
    toJSON = genericToJSON aaAesonOptions

-- ### POST FI Request ###
-- Req

data Consent = Consent
  { _id               :: Text -- "0fee9e18-cf95-11e9-bb65-2a2ae2dbcce4"
  , _digitalSignature :: Text -- "Digital signature of the consentDetail section in the consent Artefact"
  } deriving (Generic, Show, Eq)

instance FromJSON Consent where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON Consent where
    toJSON = genericToJSON aaAesonOptions

data FIDataRange = FIDataRange
  { _from :: Text -- "2001-11-27T06:26:29.761Z"
  , _to   :: Text -- "2018-12-27T06:26:29.761Z"
  } deriving (Generic, Show, Eq)

instance FromJSON FIDataRange where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON FIDataRange where
    toJSON = genericToJSON aaAesonOptions

data KeyMaterial = KeyMaterial
  { _cryptoAlg   :: Text -- "ECDHE"
  , _curve       :: Text -- "Curve25519"
  , _params      :: Text -- "string"
  , _DHPublicKey :: DHPublicKey
  , _Nonce       :: Text -- "32b32a1a-44cc-4fd0-bad2-86114912732e"
  , _Signature   :: Text -- "AA signature"
  } deriving (Generic, Show, Eq)

instance FromJSON KeyMaterial where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON KeyMaterial where
    toJSON = genericToJSON aaAesonOptions

data DHPublicKey = DHPublicKey
  { _expiry     :: Text -- "2019-06-01T09:58:50.505Z"
  , _Parameters :: Text -- "Param 1"
  , _KeyValue   :: Text -- "MAY1oK05Ga/MM3vMitixfSmuf0xm6/ROw2tle09tt1M="
  } deriving (Generic, Show, Eq)

instance FromJSON DHPublicKey where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DHPublicKey where
    toJSON = genericToJSON aaAesonOptions

data FIRequest = FIRequest
  { _ver         :: Text -- "1.0"
  , _timestamp   :: Text -- "2018-06-09T09:58:50.505Z"
  , _txnid       :: Text -- "c4a1450c-d08a-45b4-a475-0468bd10e380"
  , _Consent     :: Consent
  , _FIDataRange :: FIDataRange
  , _KeyMaterial :: KeyMaterial
  } deriving (Generic, Show, Eq)

instance FromJSON FIRequest where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON FIRequest where
    toJSON = genericToJSON aaAesonOptions

-- Resp

data FIResponse = FIResponse
  { _ver       :: Text
  , _timestamp :: Text -- "2018-06-09T09:58:50.505Z"
  , _txnid     :: Text -- "c4a1450c-d08a-45b4-a475-0468bd10e380"
  , _consentId :: Text -- "0fee9e18-cf95-11e9-bb65-2a2ae2dbcce4"
  , _sessionId :: Text -- "4600611e-205b-4acc-82f2-52cfb84a8c05"
  } deriving (Generic, Show, Eq)

instance FromJSON FIResponse where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON FIResponse where
    toJSON = genericToJSON aaAesonOptions

-- ### POST Consent ###
-- Req

data ConsentArtefact = ConsentArtefact
  { _ver                           :: Text
  , _timestamp                     :: Text -- "2018-06-09T09:58:50.505Z"
  , _consentId                     :: Text -- "0fee9e18-cf95-11e9-bb65-2a2ae2dbcce4"
  , _status                        :: Text -- "ACTIVE"
  , _createTimestamp               :: Text -- "2018-12-06T11:39:57.153Z"
  , _ConsentDetail                 :: ConsentDetailFIP
  , _consentDetailDigitalSignature :: Text -- "Digital Signature of the ConsentDetail Section."
  , _ConsentUse                    :: ConsentUse
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentArtefact where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentArtefact where
    toJSON = genericToJSON aaAesonOptions

data ConsentDetailFIP = ConsentDetailFIP
  { _consentStart  :: Text -- "2019-12-06T11:39:57.153Z"
  , _consentExpiry :: Text -- "2019-12-06T11:39:57.153Z"
  , _consentMode   :: Text -- "VIEW"
  , _fetchType     :: Text -- "ONETIME"
  , _consentTypes  :: [Text] -- ["BALANCE"]
  , _fiTypes       :: [Text] -- ["DEPOSIT"]
  , _DataConsumer  :: DataConsumerFIP
  , _DataProvider  :: DataProvider
  , _Customer      :: ConsentCustomer
  , _Accounts      :: [ConsentAccount]
  , _Purpose       :: Purpose
  , _FIDataRange   :: FIDataRange
  , _DataLife      :: DataLife
  , _Frequency     :: Frequency
  , _DataFilter    :: [Filter]
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentDetailFIP where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentDetailFIP where
    toJSON = genericToJSON aaAesonOptions

data ConsentAccount = ConsentAccount
  { _fiType          :: Text -- "DEPOSIT"
  , _fipId           :: Text -- "HDFC"
  , _accType         :: Text -- "CURRENT"
  , _linkRefNumber   :: Maybe Text -- "ee371aa0-a354-4adf-abc6-063c1411eab5"
  , _maskedAccNumber :: Text -- "XXXXXXXX2222"
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentAccount where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentAccount where
    toJSON = genericToJSON aaAesonOptions

data ConsentCustomer = ConsentCustomer 
  { _id :: Text -- "cust123"
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentCustomer where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentCustomer where
    toJSON = genericToJSON aaAesonOptions

data DataConsumerFIP = DataConsumerFIP
  { _id   :: Text -- "magizhan@gmail.com",
  , _type :: Text -- "AA"
  } deriving (Generic, Show, Eq)

instance FromJSON DataConsumerFIP where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DataConsumerFIP where
    toJSON = genericToJSON aaAesonOptions

data DataProvider = DataProvider
  { _id   :: Text -- "HDFC",
  , _type :: Text -- "FIP"
  } deriving (Generic, Show, Eq)

instance FromJSON DataProvider where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DataProvider where
    toJSON = genericToJSON aaAesonOptions

data Purpose = Purpose
  { _code     :: Text -- "101"
  , _refUri   :: Text -- "https://api.rebit.org.in/aa/purpose/101.xml"
  , _text     :: Text -- "Wealth management service"
  , _Category :: PurposeCategory
  } deriving (Generic, Show, Eq)

instance FromJSON Purpose where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON Purpose where
    toJSON = genericToJSON aaAesonOptions

data PurposeCategory = PurposeCategory
  { _type :: Text -- "string"
  } deriving (Generic, Show, Eq)

instance FromJSON PurposeCategory where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON PurposeCategory where
    toJSON = genericToJSON aaAesonOptions

data DataLife = DataLife
  { _unit  :: Text -- "DAY"
  , _value :: Int -- 0
  } deriving (Generic, Show, Eq)

instance FromJSON DataLife where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DataLife where
    toJSON = genericToJSON aaAesonOptions

data Frequency = Frequency
  { _unit  :: Text -- "HOUR"
  , _value :: Int -- 0
  } deriving (Generic, Show, Eq)

instance FromJSON Frequency where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON Frequency where
    toJSON = genericToJSON aaAesonOptions

data Filter = Filter
  { _type     :: Text -- "TRANSACTIONAMOUNT"
  , _operator :: Text -- ">="
  , _value    :: Text -- "string"
  } deriving (Generic, Show, Eq)

instance FromJSON Filter where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON Filter where
    toJSON = genericToJSON aaAesonOptions

data ConsentUse = ConsentUse
  { _logUri :: Text -- "string"
  , _count  :: Int -- 1
  , _lastUseDateTime :: Text -- "2018-12-06T11:39:57.153Z"
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentUse where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentUse where
    toJSON = genericToJSON aaAesonOptions

-- Resp
-- None

-- data ConsentResponse = ConsentResponse
--   {}

-- ### GET FI Fetch ###

-- Req

-- Resp

data FIFetchResponse = FIFetchResponse
  { _ver       :: Text -- "1.0"
  , _timestamp :: Text -- "2019-09-09T07:02:07.111+0000"
  , _txnid     :: Text -- "c4a1450c-d08a-45b4-a475-0468bd10e380"
  , _FI        :: [FI]
  } deriving (Generic, Show, Eq)

instance FromJSON FIFetchResponse where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON FIFetchResponse where
    toJSON = genericToJSON aaAesonOptions

data FI = FI
  { _fipID       :: Text -- "BARB0KIMXXX"
  , _data        :: [FIData]
  , _KeyMaterial :: KeyMaterial
  } deriving (Generic, Show, Eq)

instance FromJSON FI where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON FI where
    toJSON = genericToJSON aaAesonOptions

data FIData = FIData
  { _linkRefNumber   :: Text -- "UBI485964579"
  , _maskedAccNumber :: Text -- "XXXXXXXXXX1279"
  , _encryptedFI     :: Text -- "x0AMJWwfzh25l+dV5ip/p3uHLWC8B4f..."
  } deriving (Generic, Show, Eq)

instance FromJSON FIData where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON FIData where
    toJSON = genericToJSON aaAesonOptions

-- ### GET Heartbeat ###
-- Req

-- Resp

data HeartbeatResponse = HeartbeatResponse
  { _ver       :: Text -- "1.0",
  , _timestamp :: Text -- "2019-01-23T04:08:25.132+0000"
  , _Status    :: Text -- "UP"
  } deriving (Generic, Show, Eq)

instance FromJSON HeartbeatResponse where
  parseJSON = genericParseJSON aaAesonOptions

instance ToJSON HeartbeatResponse where
  toJSON = genericToJSON aaAesonOptions
-- ### GET Account Confirm Token ###
--Req
--Resp
-- Invalid token

-- ### POST Consents ###
-- Req

data ConsentsRequest = ConsentsRequest
  { _ver           :: Text
  , _timestamp     :: Text -- "2018-06-09T09:58:50.505Z"
  , _txnid         :: Text -- "3f2b52f0-cf97-11e9-bb65-2a2ae2dbcce4"
  , _ConsentDetail :: ConsentDetailAA
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentsRequest where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentsRequest where
    toJSON = genericToJSON aaAesonOptions
-- 
data ConsentDetailAA = ConsentDetailAA
  { _consentStart  :: Text -- "2019-12-06T11:39:57.153Z"
  , _consentExpiry :: Text -- "2019-12-06T11:39:57.153Z"
  , _consentMode   :: Text -- "VIEW"
  , _fetchType     :: Text -- "ONETIME"
  , _consentTypes  :: [Text] -- ["BALANCE"]
  , _fiTypes       :: [Text] -- ["DEPOSIT"]
  , _DataConsumer  :: DataConsumerAA
  , _Customer      :: ConsentCustomer
  , _Accounts      :: [ConsentAccount]
  , _Purpose       :: Purpose
  , _FIDataRange   :: FIDataRange
  , _DataLife      :: DataLife
  , _Frequency     :: Frequency
  , _DataFilter    :: [Filter]
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentDetailAA where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentDetailAA where
    toJSON = genericToJSON aaAesonOptions

data DataConsumerAA = DataConsumerAA
  { _id :: Text -- "fiu-1"
  } deriving (Generic, Show, Eq)

instance FromJSON DataConsumerAA where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON DataConsumerAA where
    toJSON = genericToJSON aaAesonOptions

-- Resp

data ConsentResponse = ConsentResponse
  { _ver           :: Text -- "1.0"
  , _timestamp     :: Text -- "2019-09-09T07:09:03.095+0000"
  , _txnid         :: Text -- "3f2b52f0-cf97-11e9-bb65-2a2ae2dbcce4"
  , _Customer      :: Customer 
  , _ConsentHandle :: Text -- "d803988d-d810-42fc-bea6-e6a1bac1c025"
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentResponse where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentResponse where
    toJSON = genericToJSON aaAesonOptions

-- ### POST FI fetch for FIU ###
-- Req
-- data FIRequest = FIRequest
--   { _ver :: Text -- "1.0"
--   , _timestamp :: Text -- "2018-06-09T09:58:50.505Z"
--   , _txnid :: Text -- "c4a1450c-d08a-45b4-a475-0468bd10e380"
--   , _Consent :: Consent
--   , _FIDataRange :: FIDataRange
--   , _KeyMaterial :: KeyMaterial
--   }

-- Resp

-- data FIResponse = FIResponse
--   { _ver :: Text
--   , _timestamp :: Text -- "2018-06-09T09:58:50.505Z"
--   , _txnid :: Text -- "c4a1450c-d08a-45b4-a475-0468bd10e380"
--   , _consentId :: Text -- "0fee9e18-cf95-11e9-bb65-2a2ae2dbcce4"
--   , _sessionId :: Text -- "4600611e-205b-4acc-82f2-52cfb84a8c05"
--   }

-- ### GET FI fetch Data for FIU ###
--Req
--Resp

-- data FIFetchResponse = FIFetchResponse
--   { _ver :: Text -- "1.0"
--   , _timestamp :: Text -- "2018-12-06T11:39:57.153+0000"
--   , _txnid :: Text -- "e8cc6822-d4bb-4eb1-9e1b-4996fbff8acb"
--   , _FI :: [FI]
--   }

-- data FI = FI
--   { _fipID :: Text -- "BARB0KIMXXX"
--   , _data :: [FIData]
--   , _KeyMaterial :: KeyMaterial
--   }

-- data FIData = FIData
--   { _linkRefNumber :: Text -- "UBI485964579"
--   , _maskedAccNumber :: Text -- "XXXXXXXXXX1279"
--   , _encryptedFI :: Text -- "x0AMJWwfzh25l+dV5ip/p3uHLWC8B4f..."
--   }

-- ### GET Consent Handle ###
--Req
--Resp
data ConsentHandleResponse = ConsentHandleResponse
  { _ver           :: Text -- "1.0"
  , _timestamp     :: Text -- "2019-05-28T12:58:29.654+0000"
  , _txnid         :: Text -- "4a4adbbe-29ae-11e8-a8d7-0289437bf331"
  , _ConsentHandle :: Text -- "2d216b9b-a8f3-48c0-b565-e8f35e7c2273"
  , _ConsentStatus :: ConsentStatus
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentHandleResponse where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentHandleResponse where
    toJSON = genericToJSON aaAesonOptions

data ConsentStatus = ConsentStatus
  { _id     :: Text -- "ce721611-0ed1-4043-b54a-9493b4ad3007"
  , _status :: Text -- "READY"
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentStatus where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentStatus where
    toJSON = genericToJSON aaAesonOptions

-- ### GET Consent Data Fetch ###

--Req
--Resp

data ConsentDataFetchResponse = ConsentDataFetchResponse
  { _ver                           :: Text
  , _txnid                         :: Text -- "5d9fce7c-ca84-4107-a670-c3b0837c5ef1"
  , _consentId                     :: Text -- "ce721611-0ed1-4043-b54a-9493b4ad3007"
  , _status                        :: Text -- "ACTIVE"
  , _createTimestamp               :: Text -- 2019-05-28T11:38:20.380+0000"
  , _ConsentDetail                 :: ConsentDetailFIP
  , _consentDetailDigitalSignature :: Text -- ""Signature of AA as defined in W3C standards; Base64 encoded"
  , _ConsentUse                    :: ConsentUse
  } deriving (Generic, Show, Eq)

instance FromJSON ConsentDataFetchResponse where
    parseJSON = genericParseJSON aaAesonOptions

instance ToJSON ConsentDataFetchResponse where
    toJSON = genericToJSON aaAesonOptions
