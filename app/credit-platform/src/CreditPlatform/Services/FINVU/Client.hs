{-# OPTIONS_GHC -fno-warn-orphans #-}

module CreditPlatform.Services.FINVU.Client where

import EulerHS.Prelude
import CreditPlatform.Services.FINVU.API

import Servant
import Servant.XML
import Xmlbf

import qualified Data.Text.Lazy as TL

instance FromXml TL.Text where
  fromXml = pText

-- http://api.finvu.in/ConnectHub/V1/Heartbeat
type HeartbeatAPI = "ConnectHub" :> "V1" :> "Heartbeat"
  :> Header "aa_api_key" Text
  :> Get '[JSON] HeartbeatResponse

heartbeatAPI :: Proxy HeartbeatAPI
heartbeatAPI = Proxy

-- http://api.finvu.in/ConnectHub/V1/FI/fetch/352f1a7c-326d-4256-aa94-8c8c8094be1c
type FIFetchAPI = "ConnectHub" :> "V1" :> "FI" :> "fetch"
  :> Header "aa_api_key" Text
  :> Capture "FIid" Text
  :> Get '[JSON] FIFetchResponse

fiFetchAPI :: Proxy FIFetchAPI
fiFetchAPI = Proxy

-- http://aa.finvu.in/API/V1/Consent/handle/2d216b9b-a8f3-48c0-b565-e8f35e7c2273
type ConsentHandleAPI = "API" :> "V1" :> "Consent" :> "handle"
  :> Header "fip_api_key" Text
  :> Capture "consentHandleId" Text
  :> Get '[JSON] ConsentHandleResponse

consentHandleAPI :: Proxy ConsentHandleAPI
consentHandleAPI = Proxy

--http://aa.finvu.in/API/V1/Consent/ce721611-0ed1-4043-b54a-9493b4ad3007
type ConsentArtefactFetchAPI = "API" :> "V1" :> "Consent"
  :> Header "fip_api_key" Text
  :> Capture "consentArtefactId" Text
  :> Get '[JSON] ConsentArtefact

consentArtefactFetchAPI :: Proxy ConsentArtefactFetchAPI
consentArtefactFetchAPI = Proxy

-- http://api.finvu.in/ConnectHub/V1/Accounts/discover
type DiscoverAccountAPI = "ConnectHub" :> "V1" :> "Accounts" :> "discover"
  :> Header "aa_api_key" Text
  :> ReqBody '[JSON] DiscoverAccountRequest
  :> Post '[JSON] DiscoverAccountResponse

discoverAccountAPI :: Proxy DiscoverAccountAPI
discoverAccountAPI = Proxy

-- http://api.finvu.in/Accounts/add

type AddAccountAPI = "Accounts" :> "add"
  :> Header "fip_api_key" Text
  :> ReqBody '[XML] UserAccountInfo
  :> Post '[XML] TL.Text

addAccountAPI :: Proxy AddAccountAPI
addAccountAPI = Proxy

-- http://api.finvu.in/Accounts/Transaction/add
type AddTransactionAPI = "Accounts" :> "Transaction" :> "add"
  :> Header "fip_api_key" Text
  :> ReqBody '[XML] UserAccountTrans
  :> Post '[XML] TL.Text

addTransactionAPI :: Proxy AddTransactionAPI
addTransactionAPI = Proxy

-- http://api.finvu.in/ConnectHub/V1/Accounts/link
type AccountLinkingAPI = "ConnectHub" :> "V1" :> "Accounts" :> "link"
  :> Header "aa_api_key" Text
  :> ReqBody '[JSON] AccLinkingRequest
  :> Post '[JSON] AccLinkingResponse

accountLinkingAPI :: Proxy AccountLinkingAPI
accountLinkingAPI = Proxy

-- http://api.finvu.in/ConnectHub/V1/FI/request
type FIRequestAPI = "ConnectHub" :> "V1" :> "FI" :> "request"
  :> Header "aa_api_key" Text
  :> ReqBody '[JSON] FIRequest
  :> Post '[JSON] FIResponse

fiRequestAPI :: Proxy FIRequestAPI
fiRequestAPI = Proxy

-- http://api.finvu.in/ConnectHub/V1/Consent
type ConsentArtefactAPI = "ConnectHub" :> "V1" :> "Consent"
  :> Header "aa_api_key" Text
  :> ReqBody '[JSON] ConsentArtefact
  :> Post '[JSON] NoContent

consentArtefactAPI :: Proxy ConsentArtefactAPI
consentArtefactAPI = Proxy


-- http://api.finvu.in/ConnectHub/V1/Accounts/link/ee371aa0-a354-4adf-abc6-063c1411eab5/872150
type AccLinkWithTokenAPI = "ConnectHub" :> "V1" :> "Accounts" :> "link"
  :> Header "aa_api_key" Text
  :> Capture "refNumber" Text
  :> Capture "token" Text
  :> Get '[JSON] LinkDelinkTokenResponse

accLinkWithTokenAPI :: Proxy AccLinkWithTokenAPI
accLinkWithTokenAPI = Proxy

-- http://aa.finvu.in/API/V1/Consent
type ConsentRequestAPI = "API" :> "V1" :> "Consent"
  :> Header "fip_api_key" Text
  :> ReqBody '[JSON] ConsentRequest
  :> Post '[JSON] ConsentResponse

consentRequestAPI :: Proxy ConsentRequestAPI
consentRequestAPI = Proxy

-- http://aa.finvu.in/API/V1/FI/request
type FIUFIRequestAPI = "API" :> "V1" :> "FI" :> "request"
  :> Header "fip_api_key" Text
  :> ReqBody '[JSON] FIRequest
  :> Post '[JSON] FIResponse

fiuFIRequestAPI :: Proxy FIUFIRequestAPI
fiuFIRequestAPI = Proxy

-- http://aa.finvu.in/API/V1/FI/fetch/fd1dc34c-da66-4c5d-8101-260c68286836
type FIUFIFetchAPI = "API" :> "V1" :> "FI" :> "fetch"
  :> Header "fip_api_key" Text
  :> Capture "sessionId" Text
  :> Get '[JSON] FIFetchResponse

fiuFIFetchAPI :: Proxy FIUFIFetchAPI
fiuFIFetchAPI = Proxy
