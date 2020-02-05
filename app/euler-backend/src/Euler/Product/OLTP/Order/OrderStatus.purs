



addGatewayResponse ::forall st rt e. Newtype st (TState e) => TxnDetail -> Boolean -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addGatewayResponse txn shouldSendFullGatewayResponse orderStatus = do
  paymentGatewayResp <- sequence $ findMaybePGRById <$> unNullOrUndefined (txn ^. _successResponseId)
  case join paymentGatewayResp of
    Just paymentGatewayResponse -> do
      ordStatus <- getPaymentGatewayResponse txn paymentGatewayResponse orderStatus
      nullVal   <- pure $ nullValue unit
      case (unNullOrUndefined (ordStatus ^._payment_gateway_response')) of
        Just (MerchantPaymentGatewayResponse' pgr') -> do
            gatewayResponse <- getGatewayResponseInJson paymentGatewayResponse shouldSendFullGatewayResponse
            let pgr = MerchantPaymentGatewayResponse {
                           resp_code : just $ checkNull pgr'.resp_code nullVal
                        ,  rrn : just $ checkNull pgr'.rrn nullVal
                        ,  created : just $ checkNull pgr'.created nullVal
                        ,  epg_txn_id : just $ checkNull pgr'.epg_txn_id nullVal
                        ,  resp_message : just $ checkNull pgr'.resp_message nullVal
                        ,  auth_id_code : just $ checkNull pgr'.auth_id_code nullVal
                        ,  txn_id : just $ checkNull pgr'.txn_id nullVal
                        ,  offer : pgr'.offer
                        ,  offer_type : pgr'.offer_type
                        ,  offer_availed : pgr'.offer_availed
                        ,  discount_amount : pgr'.discount_amount
                        ,  offer_failure_reason : pgr'.offer_failure_reason
                        ,  gateway_response : gatewayResponse
                      }
                ordStatus' = ordStatus # _payment_gateway_response' .~ (NullOrUndefined Nothing)
            pure $ ordStatus' # _payment_gateway_response .~ (just pgr)
        Nothing -> pure $ orderStatus
    Nothing -> pure $ orderStatus
  where checkNull resp nullVal      = if (unNull resp "") == "null" then nullVal else toForeign $ (unNull resp "")

getGatewayResponseInJson ::forall st rt e. Newtype st (TState e) =>
                        PaymentGatewayResponse -> Boolean -> BackendFlow st _ (NullOrUndefined Foreign)
getGatewayResponseInJson paymentGatewayResponse shouldSendFullGatewayResponse =
  if shouldSendFullGatewayResponse then do
    jsonPgr <- createJsonFromPGRXmlResponse <$> (O.getResponseXml (paymentGatewayResponse ^.. _responseXml $ ""))
    pure $ just $ jsonPgr
    else pure nothing

casematch :: TxnDetail → PaymentGatewayResponse → MerchantPaymentGatewayResponse' → String → _ → MerchantPaymentGatewayResponse'
casematch txn pgr dfpgresp gw xmls = match gw
  where
        match "CCAVENUE_V2"    = executePGR dfpgresp xmls $ ccavenue_v2  txn pgr xmls
        match "BLAZEPAY"       = executePGR dfpgresp xmls $ blazepay     txn pgr xmls
        match "STRIPE"         = executePGR dfpgresp xmls $ stripe       txn pgr xmls
        match "CITI"           = executePGR dfpgresp xmls $ citi         txn xmls
        match "IPG"            = executePGR dfpgresp xmls $ ipg          txn xmls
        match "FSS_ATM_PIN_V2" = executePGR dfpgresp xmls $ fssatmpin    txn pgr
        match "HDFC_EBS_VAS"   = executePGR dfpgresp xmls $ hdfc_ebs_vas txn pgr
        match "FREECHARGE_V2"  = executePGR dfpgresp xmls $ freechargev2 txn pgr
        match "FSS_ATM_PIN"    = executePGR dfpgresp xmls $ fssatmpin    txn pgr
        match "AIRTELMONEY"    = executePGR dfpgresp xmls $ airtelmoney  txn pgr
        match "CYBERSOURCE"    = executePGR dfpgresp xmls $ cybersource  txn pgr
        match "OLAPOSTPAID"    = executePGR dfpgresp xmls $ olapostpaid  txn pgr
        match "GOCASHFREE"     = executePGR dfpgresp xmls $ gocashfree   txn pgr
        match "EPAYLATER"      = executePGR dfpgresp xmls $ epaylater    txn pgr
        match "ZESTMONEY"      = executePGR dfpgresp xmls $ zestmoney    txn pgr
        match "BILLDESK"       = executePGR dfpgresp xmls $ billdesk     txn pgr
        match "JIOMONEY"       = executePGR dfpgresp xmls $ jiomoney     txn pgr
        match "SBIBUDDY"       = executePGR dfpgresp xmls $ sbibuddy     txn pgr
        match "RAZORPAY"       = executePGR dfpgresp xmls $ razorpay     txn pgr
        match "AXIS_UPI"       = executePGR dfpgresp xmls $ axisupi      txn pgr
        match "PINELABS"       = executePGR dfpgresp xmls $ pinelabs     txn pgr
        match "MOBIKWIK"       = executePGR dfpgresp xmls $ mobikwik     txn pgr
        match "LINEPAY"        = executePGR dfpgresp xmls $ linepay      txn pgr
        match "PHONEPE"        = executePGR dfpgresp xmls $ phonepe      txn pgr
        match "ICICINB"        = executePGR dfpgresp xmls $ icicinb      txn pgr
        match "ZAAKPAY"        = executePGR dfpgresp xmls $ zaakpay      txn pgr
        match "AIRPAY"         = executePGR dfpgresp xmls $ airpay       txn pgr
        match "AXISNB"         = executePGR dfpgresp xmls $ axisnb       txn pgr
        match "SODEXO"         = executePGR dfpgresp xmls $ sodexo       txn pgr
        match "CITRUS"         = executePGR dfpgresp xmls $ citrus       txn pgr
        match "PAYPAL"         = executePGR dfpgresp xmls $ paypal       txn pgr
        match "HDFCNB"         = executePGR dfpgresp xmls $ hdfcnb       txn pgr
        match "KOTAK"          = executePGR dfpgresp xmls $ kotak        txn pgr
        match "MPESA"          = executePGR dfpgresp xmls $ mpesa        txn pgr
        match "SIMPL"          = executePGR dfpgresp xmls $ simpl        txn pgr
        match "CASH"           = executePGR dfpgresp xmls $ cash         txn pgr
        match "TPSL"           = executePGR dfpgresp xmls $ tpsl         txn pgr
        match "LAZYPAY"        = executePGR dfpgresp xmls $ lazypay      txn pgr
        match "FSSPAY"         = executePGR dfpgresp xmls $ fsspay       txn pgr
        match "AMEX"           = executePGR dfpgresp xmls $ amex         txn pgr
        match "ATOM"           = executePGR dfpgresp xmls $ atom         txn pgr
        match "PAYTM_V2"       = executePGR dfpgresp xmls $ paytm_v2     pgr
        match "EBS_V3"         = executePGR dfpgresp xmls $ ebs_v3       pgr
        match "AXIS"           = executePGR dfpgresp xmls $ axis         pgr
        match "HDFC"           = executePGR dfpgresp xmls $ hdfc         pgr
        match "EBS"            = executePGR dfpgresp xmls $ ebs          pgr
        match "MIGS"           = executePGR dfpgresp xmls $ migs         pgr
        match "ICICI"          = executePGR dfpgresp xmls $ icici        pgr
        match "PAYLATER"       = executePGR dfpgresp xmls $ paylater     txn
        match "DUMMY"          = executePGR dfpgresp xmls $ dummy
        match "FREECHARGE"     = executePGR dfpgresp xmls (freecharge txn pgr)
                                  # \upgr
                                    → if campaignCode /= "NA"
                                        then upgr
                                          # _offer           .~ (just campaignCode)
                                          # _offer_type      .~ justNa
                                          # _offer_availed   .~ (just $ toForeign "NA")
                                          # _discount_amount .~ (just $ nullValue unit)
                                        else upgr
                                 -- Freecharge doesn't return any information about offers in their response
                                 where
                                    campaignCode = lookupXML xmls "campaignCode" "NA"
        match "PAYU"           = executePGR dfpgresp xmls (payu  pgr)
                                  # \upgr → if offer /= "NA"
                                  -- * Payu allows merchants to send multiple offers and avails a valid offer among them
                                  -- * offer_availed contains the successfully availed offer.
                                        then upgr
                                                    # _offer           .~ (just offerVal)
                                                    # _offer_type      .~ offerType
                                                    # _offer_availed   .~ (just $ toForeign offerAvailed)
                                                    # _discount_amount .~ (just disAmount)
                                                    # \rec → if offerFailure /= "null"
                                                              then rec # _offer_failure_reason .~ (just offerFailure)
                                                              else rec
                                        else upgr
                                 where
                                     offer        = lookupXML     xmls "offer"                 "NA"
                                     offerVal     = lookupXMLKeys xmls "offer_availed" "offer" "null"
                                     offerType    = lookupXML     xmls "offer_type"            "null" # just
                                     offerFailure = lookupXML     xmls "offer_failure_reason"  "null"
                                     discount     = lookupXML     xmls "discount"              "NA"
                                     offerAvailed = offerVal /= "null"
                                     disAmount    = Number.fromString discount # maybe (nullValue unit) toForeign

        match "PAYTM"           = executePGR dfpgresp xmls (paytm pgr)
                                    # \upgr → if promoCampId /= "null"
                                              then upgr
                                                    # _offer           .~ (just promoCampId)
                                                    # _offer_type      .~ justNa
                                                    # _offer_availed   .~ (promoStatus == "PROMO_SUCCESS" # toForeign >>> just)
                                                    # _discount_amount .~ (just $ nullValue unit)
                                              else upgr
                                  where
                                    promoCampId = lookupXML xmls "PROMO_CAMP_ID" "null"
                                    promoStatus = lookupXML xmls "PROMO_STATUS"  "null"

        match "OLAMONEY"        = executePGR dfpgresp xmls (olamoney txn pgr)
                                    # \upgr → if couponCode /= "null"
                                              then upgr
                                                    # _offer           .~ (just couponCode)
                                                    # _offer_type      .~ justNa
                                                    # _offer_availed   .~ (strToBool isCashbackSuc # toForeign >>> just)
                                                    # _discount_amount .~ (just $ nullValue unit)
                                              else upgr
                                  where
                                    couponCode    = lookupXML xmls "couponCode"           "null"
                                    isCashbackSuc = lookupXML xmls "isCashbackSuccessful" "null"

        match "AMAZONPAY"       = executePGR dfpgresp xmls (amazonpay txn pgr)
                                    # \upgr → if sellerNote /= "null"
                                              then upgr
                                                    # _offer           .~ (just sellerNote)
                                                    # _offer_type      .~ justNa
                                                    # _offer_availed   .~ (nullValue unit # just)
                                                    # _discount_amount .~ (just $ nullValue unit)
                                              else upgr

                                  where
                                    sellerNote = lookupXML xmls "sellerNote" "null"

        match _             = find (\val → gw == show val) (eulerUpiGateways <> [GOOGLEPAY])
                                # maybe
                                    (executePGR dfpgresp xmls $ otherGateways pgr)
                                    (const (executePGR dfpgresp xmls $ eulerUpiGWs txn pgr))

getPaymentGatewayResponse
  :: ∀ st r.
      Newtype st
        { orderId           :: Maybe String
        , merchantId        :: Maybe String
        , isDBMeshEnabled   :: Maybe Boolean
        , isMemCacheEnabled :: Boolean
        | r }
      => TxnDetail
      → PaymentGatewayResponse
      → OrderStatusResponse
      → BackendFlow st _ OrderStatusResponse
getPaymentGatewayResponse txn pgr orderStatusResp = do
  getResponseXMLTuple pgrXml
    <#>
      (casematch txn pgr upgr gateway >>> just >>> \v → orderStatusResp # _payment_gateway_response' .~ v)

  where
        gateway = unNull (txn ^. _gateway) ""
        pgrXml  = unNull (pgr ^. _responseXml) ""
        date    = pgr ^. _dateCreated # unNullOrUndefined >>> maybe nothing (_dateToString >>> just)
        upgr    = defaultPaymentGatewayResponse # _created .~ date

versionSpecificTransforms ::forall st rt e. Newtype st (TState e) => RouteParameters -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
versionSpecificTransforms headers orderStatus = do
  let pgResponse     = unNullOrUndefined (orderStatus ^. _payment_gateway_response)
      refunds        = unNull (orderStatus ^. _refunds) []
      gatewayId      = unNull (orderStatus ^. _gateway_id) 0
  version     <- pure $ StrMap.lookup "version" headers
  apiVersion  <- pure $ fromMaybe "" version
  refunds'    <- if (apiVersion < "2015-08-18" && apiVersion /= "") || apiVersion == "" then
                    filterA (\refund -> pure ((refund ^._status) /= (FAILURE))) refunds
                  else pure $ refunds
  refund      <- traverse (getRefundStatus apiVersion) refunds'
    -- Removing all the offer related params from the PG response
  pgResps <- case pgResponse of
                Just pgResp -> do
                    --let discountAmount = unNull (pgResp ^. _discount_amount) ""
                    pgResponse' <- if apiVersion < "2017-05-25" || apiVersion == "" then do
                                      let pgResp1 = pgResp  # _offer .~ NullOrUndefined Nothing
                                          pgResp2 = pgResp1 # _offer_availed .~ NullOrUndefined Nothing
                                          pgResp3 = pgResp2 # _offer_type .~ NullOrUndefined Nothing
                                          pgResp4 = pgResp3 # _offer_failure_reason .~ NullOrUndefined Nothing
                                          pgResp5 = pgResp4 # _discount_amount .~ NullOrUndefined Nothing
                                      pure $ pgResp5
                                    else pure $ pgResp

                    -- pgResponse' <- if apiVersion >= "2017-12-08" && apiVersion /= "" && discountAmount == "NA"
                    --                 then pure $ pgResp # _discount_amount .~ NullOrUndefined (Nothing)
                    --                 else pure $ pgResp

                    pgResp <- if (gatewayId == gatewayIdFromGateway PAYU && ((apiVersion < "2017-10-26" && apiVersion /= "") || apiVersion == "")) then do
                                  let pgResp = pgResponse' # _auth_id_code .~ pgResponse' ^. _rrn
                                  pure $ pgResp # _rrn .~ pgResp ^. _epg_txn_id
                                else pure $ pgResponse'
                    pure $ Just pgResp
                Nothing -> pure $ Nothing
  let orderStatus' = if length refund > 0 then orderStatus # _refunds .~ NullOrUndefined ( Just refund) else orderStatus
      ordStatus = orderStatus' # _payment_gateway_response .~ NullOrUndefined pgResps
  ordStatusResp <- if (isNotNull (ordStatus ^. _chargebacks)) && ((apiVersion < "2017-07-26" && apiVersion /= "") || apiVersion == "") then pure $ ordStatus # _chargebacks .~ NullOrUndefined Nothing
                      else if (apiVersion == "") then pure $ ordStatus # _chargebacks .~ NullOrUndefined Nothing
                      else pure $ ordStatus
  ordResp      <- if (isNotNull (ordStatusResp ^. _txn_detail)) && ((apiVersion < "2018-07-16" && apiVersion /= "") || apiVersion == "") then pure $ ordStatusResp # _txn_detail .~ NullOrUndefined Nothing
                      else if (apiVersion == "") then pure $ ordStatusResp # _txn_detail .~ NullOrUndefined Nothing
                      else pure $ ordStatusResp
  if (isNotNull (ordResp ^. _gateway_reference_id)) && ((apiVersion < "2018-10-25" && apiVersion /= "") || apiVersion == "") then pure $ ordResp # _gateway_reference_id .~ NullOrUndefined Nothing
    else if (apiVersion == "") then pure $ ordResp # _gateway_reference_id .~ NullOrUndefined Nothing
    else pure $ ordResp

getRefundStatus ::forall st rt e. Newtype st (TState e) => String  -> Refund' -> BackendFlow st _ Refund'
getRefundStatus apiVersion refund = do
   let status = refund ^. _status
   refunds <- if (apiVersion < "2018-09-20" && apiVersion /= "") || (apiVersion == "") then pure (refund # _initiated_by .~ nothing) else pure refund
   refunds <- if (apiVersion < "2019-03-12" && apiVersion /= "") || (apiVersion == "") then do
                                                                                            let refund = refunds # _refund_source .~ nothing # _refund_type .~ nothing
                                                                                            pure refund
                                                                                       else pure refunds
   if apiVersion >= "2015-01-09" && apiVersion /= "" then do
     if apiVersion < "2017-03-16" && apiVersion /= "" && status == MANUAL_REVIEW then pure $ refunds # _status .~ PENDING
      else pure $ refunds
    else pure $ refunds # _status .~ SUCCESS


-- TODO: Move to method decode once presto is fixed
postOrderStatus :: BackendFlow SyncStatusState Configs Foreign
postOrderStatus = proxyOrderStatus POST

postOrderCreate :: BackendFlow SyncStatusState Configs Foreign
postOrderCreate = proxyOrderCreate POST

getOrderStatus :: BackendFlow SyncStatusState Configs Foreign
getOrderStatus = proxyOrderStatus GET

proxyOrderStatus :: Method -> BackendFlow SyncStatusState Configs Foreign
proxyOrderStatus method = do
  config <- ask
  let url = S.replace (S.Pattern "proxy/") (S.Replacement "") config.url
      finalSystemName = if url == "txns" then "euler_txns" else expectationsSystem
      request = Request {
        method
      , url : getBaseUrl <> url
      , payload : config.rawBody
      , headers : modifyContentType config.rawHeaders
      }
  stringResponse <- doAffRR' "proxyOrderStatus. api mkNativeRequest" (api (mkNativeRequest request))
  let resp = fromMaybe (toForeign "") (parseJson stringResponse)
      responseHeaders = getValueFromRecJ resp ["headers"]
      requestHeaders = getValueFromRecJ resp ["requestHeaders"]
      responseStatus = getValueFromRecJ resp ["status"]
      responseStatusText = getValueFromRecJ resp ["statusText"]
      responseData = getValueFromRecJ resp ["data"]
      latency = getValueFromRecJ resp ["latency"]
      expectationsRequest = {
        method: POST
      , url: getExpectationsUrl <> "store/request-response"
      , payload: defaultEncodeJSON $ EXP.ExpectationsRequest {
          payload: EXP.ExpectationsPayload {
              request: EXP.ExpectationsReqResp {
                  header: jsonStringify requestHeaders
                , body: config.rawBody
                , url: getBaseUrl <> url
                , method: maybe "GET" (S.toUpper <<< show) config.method
                , status: 0
                , statusText: ""
              }
            , response: EXP.ExpectationsReqResp {
                  header: jsonStringify responseHeaders
                , body: jsonStringify responseData
                , url: ""
                , method: ""
                , status: responseStatus
                , statusText: responseStatusText
              }
            , latency
          }
          , systemName: finalSystemName
      }
      , headers: Headers []
      }
  -- TODO: WTF??
  void $ doAffRR' "proxyOrderStatus. unsafe forked mkNativeRequest" (do
    void $ unsafeCoerceAff $ forkAff $ api $ mkNativeRequest $ Request expectationsRequest
    pure UnitEx
    )
  pure $ toForeign {
      responseData
    , responseStatus
    }
  where
    modifyContentType :: StrMap String -> Headers
    modifyContentType rawHeaders =
      case lookup "Content-Type" rawHeaders <|> lookup "content-type" rawHeaders of
        Just val -> case val of
          "application/json" -> mapToHeaders rawHeaders
          _ ->  mapToHeaders
            <<< insert "Content-Type" "application/x-www-form-urlencoded"
            <<< delete "Content-Type"
            <<< delete "content-type"
            $ rawHeaders
        Nothing -> mapToHeaders rawHeaders


proxyOrderCreate :: Method -> BackendFlow SyncStatusState Configs Foreign
proxyOrderCreate method = do
  config <- ask
  let url = S.replace (S.Pattern "proxy/") (S.Replacement "") config.url
      finalSystemName = if url == "orders" then "euler_order_create" else expectationsSystem
      request = Request {
        method
      , url : getBaseUrl <> url
      , payload : config.rawBody
      , headers : modifyContentType config.rawHeaders
      }
  stringResponse <- doAffRR' "proxyOrderCreate. api mkNativeRequest"  do api (mkNativeRequest request)
  let resp = fromMaybe (toForeign "") (parseJson stringResponse)
      responseHeaders = getValueFromRecJ resp ["headers"]
      requestHeaders = getValueFromRecJ resp ["requestHeaders"]
      responseStatus = getValueFromRecJ resp ["status"]
      responseStatusText = getValueFromRecJ resp ["statusText"]
      responseData = getValueFromRecJ resp ["data"]
      latency = getValueFromRecJ resp ["latency"]
      expectationsRequest = {
        method: POST
      , url: getExpectationsUrl <> "store/request-response"
      , payload: defaultEncodeJSON $ EXP.ExpectationsRequest {
          payload: EXP.ExpectationsPayload {
              request: EXP.ExpectationsReqResp {
                  header: jsonStringify requestHeaders
                , body: config.rawBody
                , url: getBaseUrl <> url
                , method: maybe "GET" (S.toUpper <<< show) config.method
                , status: 0
                , statusText: ""
              }
            , response: EXP.ExpectationsReqResp {
                  header: jsonStringify responseHeaders
                , body: jsonStringify responseData
                , url: ""
                , method: ""
                , status: responseStatus
                , statusText: responseStatusText
              }
            , latency
          }
          , systemName: finalSystemName
      }
      , headers: Headers []
      }
  void $ doAffRR' "proxyOrderCreate. unsafe forked mkNativeRequest" (do
    void $ unsafeCoerceAff $ forkAff $ api $ mkNativeRequest $ Request expectationsRequest
    pure UnitEx
    )
  pure $ toForeign {
      responseData
    , responseStatus
    }
  where
    modifyContentType :: StrMap String -> Headers
    modifyContentType rawHeaders =
      case lookup "Content-Type" rawHeaders <|> lookup "content-type" rawHeaders of
        Just val -> case val of
          "application/json" -> mapToHeaders rawHeaders
          _ ->  mapToHeaders
            <<< insert "Content-Type" "application/x-www-form-urlencoded"
            <<< delete "Content-Type"
            <<< delete "content-type"
            $ rawHeaders
        Nothing -> mapToHeaders rawHeaders

getTxnStatusResponse ::forall st rt e. Newtype st (TState e) => TxnDetail -> MerchantAccount -> SecondFactor -> BackendFlow st _ TxnStatusResponse
getTxnStatusResponse txnDetail@(TxnDetail txn) merchantAccount sf = do
    ordStatusResponse <- addPaymentMethodInfo merchantAccount txnDetail def
                          >>= addRefundDetails txnDetail
                          >>= addGatewayResponse txnDetail false
    pure $ TxnStatusResponse {
        id : txnDetail .^. _txnUuid
      , order_id : txnDetail ^. _orderId
      , txn_id : txnDetail ^. _txnId
      , status : txnDetail ^. _status
      , gateway : txnDetail .^. _gateway
      , created : txnDetail .^. _dateCreated
      , resp_code : txnDetail ^.. _bankErrorCode  $ ""
      , resp_message : txnDetail ^.. _bankErrorMessage $ ""
      , payment_info : getPaymentInfo ordStatusResponse
      , payment_gateway_response : ordStatusResponse ^. _payment_gateway_response
      , refunds : ordStatusResponse ^. _refunds
      , payment : nothing
    }

getPaymentInfo :: OrderStatusResponse -> PaymentInfo
getPaymentInfo ordStatusResponse = PaymentInfo {
    payment_method_type : ordStatusResponse ^. _payment_method_type
  , payment_method      : ordStatusResponse ^. _payment_method
  , card                : ordStatusResponse ^. _card
  , auth_type           : nothing
  , authentication      : nothing
}

--- TODO: Move this to a common file
getTokenExpiryData :: BackendFlow SyncStatusState _ OrderTokenExpiryData
getTokenExpiryData = do
  orderToken <- ((append "tkn_") <$> getUUID32)
  currentDateWithOffset <- getCurrentDateStringWithOffset Config.orderTokenExpiry
  defaultTokenData <- pure $ OrderTokenExpiryData {expiryInSeconds : Config.orderTokenExpiry
                      , tokenMaxUsage : Config.orderTokenMaxUsage
                      , orderToken : NullOrUndefined $ Just orderToken
                      , currentDateWithExpiry : NullOrUndefined $ Just currentDateWithOffset}
  tokenExpiryData :: (Maybe ServiceConfiguration) <- DB.findOne ecDB (where_ := WHERE ["name" /\ String "ORDER_TOKEN_EXPIRY_DATA"])
  case tokenExpiryData of
    (Just toknExpData) -> do
      OrderTokenExpiryData decodedVal <- decodeString (_.value <<< unwrap $ toknExpData)
      pure $ OrderTokenExpiryData (unwrap defaultTokenData) { expiryInSeconds = decodedVal.expiryInSeconds
                                                            , tokenMaxUsage = decodedVal.tokenMaxUsage}
    Nothing -> pure $ defaultTokenData

getAxisUpiTxnIdFromPgr ::forall st rt e. Newtype st (TState e) =>
      TxnDetail -> (Array _)  -> BackendFlow st _ String
getAxisUpiTxnIdFromPgr txn pgResponse = do
  merchantRequestId <- lookupRespXml' pgResponse "merchantRequestId" ""
  if merchantRequestId == "" then do
    transactionRef <- lookupRespXml' pgResponse "transactionRef" ""
    if transactionRef /= "" then do
      pure transactionRef else pure (txn ^. _txnId)
    else pure merchantRequestId

getAxisUpiRequestIdFromPgr ::forall st rt e. Newtype st (TState e) =>
      TxnDetail -> (Array _) -> BackendFlow st _ String
getAxisUpiRequestIdFromPgr txn pgResponse = do
  gatewayTransactionId <- lookupRespXml' pgResponse "gatewayTransactionId" ""
  if gatewayTransactionId == "" then lookupRespXml' pgResponse "upiRequestId" "null"
    else pure gatewayTransactionId

createJsonFromPGRXmlResponse :: Array String -> Foreign
createJsonFromPGRXmlResponse arrayXml = toForeign $ foldl xmlToJsonPgrAccumulater empty arrayXml

xmlToJsonPgrAccumulater ::forall b.(StrMap String) -> String -> StrMap String
xmlToJsonPgrAccumulater strmap xml = do
  case runExcept $ decode $ toForeign $ xml of
    Right (JsonToXmlType val) -> do
      let array = val.string
      insert (fromMaybe "" (array !! 0))  (fromMaybe "" (array !! 1)) strmap
    Left err  -> do
      case runExcept $ decode $ toForeign $ xml of
        Right (JsonToXmlTypeForeign foreignVal) -> do
          -- To handle {} cases.
          let arrayForeign = foreignVal.string
          insert (fromMaybe "" (readStringMaybe $ (fromMaybe (toForeign $ "") (arrayForeign !! 0))))  "" strmap
        Left err -> strmap

-- In direct UPI flow, in case of AXIS_UPI, we've added one more lookup for customerVpa as gateway is sending payer vpa in customerVpa field. For other upi gateways also we've to add loopup based on their response.
--TODO-- Once all the upi gateways are migrated to euler from euler-upi, Remove payerVpa lookup and keep gateway specific loopup key for payer vpa param.

addPayerVpaToResponse :: forall st rt e. Newtype st (TState e)
  =>  TxnDetail
  -> OrderStatusResponse
  -> NullOrUndefined Foreign
  -> BackendFlow st _ OrderStatusResponse
addPayerVpaToResponse txnDetail ordStatusResp paymentSource = do
  let ord' = (ordStatusResp # _payer_app_name .~ paymentSource)
  pgr <- sequence $ findMaybePGRById <$> (unNullOrUndefined $ txnDetail ^. _successResponseId)
  case join pgr of
    Just (PaymentGatewayResponse pg) -> do
      pgResponse  <- O.getResponseXml (unNull pg.responseXml "")
      payervpa <- case txnDetail ^.. _gateway $ "" of
                    "AXIS_UPI"    -> lookupRespXml' pgResponse "payerVpa" =<< lookupRespXml' pgResponse "customerVpa" ""
                    "HDFC_UPI"    -> lookupRespXml' pgResponse "payerVpa" ""
                    "INDUS_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
                    "KOTAK_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
                    "SBI_UPI"     -> lookupRespXml' pgResponse "payerVpa" ""
                    "ICICI_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
                    "HSBC_UPI"    -> lookupRespXml' pgResponse "payerVpa" ""
                    "VIJAYA_UPI"  -> lookupRespXml' pgResponse "payerVpa" ""
                    "YESBANK_UPI" -> lookupRespXml' pgResponse "payerVpa" ""
                    "PAYTM_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
                    "PAYU"        -> lookupRespXml' pgResponse "field3" ""
                    "RAZORPAY"    -> lookupRespXml' pgResponse "vpa" ""
                    "PAYTM_V2"    -> lookupRespXml' pgResponse "VPA" ""
                    "GOCASHFREE"  -> lookupRespXml' pgResponse "payersVPA" ""
                    _             -> pure ""
      if (payervpa == "") then pure ord' else pure (ord' # _payer_vpa .~ just (toForeign payervpa))
    Nothing -> pure ord'
