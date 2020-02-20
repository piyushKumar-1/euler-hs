
Order Create API
----------------

## Recordings from euler-ps (sessionId: "NOREQID-d24421fe521b4b46a9a1b411267550d6")

- SQL findOne merchant                             - found
- SQL findOne from MerchantAccount                - found
- KV getCache                                      - not found
- ???? redis fetch error
- SQL: findAll where merchant_account_id            - found []
- KV getCache                                       - not found
- KV redis_lookup                                   - ?
- ??? redis fetch error
- KV keyExistInCache                                - false
- KV getCache                                       - not found
- ??? redis fetch error
- isDBMeshEnabled                               - false
- SQL findOne where order_id, merchant_id      - not found
- SQL findOne where merchant_id from MerchantIframePreferences                - found
- SQL findOne where merchant_account_id from Customer    - not found
- SQL findOne where merchant_account_id from Customer    - not found
- SQL createWithOpts in OrderReference
- SQL createWithOpts in OrderMetadataV2
- KV setCacheWithExpiry
- SQL findOne where name=ORDER_TOKEN_EXPIRY_DATA_MERCHANT_WISE                 - not found
- KV setCacheWithExpiry
- logInfo "order_token_cache" tokenMaxUsage
- logInfo "createOrUpdateOrder response"

## Recordings from euler-hs (2020-02-20_11-18-33_orderCreate_sessionId)

- SQL where merchantAccountId       - found
- SQL from MerchantAccount          - found
- KV get key "euler_ip_whitelist_for_merchantId"    - found
- log "ipAddressFilters"
- KV get key merchantId_mandate_max_amount_limit    - not found
- SQL                               - not found
- SQL from MerchantIframePreferences
- SQL from Customer
- SQL OrderAddress
- SQL OrderAddress
- GenerateGUIDEntry
- RunIOEntry get time
- SQL OrderReference
- RunIOEntry get time
- SQL OrderMetadataV2
- KV SetExEntry key merchantId_orderid_1580125159093 Order
- LogMessageEntry "Redis setCacheWithExpiry"
- RunIOEntry Config.getECRConfig
- GenerateGUIDEntry (in tokenizeResource)
- SQL  from ServiceConfiguration                - no result
- RunIOEntry getCurrentDateInMillis
- KV SetExEntry key "tkn_f845c4cc72ff4a6797cd479a8310862e"
- LogMessageEntry "Redis setCacheWithExpiry"
- LogMessageEntry "ORDER_token_cache"
- RunIOEntry getCurrentDateStringWithSecOffset
- RunIOEntry incrementClientAuthTokenGeneratedCount
-
