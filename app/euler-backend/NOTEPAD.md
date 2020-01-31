Type correspondence
===================
- NullOrUndefined -> Maybe

Storage types
- Number -> Double
- String -> Text
- Date -> LocalTime

API types
- Foreign -> Text

Storage
=======
++ Chargeback
++ PaymentGatewayResponse (!!! PaymentGatewayResponseV1 use other functions at EC.PaymentGatewayResponseV1)
++ Refund (+ RefundStatus)
++ RiskManagementAccount
++ SecondFactor
++ SecondFactorResponse
++ TxnCardInfo

API
===
- Customer(?)
- Feature (не нужен)
- Mandate (+)
- Promotions (+)
- Risk' (+) для RiskManagementAccount
- MerchantSecondFactorResponse (+) для SecondFactorResponse, достаётся через SecondFactor
- Card (+)
- TxnFlowInfo (+)

Общие вопросы
=============
- Есть ли смысл делать Maybe в Response если в БД поле обязательное (типа потом может перестать им быть?)
- Функции а-ля репозитории для работы с данными (чтобы не повторять runDb, runDb, runDb) -- местами есть, напримери `findMaybePGRById`
- `orderReferenceId` vs `orderId`?
- Посмотреть OrderStatus, большой и сложный

OrderStatusRequest (собирается из order id)
OrderStatusResponse

_Response vs ` vs Resp_

Chargeback
==========
+api Chargeback'
-storage

Chargeback' = Chargeback + TxnDetail' - version + object "chargeback
Версии очищаются в `versionSpecificTransforms`

Customer
========
-api (?)
+storage Customer

- (PS) используется при аутентификации для поиска merchantId если resourceType: "CUSTOMER"

Feature
=======
-api
+storage Feature

- Используется фича *eulerOrderStatusCachingKey = "EULER_ORDER_STATUS_CACHING"* для кеширования и ииспользования кешированного значения.Chargeback
- USE_UDF2_FOR_GATEWAY_REFERENCE_ID

Mandate
=======
-api (?)
+storage Mandate

Мандат создается на каждый заказ (поле authOrderId?) addMandateDetails?

MerchantAccount
===============
-api (неверное, и не должно быть)
+storage MerchantAccount

 Очень неудобный тип, всё опциональное.

MerchantIframePreferences
=========================
+storage MerchantIframePreferencesT

MerchantKey
===========
+storage

OrderMetadataV2
===============
+storage

OrderReference
==============
api -- соотв. OrderStatusResponse примерно
+storage
у нас это Order

PaymentGatewayResponse
======================
-storage

Promotions
==========
-api
+storage Promotions

Refund
======
+api
-storage

Не все версии поддерживают (`versionSpecificTransforms`)

ResellerAccount
===============
+storage

RiskManagementAccount
=====================
-api (Risk)
-storage

SecondFactor
============
-api не нужен
-storage

SecondFactorResponse
====================
-api
-storage

ServiceConfiguration
====================

+storage

TxnCardInfo
===========
api: часть OrderStatusResponse, Card
-storage
