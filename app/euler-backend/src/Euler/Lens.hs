module Euler.Lens where

import EulerHS.Prelude (Lens)
import Data.Generics.Product.Fields

-- EHS: not lens for "Euler", but rather for domain / api types.
-- Should not be on the top level?
--

_merchantAccountId :: HasField' "merchantAccountId" s a => Lens s s a a --  t a b
_merchantAccountId = field' @"merchantAccountId"

_merchantId :: HasField' "merchantId" s a => Lens s s a a --  t a b
_merchantId = field' @"merchantId"

_id :: HasField' "id" s a => Lens s s a a --  t a b
_id = field' @"id"

_apiKey :: HasField' "apiKey" s a  => Lens s s a a -- t a b
_apiKey = field' @"apiKey"

_resellerId :: HasField' "resellerId" s a  => Lens s s a a
_resellerId = field' @"resellerId"

_resellerApiEndpoint :: HasField' "resellerApiEndpoint" s a  => Lens s s a a
_resellerApiEndpoint = field' @"resellerApiEndpoint"

_mandate :: HasField' "mandate" s a => Lens s s a a
_mandate = field' @"mandate"

_order_id :: HasField' "order_id" s a => Lens s s a a
_order_id = field' @"order_id"

_orderId :: HasField' "orderId" s a => Lens s s a a
_orderId = field' @"orderId"

_ipAddress :: HasField' "ipAddress" s a => Lens s s a a
_ipAddress = field' @"ipAddress"

_options_create_mandate :: HasField' "options_create_mandate" s a => Lens s s a a
_options_create_mandate = field' @"options_create_mandate"

_customer_id :: HasField' "customer_id" s a => Lens s s a a
_customer_id = field' @"customer_id"

_mandate_max_amount :: HasField' "mandate_max_amount" s a => Lens s s a a
_mandate_max_amount = field' @"mandate_max_amount"

_protocol :: HasField' "protocol" s a => Lens s s a a
_protocol = field' @"protocol"

_host :: HasField' "host" s a => Lens s s a a
_host = field' @"host"

_value :: HasField' "value" s a => Lens s s a a --  t a b
_value = field' @"value"
