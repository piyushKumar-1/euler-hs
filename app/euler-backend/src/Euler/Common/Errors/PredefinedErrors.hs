module Euler.Common.Errors.PredefinedErrors where

import EulerHS.Prelude

import Euler.Common.Errors.Types

import qualified Data.Aeson.Encode.Pretty as A   (encodePretty)
-- Predefined Errors from src/Engineering/Flow.purs


merchantIdError :: ErrorResponse
merchantIdError = ErrorResponse
  { code = 500
  , response = ErrorPayload
      { error_message = "Server error."
      , userMessage = "Invalid merchant id. Cannot process your request."
      , error = True
      }
  }

resellerIdError :: ErrorResponse
resellerIdError = ErrorResponse
  { code = 500
  , response = ErrorPayload
      { error_message = "Server error."
      , userMessage = "Invalid reseller id. Cannot process your request."
      , error = True
      }
  }

loginContextError :: ErrorResponse
loginContextError = ErrorResponse
  { code = 403
  , response = ErrorPayload
      { error_message = "Forbidden request."
      , userMessage = "Unauthorized user role."
      , error = True
      }
  }

twoFAError :: ErrorResponse
twoFAError = ErrorResponse
  { code = 401
  , response = ErrorPayload
      { error_message = "TOTP enabled for user."
      , userMessage = "TOTP is enabled for user, provide OTP and try again."
      , error = True
      }
  }

otpError :: ErrorResponse
otpError = ErrorResponse
  { code = 401
  , response = ErrorPayload
      { error_message = "Unauthorized."
      , userMessage = "Invalid OTP. Please try again."
      , error = True
      }
  }

dbError :: Text -> ErrorResponse
dbError x = ErrorResponse
  { code = 400
  , response = ErrorPayload
      { error_message = "Bad request."
      , userMessage = "Cannot find " <> x <> " in database."
      , error = True
      }
  }

dbsendError :: Text -> ErrorResponse
dbsendError x = ErrorResponse
  { code = 400
  , response = ErrorPayload
      { error_message = "Bad request."
      , userMessage = "Cannot create " <> x <> "."
      , error = True
      }
  }

dbUpdateError :: Text -> ErrorResponse
dbUpdateError x = ErrorResponse
  { code = 400
  , response = ErrorPayload
      { error_message = "Bad request."
      , userMessage = "Cannot update " <> x <> " in database."
      , error = True
      }
  }

urlError :: Text -> ErrorResponse
urlError x = ErrorResponse
  { code = 400
  , response = ErrorPayload
      { error_message = "Bad request."
      , userMessage = "Cannot find " <> x <> " in URL."
      , error = True
      }
  }

eulerAccessDenied :: Text -> ErrorResponse
eulerAccessDenied message = ErrorResponse
  { code = 403
  , response = ErrorPayload
      { error_message = "Forbidden." <> message
      , userMessage = "Access Denied. Unable to proceed."
      , error = True
      }
  }

accessDenied :: ErrorResponse
accessDenied = ErrorResponse
  { code = 401
  , response = ErrorPayload
      { error_message = "Access Denied"
      , userMessage = "Access Denied"
      , error = True
      }
  }

internalError :: ErrorResponse
internalError = ErrorResponse
  { code = 500
  , response = ErrorPayload
      { error_message = "Internal Server Error"
      , userMessage = "Internal Server Error"
      , error = True
      }
  }

sqlInjectionError :: ErrorResponse
sqlInjectionError = ErrorResponse
  { code = 500
  , response = ErrorPayload
      { error_message = "SQL Injection Attempted"
      , userMessage = "SQL Injection Attempted"
      , error = True
      }
  }

timeLimitExceeded :: ErrorResponse
timeLimitExceeded = ErrorResponse
  { code = 500
  , response = ErrorPayload
      { error_message = "Permitted Time Range Exceeded"
      , userMessage = "Permitted Time Range Exceeded"
      , error = True
      }
  }

mandateNotFound :: ErrorResponse
mandateNotFound = ErrorResponse
  { code = 400
  , response = ErrorPayload
      { error_message = "Mandate not found"
      , userMessage = "Mandate not found"
      , error = True
      }
  }

invalidMandateParams :: ECErrorResponse
invalidMandateParams = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Just (-1)
      , error_message = Just $ "invalid mandate params"
      , error_code = Just "INVALID_REQUEST"
      }
  }

invalidMandateMaxAmount :: Double -> ECErrorResponse
invalidMandateMaxAmount maxAm = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Just (-1)
      , error_message = Just $ "invalidMandateMaxAmount " <> show maxAm
      , error_code = Just "INVALID_MANDATE_MAX_AMOUNT"
      }
  }

invalidMandateFields ::  ECErrorResponse
invalidMandateFields = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Just (-1)
      , error_message = Just $ "Pass mandate_max_amount also to create a mandate order"
      , error_code = Just "INVALID_REQUEST"
      }
  }

mkValidationError :: [Text] -> ECErrorResponse
mkValidationError errs = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just $ show errs
      , error_code = Just "INVALID_REQUEST"
      }
  }

invalidCustomerId :: ECErrorResponse
invalidCustomerId = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Just (-1)
      , error_message = Just $ "pass customer_id"
      , error_code = Just "INVALID_CUSTOMER_ID"
      }
  }

txnValidationErrorResp' :: TxnValidationErrorResp -> ECErrorResponse
txnValidationErrorResp' TxnValidationErrorResp {..} = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just $ error_message
      , error_code = Just error_code
      }
  }

ecBadOrigin :: ECErrorResponse
ecBadOrigin = ECErrorResponse
  { code = 403
  , response = A.encodePretty $ ECErrorPayload
      { status = "bad_origin"
      , status_id = Nothing
      , error_message = Just "Bad Origin. Permission denied."
      , error_code = Just "bad_origin"
      }
  }

ecForbidden :: ECErrorResponse
ecForbidden =ECErrorResponse
  { code = 403
  , response = A.encodePretty $ ECErrorPayload
      { status = "Forbidden Resource"
      , status_id = Nothing
      , error_message = Just "Authentication is required for fetching order details"
      , error_code = Just "forbidden_resource"
      }
  }

ecAccessDenied :: ECErrorResponse
ecAccessDenied = ECErrorResponse
  { code = 401
  , response = A.encodePretty $ ECErrorPayload
      { status = "error"
      , status_id = Nothing
      , error_message = Nothing
      , error_code = Just "access_denied"
      }
  }

ecMandatoryFieldsMissing :: ECErrorResponse
ecMandatoryFieldsMissing = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "error"
      , status_id = Nothing
      , error_message = Just "Mandatory fields are missing"
      , error_code = Just "Mandatory fields are missing"
      }
  }

txnNotProcessError :: ECErrorResponse
txnNotProcessError = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just "Transaction is inprocess"
      , error_code = Just "invalid"
      }
  }

badRequest :: ECErrorResponse
badRequest = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "error"
      , status_id = Nothing
      , error_message = Nothing
      , error_code =  Just "invalid_request"
      }
  }

genericBadRequest :: Text -> ECErrorResponse
genericBadRequest errMsg = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "error"
      , status_id = Nothing
      , error_message = Just errMsg
      , error_code = Just "invalid_request"
      }
  }

merchantIdMissing :: ECErrorResponse
merchantIdMissing = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just "Merchant ID information is missing. Either authenticate or pass merchant_id."
      , error_code = Just "missing"
      }
  }

merchantAccountNull :: ECErrorResponse
merchantAccountNull = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just "[merchantAccount] cannot be null"
      , error_code = Just "nullable"
      }
  }

pmNotFound :: Text -> ECErrorResponse
pmNotFound txnId = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just ("txnCardInfo missing for txnid: " <> txnId)
      , error_code = Nothing
      }
  }

orderDoesNotExist :: Text -> ECErrorResponse
orderDoesNotExist orderId = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status        = "invalid_request_error"
      , error_code    = Nothing
      , error_message = Just ("Order with id = \"" <> orderId <> "\" does not exist.")
      , status_id     = Nothing
      }
  }

orderNotFound :: Text -> ECErrorResponse
orderNotFound orderId = ECErrorResponse
  { code = 200
  , response = A.encodePretty $ ECOrderStatusErrorPayload
      { status = "NOT_FOUND"
      , status_id = Just 40
      , error_message = Nothing
      , error_code = Nothing
      , order_id = Just orderId
      }
  }

orderAlreadyCreated :: Text -> ECErrorResponse
orderAlreadyCreated orderId = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECOrderStatusErrorPayload
      { status = "order_already_created"
      , status_id = Nothing
      , error_message = Just ("Order with id = \"" <> orderId <> "\" already created.")
      , error_code = Nothing
      , order_id = Just orderId
      }
  }

txnNotFound :: Text -> ECErrorResponse
txnNotFound tid = ECErrorResponse
  { code = 200
  , response = A.encodePretty $ ECTxnStatusErrorPayload
      { status = "NOT_FOUND"
      , status_id = Just 40
      , error_message = Nothing
      , error_code = Nothing
      , txn_uuid =  Just tid
      }
  }

customerNotFound :: ECErrorResponse
customerNotFound = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "NOT_FOUND"
      , status_id = Just 40
      , error_message = Just "Customer not found for this order"
      , error_code = Nothing
      }
  }

-- customerNotFound :: ECErrorResponse
-- customerNotFound = ECErrorResponse
--   { code = 200
--   , response = A.encodePretty $ ECErrorPayload
--       { status = "ERROR"
--       , status_id = Nothing
--       , error_message = Just "Customer not found"
--       , error_code = Just "INVALID_REQUEST"
--       }
--   }

unAuthorizedAccess :: ECErrorResponse
unAuthorizedAccess = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "Unauthorized access"
      , status_id = Nothing
      , error_message = Nothing
      , error_code = Nothing
      }
  }

cardNotFound :: ECErrorResponse
cardNotFound = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just "No card could be located for token"
      , error_code = Nothing
      }
  }

secondFactorNotFound :: ECErrorResponse
secondFactorNotFound = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just "Second Factor not found for this transaction id"
      , error_code = Nothing
      }
  }

gatewayNotSupported :: Text -> ECErrorResponse
gatewayNotSupported gwStr = ECErrorResponse
  { code = 500
  , response = A.encodePretty $ ECErrorPayload
      { status = "gateway_not_supported"
      , status_id = Nothing
      , error_message = Just $ "Gateway not supported" <> gwStr
      , error_code = Nothing
      }
  }

authAccntNotFound :: ECErrorResponse
authAccntNotFound = ECErrorResponse
  { code = 500
  , response = A.encodePretty $ ECErrorPayload
      { status = "authentication_account_not_found"
      , status_id = Nothing
      , error_message = Just $ "Authentication Account not found"
      , error_code = Nothing
      }
  }

sessionTimeout :: ECErrorResponse
sessionTimeout = ECErrorResponse
  { code = 500
  , response = A.encodePretty $ ECErrorPayload
      { status = "session_expried"
      , status_id = Nothing
      , error_message = Just $ "Session Expired"
      , error_code = Nothing
      }
  }

networkNotSupported :: Text -> ECErrorResponse
networkNotSupported err = ECErrorResponse
  { code = 500
  , response = A.encodePretty $ ECErrorPayload
      { status = "network_not_supported"
      , status_id = Nothing
      , error_message = Just $ "Network not supported " <> err
      , error_code = Nothing
      }
  }

paymentTypeNotSupported :: Text -> ECErrorResponse
paymentTypeNotSupported pm = ECErrorResponse
  { code = 500
  , response = A.encodePretty $ ECErrorPayload
      { status = "payment_type_not_supported"
      , status_id = Nothing
      , error_message = Just $ "Payment type not supported" <> pm
      , error_code = Nothing
      }
  }

authTypeNotSupported :: Text -> ECErrorResponse
authTypeNotSupported auth_type = ECErrorResponse
  { code = 500
  , response = A.encodePretty $ ECErrorPayload
      { status = "auth_type_not_supported"
      , status_id = Nothing
      , error_message = Just $ "Auth type not supported" <> auth_type
      , error_code = Nothing
      }
  }

generic500 :: Text -> ECErrorResponse
generic500 errMsg = ECErrorResponse
  { code = 500
  , response = A.encodePretty $ ECErrorPayload
      { status = "internal_error"
      , status_id = Nothing
      , error_message = Just errMsg
      , error_code = Nothing
      }
  }

txnOfferNotFound :: Text -> ECErrorResponse
txnOfferNotFound txnId = ECErrorResponse
  { code = 400
  , response = A.encodePretty $ ECErrorPayload
      { status = "invalid_request_error"
      , status_id = Nothing
      , error_message = Just ("txnOffer missing for txnid: " <> txnId)
      , error_code = Nothing
      }
  }
