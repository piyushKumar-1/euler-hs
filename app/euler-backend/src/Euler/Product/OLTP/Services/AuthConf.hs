module Euler.Product.OLTP.Services.AuthConf
  ( module X
  , mkKeyAuthService
  , mkTokenAuthService
  , mkKeyTokenAuthService
  ) where

import Euler.Product.OLTP.Services.Auth.AuthService         as X
import Euler.Product.OLTP.Services.Auth.AuthKeyService      as K
import Euler.Product.OLTP.Services.Auth.AuthTokenService    as T
import Euler.Product.OLTP.Services.Auth.AuthKeyTokenService as KT

mkKeyAuthService :: X.SHandle
mkKeyAuthService = K.newHandle

mkTokenAuthService :: X.SHandle
mkTokenAuthService = T.newHandle

mkKeyTokenAuthService :: X.SHandle
mkKeyTokenAuthService = KT.newHandle mkKeyAuthService mkTokenAuthService
