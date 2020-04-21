{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WebService.Types.ReqResp where

import EulerHS.Prelude


newtype EmptyReq = EmptyReq () deriving (FromJSON, ToJSON)
newtype EmptyResp = EmptyResp () deriving (Eq, Show, FromJSON, ToJSON)

emptyReq :: EmptyReq
emptyReq = EmptyReq ()

emptyResp :: EmptyResp
emptyResp = EmptyResp()