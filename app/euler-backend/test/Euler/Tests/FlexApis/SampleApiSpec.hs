{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- TODO: Fix the placeholders for SampleResponse 
module Euler.Tests.FlexApis.SampleApiSpec where

import           EulerHS.Prelude           hiding (pack, unpack, get)

import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import qualified Control.Exception                as EX (bracket)
import           Control.Lens              hiding (Context)
import           Data.Aeson                       
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as BC
import qualified Data.ByteString.Lazy.UTF8        as BSU
import qualified Data.HashMap.Strict              as HM
import           Data.Text                        (Text, pack, unpack)
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.Wai.Middleware.Routed
                (routedMiddleware)
import           Test.RandomStrings
                (randomString, onlyAlphaNum, randomASCII)
import           Servant
import           Servant.Client
import           Servant.Server
--import           Servant.QuickCheck
--import           Servant.QuickCheck.Internal (serverDoesntSatisfy)
import           Test.Hspec
import           Test.Hspec.Wai                        
                (with, shouldRespondWith, get, postHtmlForm)
import qualified Test.Hspec.Wai.JSON              as WJ
import           Test.Hspec.Wai.Matcher
import           WebService.ContentType               
                (WrappableJSON, JavascriptWrappedJSON, mkDynContentTypeMiddleware, wrapper, throwJsonError)
import           WebService.FlexCasing                
                (QueryParamC, FlexCasingResponse, mkFlexCaseMiddleware, 
                casing, flexCasingOptions, casingParam, unsupportedCase)
import           WebService.PostRewrite 
                (mkPostToGetMiddleware)

-- sample response payload
data Sample = Sample 
  { someField :: Text
  } deriving (Eq, Show, Generic)


defaultSample = Sample "abc"

-- sample error response
data JsonError = JsonError
  { status       :: Text
  , errorCode    :: Text
  , errorMessage :: Text
  } deriving (Generic, ToJSON)

defaultJsonError = JsonError "invalid_request_error" "nullable" "[merchantId] cannot be null"

data SampleResponse = SampleResponse 
  { payload    :: Sample
  , caseStyle :: Text
  , callback  :: Maybe Text
  }

instance FlexCasingResponse SampleResponse where    
  casing = caseStyle

-- for wrapped JS repsonse
instance WrappableJSON SampleResponse where
  wrapper p = case (callback $ p) of
    Just s ->  BC.pack $ unpack $ s
    Nothing -> undefined        

-- for JSON response
instance ToJSON SampleResponse where    
  toJSON v = genericToJSON (flexCasingOptions v) (payload v)

-- dummy deserializer
instance FromJSON SampleResponse where
  parseJSON = undefined

type SampleApi =
  "path" :> "to" :> "sample" :> QueryParamC "objectId" String :> QueryParamC "callback" String :> QueryParamC "casing" String:> Get '[JSON, JavascriptWrappedJSON] SampleResponse

sampleApp :: Application
sampleApp = 
  let 
    flexCaseMiddleware = routedMiddleware (["path", "to", "sample"] ==) 
      (mkPostToGetMiddleware
       . mkFlexCaseMiddleware "objectId" 
       . mkDynContentTypeMiddleware "callback") 
  in
    flexCaseMiddleware $ serve (Proxy :: Proxy SampleApi) sampleServer

sampleServer :: Server SampleApi
sampleServer = sample

sample :: Maybe String -> Maybe String -> Maybe String -> Handler SampleResponse
sample (Just objectId) callback (Just casing) = do
  when (casing == unsupportedCase) $ throwJsonError err400 $ defaultJsonError
  return $ SampleResponse { 
    payload = defaultSample { someField = pack objectId }
    , caseStyle = pack casing
    , callback = pack <$> callback
  }
sample _ _ _ = throwError err500

asciiAlphanum = onlyAlphaNum randomASCII

rand10 = liftIO $ randomString asciiAlphanum 10

json200 b = 200 
  { matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
  , matchBody = fromString b
  }

defaultJson400Response = 400 
  { matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
  , matchBody = fromString $ BSU.toString $ encode defaultJsonError
  }

testPort :: Int
testPort = 8080

spec :: Spec
spec = with (pure $ sampleApp) $ do 

  -- test flexible content response
  describe "GET, adaptive content-types" $ do
    it "`application/json` when no `callback`" $ do                
      get "/path/to/sample?objectId=123" `shouldRespondWith` 200 
        { matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"] }
    
    it "`application/javascript` when `callback` is present" $ do                
      get "/path/to/sample?objectId=123&callback=someFun" `shouldRespondWith` 200 
        { matchHeaders = ["Content-Type" <:> "application/javascript;charset=utf-8"] }

  -- test flexible response casing
  describe "GET, adaptive cases for JSON-response" $ do
    it "PascalCase is NOT ok" $ do
      get "/path/to/sample?ObjectId=123" `shouldRespondWith` defaultJson400Response

    it "camelCase is OK" $ do
      v <- rand10        
      let res = "{\"someField\":\""<> v <>"\"}"
      get ("/path/to/sample?objectId=" <> BS.pack v) `shouldRespondWith` json200 res

    -- currently we do not support this casing      
    -- it "kebab-case is NOT ok" $ do
    --   get "/path/to/sample?object-id=123" `shouldRespondWith` defaultJson400Response
    
    -- currently we do not support this casing
    -- it "snake_Case is NOT ok" $ do
    --   get "/path/to/sample?object_Id=123" `shouldRespondWith` defaultJson400Response
    
    it "quiet_snake_case is OK" $ do
      v <- rand10
      let res = "{\"some_field\":\""<> v <>"\"}"
      get ("/path/to/sample?object_id=" <> BS.pack v) `shouldRespondWith` json200 res

    -- currently we do not support this casing
    -- it "SCREAMING_SNAKE_CASE is NOT ok" $ do
    --   get "/path/to/sample?OBJECT_ID=123" `shouldRespondWith` defaultJson400Response
      
  --
  describe "POST-to-GET transformation" $ do
    
    it "POST rewrite for camelCase works" $ do        
      postHtmlForm "/path/to/sample" [("objectId", "123")] `shouldRespondWith` [WJ.json|{someField: "123"}|]

    it "POST rewrite for snake_case works" $ do        
      postHtmlForm "/path/to/sample" [("object_id", "123")] `shouldRespondWith` [WJ.json|{some_field: "123"}|]
