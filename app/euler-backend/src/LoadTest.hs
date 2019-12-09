{-# LANGUAGE BangPatterns #-}

module LoadTest (main) where

import           EulerHS.Prelude

import App
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, withMVar)
import Euler.API.Order
import Network.HTTP.Client
import Network.Wai.Handler.Warp (run, Settings, setPort, defaultSettings, runSettings, setBeforeMainLoop)

import qualified Data.List as L (partition)
import qualified Euler.Server as Server
import qualified System.Process as SP

import Servant
import Servant.Client

import System.TimeIt

test :<|> txns        :<|> orderStatus
     :<|> orderCreate :<|> orderUpdate
     :<|> paymentStatus
     :<|> emptyServer = client Server.eulerAPI


stressTestPort :: Int
stressTestPort = 8080

nthreads :: Int
nthreads = 8

-- Clear screen escape sequence for ANSI terminals
clear :: IO ()
clear = putStr @String "\ESC[2J"

runBackend :: IO ThreadId
runBackend = do
  serverStartupLock <- newEmptyMVar

  let settings = setBeforeMainLoop (putMVar serverStartupLock ()) $
        setPort stressTestPort defaultSettings

  threadId <- forkIO $ runEulerBackendApp' settings
  readMVar serverStartupLock
  pure threadId

main :: IO ()
main = do
  -- runBackend

 ----------------------------------------------------------------------

  manager <- newManager defaultManagerSettings

  let baseUrl = BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost   = "localhost"
        , baseUrlPort   = stressTestPort
        , baseUrlPath   = ""
        }

  let runClient :: ClientM a -> IO (Either ClientError a)
      runClient a = runClientM a $ mkClientEnv manager baseUrl

----------------------------------------------------------------------

  let orderCreateBody = defaultOrderCreateRequest
        { order_id                          = "hello"
        , amount                            = 1.01
        , currency                          = Just "INR"
        , customer_id                       = Just "randomcustomer"
        , customer_email                    = Just "randomcustomer@mail.com"
        , customer_phone                    = Just "7076607677"
        , description                       = Just "description"
        , return_url                        = Just "http://somereturnUrl.non"
        , billing_address_first_name        = Just "Parth"
        , billing_address_city              = Just "Bengaluru"
        , shipping_address_city             = Just "Mumbai"
        , shipping_address_state            = Just "Parth"
        , udf7                              = Just "hello"
        , gateway_id                        = Just "8"
        }

  orderCreateVar <- runSuit $ runClient $ orderCreate orderCreateBody
  -- metricsVar     <- runSuit $ runClient metrics

  let mainLoop = do
        threadDelay $ 1000000 -- run in one time per second

        clear

----------------------------------------------------------------------

        results <- modifyMVar orderCreateVar $ pure . ([], )

        let (success, failure) = L.partition (isRight . snd) results

        ppt "ORDER CREATE. SUCCESS" success
        ppt "ORDER CREATE. FAILURE" failure

-- ----------------------------------------------------------------------

--         results <- modifyMVar metricsVar $ pure . ([], )

--         let (success, failure) = L.partition (isRight . snd) results

--         ppt "METRICS. SUCCESS" success
--         ppt "METRICS. FAILURE" failure

----------------------------------------------------------------------

        mainLoop

  mainLoop


----------------------------------------------------------------------
----------------------------------------------------------------------

ppt :: String -> [(Double, b)] -> IO ()
ppt title lst = do
  let times = (* 1000) . fst <$> lst

  putStrLn @String "----------------------------------------------------------------------"
  putStrLn @String title
  putStrLn @String "----------------------------------------------------------------------"
  fromMaybe (putStrLn @String "Nothing here...") $ do
    min' <- min_ times
    max' <- max_ times
    p50  <- percentile 50 times
    p90  <- percentile 90 times
    pure $ do
      putStrLn @String $ "Performance [req/sec]: " <> show (length lst)
      putStrLn @String $ "Minimum run time [msec]: " <> show min'
      putStrLn @String $ "Maximum run time [msec]: " <> show max'
      putStrLn @String $ "Percentile 50: " <> show p50
      putStrLn @String $ "Percentile 90: " <> show p90
  putStrLn @String ""

min_ :: Ord a => [a] -> Maybe a
min_ []  = Nothing
min_ lst = Just $ minimum lst

max_ :: Ord a => [a] -> Maybe a
max_ []  = Nothing
max_ lst = Just $ maximum lst

-- so rough!
percentile :: Ord a => Int -> [a] -> Maybe a
percentile _ []  = Nothing
percentile p lst = Just $ sort lst !! (length lst * p `div` 100)

defaultOrderCreateRequest :: OrderCreateRequest
defaultOrderCreateRequest = OrderCreateRequest
  { order_id                          = error "undefiend order_id :: Text"
  , amount                            = error "undefiend amount :: Double"
  , currency                          = Nothing
  , customer_id                       = Nothing
  , customer_email                    = Nothing
  , customer_phone                    = Nothing
  , description                       = Nothing
  , return_url                        = Nothing
  , product_id                        = Nothing
  , billing_address_first_name        = Nothing
  , billing_address_last_name         = Nothing
  , billing_address_line1             = Nothing
  , billing_address_line2             = Nothing
  , billing_address_line3             = Nothing
  , billing_address_city              = Nothing
  , billing_address_state             = Nothing
  , billing_address_country           = Nothing
  , billing_address_postal_code       = Nothing
  , billing_address_phone             = Nothing
  , billing_address_country_code_iso  = Nothing -- Default value: IND
  , shipping_address_first_name       = Nothing
  , shipping_address_last_name        = Nothing
  , shipping_address_line1            = Nothing
  , shipping_address_line2            = Nothing
  , shipping_address_line3            = Nothing
  , shipping_address_city             = Nothing
  , shipping_address_state            = Nothing
  , shipping_address_country          = Nothing
  , shipping_address_postal_code      = Nothing
  , shipping_address_phone            = Nothing
  , shipping_address_country_code_iso = Nothing -- Default value: IND
  , udf1                              = Nothing
  , udf2                              = Nothing
  , udf3                              = Nothing
  , udf4                              = Nothing
  , udf5                              = Nothing
  , udf6                              = Nothing
  , udf7                              = Nothing
  , udf8                              = Nothing
  , udf9                              = Nothing
  , udf10                             = Nothing
  , metaData                          = Nothing
  , gateway_id                        = Nothing -- converted to Int, why Text?
  , mandate_max_amount                = Nothing
  , auto_refund                       = Nothing
  }

runSuit :: IO a -> IO (MVar [(Double, a)])
runSuit io = do
  summaryVar <- newMVar []
  threadIds  <- replicateM nthreads $ forkIO $
    let loop = do
          result <- timeItT io
          modifyMVar_ summaryVar $ pure . (result :)
          loop
    in loop
  pure summaryVar


