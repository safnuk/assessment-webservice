{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), object)
import qualified Database.Redis as R
import Network.HTTP.Types.Status as Status
import Web.Scotty as WS
import System.Hworker
import Assessment.Types

routes :: ScottyM ()
routes = do
  get "/health" healthCheck
  get "/submissions/status" submissionsGetStatus
  -- any errors in submissionsGetStatus drops down to submissionsGetStatusFailure
  get "/submissions/status" submissionsGetStatusFailure
  post "/submissions/status" requestNotAllowed
  post "/submissions" submissionsPostNew
  post "/submissions" submissionsPostNewFailure
  get "/submissions" requestNotAllowed


addToQueue :: Submission -> IO (JobID Integer)
addToQueue submission = do
  conn <- R.checkedConnect R.defaultConnectInfo
  hw <- hworker
  r <- R.runRedis conn $ do
    R.incr "job_id"
  case r of
    Left err -> undefined
    Right j -> do
        _ <- queue hw submission
        return $ JobID j

submissionsGetStatus :: ActionM ()
submissionsGetStatus = do
  jobId <- ((param "id") :: ActionM Int) `rescue` (const next)
  let response = Response jobId Done "" [Pass, Fail]
  json response

submissionsGetStatusFailure :: ActionM ()
submissionsGetStatusFailure = invalidRequest Status.badRequest400

submissionsPostNew :: ActionM ()
submissionsPostNew = do
  submission <- (jsonData :: ActionM Submission) `rescue` (const next)
  tooMany <- liftIO $ tooManyRequests submission
  if tooMany
    then invalidRequest Status.tooManyRequests429
    else do response <- liftIO $ addToQueue submission
            json response 

tooManyRequests :: Submission -> IO (Bool)
tooManyRequests _ = return False

requestNotAllowed :: ActionM ()
requestNotAllowed = invalidRequest Status.methodNotAllowed405

submissionsPostNewFailure :: ActionM ()
submissionsPostNewFailure = invalidRequest Status.badRequest400

invalidRequest :: Status.Status -> ActionM ()
invalidRequest x = do
  json $ object [ "error" .= ("Invalid request" :: String) ]
  WS.status x

-- in case we use kubernetes
healthCheck :: ActionM ()
healthCheck = do
  text "UP"


main :: IO ()
main = do
    hw <- hworker
    _ <- forkIO (monitor hw)
    putStrLn "Starting server..."
    scotty 3000 $ do
        routes
