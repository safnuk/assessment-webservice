{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), object)
import Network.HTTP.Types.Status as Status
import Web.Scotty as WS
import System.Hworker
import Assessment.Data
import Assessment.Types

routes :: ScottyM ()
routes = do
  get "/health" healthCheck
  get "/submissions/status" submissionsGetStatus
  -- any errors in submissionsGetStatus drops down to
  -- submissionsGetStatusFailure
  get "/submissions/status" submissionsGetStatusFailure
  post "/submissions/status" requestNotAllowed
  post "/submissions" submissionsPostNew
  post "/submissions" submissionsPostNewFailure
  get "/submissions" requestNotAllowed

addToQueue :: Submission -> IO (JobID)
addToQueue submission = do
  j <- nextJobID
  putSubmission j submission
  enqueue j
  return j

submissionsGetStatus :: ActionM ()
submissionsGetStatus = do
  j <- ((param "id") :: ActionM Integer) `rescue` (const next)
  response <- liftIO $ getStatusResponse (JobID j)
  json response

submissionsGetStatusFailure :: ActionM ()
submissionsGetStatusFailure = invalidRequest Status.badRequest400

submissionsPostNew :: ActionM ()
submissionsPostNew = do
  submission <- (jsonData :: ActionM Submission) `rescue` (const next)
  tooMany <- liftIO $ tooManyRequests submission 2
  if tooMany
    then invalidRequest Status.tooManyRequests429
    else do response <- liftIO $ addToQueue submission
            json response 

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
