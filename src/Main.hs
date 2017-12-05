{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), object)
import Network.HTTP.Types.Status as Status
import Web.Scotty as WS
import System.Hworker
import Assessment.Types

routes :: ScottyM ()
routes = do
  get "/test" testQueue
  get "/health" healthCheck
  get "/submissions/status" submissionsGetStatus
  get "/submissions/status" submissionsGetStatusFailure
  post "/submissions/status" submissionsPostStatus
  post "/submissions" submissionsPostNew
  post "/submissions" submissionsPostNewFailure
  get "/submissions" submissionsGetNew


testQueue :: ActionM ()
testQueue = do
  response <- liftIO $ addToQueue testSubmission
  json response

addToQueue :: Submission -> IO (JobID)
addToQueue submission = do
  hw <- hworker
  _ <- queue hw submission
  return $ JobID 1

submissionsGetStatus :: ActionM()
submissionsGetStatus = do
  jobId <- ((param "id") :: ActionM Int) `rescue` (const next)
  let response = Response jobId Done "" [Pass, Fail]
  json response

submissionsPostStatus :: ActionM()
submissionsPostStatus = invalidRequest Status.methodNotAllowed405

submissionsGetStatusFailure :: ActionM()
submissionsGetStatusFailure = invalidRequest Status.badRequest400

submissionsPostNew :: ActionM ()
submissionsPostNew = do
  submission <- (jsonData :: ActionM Submission) `rescue` (const next)
  response <- liftIO $ addToQueue submission
  json response 

submissionsGetNew :: ActionM()
submissionsGetNew = invalidRequest Status.methodNotAllowed405

submissionsPostNewFailure :: ActionM()
submissionsPostNewFailure = invalidRequest Status.badRequest400

invalidRequest :: Status.Status -> ActionM()
invalidRequest x = do
  json $ object [ "error" .= ("Invalid request" :: String) ]
  WS.status x

healthCheck :: ActionM()
healthCheck = do
  text "UP"


main :: IO ()
main = do
    hw <- hworker
    _ <- forkIO (monitor hw)
    putStrLn "Starting server..."
    scotty 3000 $ do
        routes
