{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, (.=), object)
-- import Data.Monoid ((<>))
import GHC.Generics
import Network.HTTP.Types.Status as Status
import Web.Scotty as WS
import System.Hworker
import Assessment.Types

data Submission = Submission { username :: String, exercise_id :: Int, code :: String } deriving (Show, Generic)
instance ToJSON Submission
instance FromJSON Submission

data Response = Response { job_id :: Int, status :: String, compiler_msg :: String, test_results :: [String] } deriving (Show, Generic)
instance ToJSON Response
instance FromJSON Response

routes :: ScottyM ()
routes = do
  get "/test" $ join $ liftIO addToQueue
  get "/health" healthCheck
  get "/submissions/status" submissionsGetStatus
  get "/submissions/status" submissionsGetStatusFailure
  post "/submissions/status" submissionsPostStatus
  post "/submissions" submissionsPostNew
  post "/submissions" submissionsPostNewFailure
  get "/submissions" submissionsGetNew
  -- get "/hello/:name" helloPersonalized
  -- get "/users" $ do
  --   json allUsers
  -- get "/users/:id" $ do
  --   id <- param "id"
  --   json (filter (matchesId id) allUsers)
  -- post "/users" usersPost
  -- post "/users" usersPostFailure


addToQueue :: IO (ActionM())
addToQueue = do
  hw <- hworker
  queue hw Print
  return $ text "Submitted job to queue"

submissionsGetStatus :: ActionM()
submissionsGetStatus = do
  jobId <- ((param "id") :: ActionM Int) `rescue` (const next)
  let response = Response jobId "DONE" "" ["PASS", "FAIL"]
  json response

submissionsPostStatus :: ActionM()
submissionsPostStatus = invalidRequest Status.methodNotAllowed405

submissionsGetStatusFailure :: ActionM()
submissionsGetStatusFailure = invalidRequest Status.badRequest400

submissionsPostNew :: ActionM()
submissionsPostNew = do
  submission <- (jsonData :: ActionM Submission) `rescue` (const next)
  json submission 

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
    forkIO (monitor hw)
    putStrLn "Starting server..."
    scotty 3000 $ do
        routes
