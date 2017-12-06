{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), encode, object)
import qualified Data.Binary as DB 
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy.Encoding as TE
import qualified Database.Redis as R
import Network.HTTP.Types.Status as Status
import Web.Scotty as WS
import System.Hworker
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
  conn <- R.checkedConnect R.defaultConnectInfo
  hw <- hworker
  r <- liftIO $ R.runRedis conn $ do
    R.incr "job_id"
  case r of
    Left err -> undefined
    Right j -> do
        _ <- liftIO $ R.runRedis conn $ do
          R.set (LB.toStrict $ DB.encode j) (LB.toStrict $ encode submission)
        _ <- queue hw (JobID j)
        return $ JobID j

submissionsGetStatus :: ActionM ()
submissionsGetStatus = do
  jobId <- ((param "id") :: ActionM Integer) `rescue` (const next)
  conn <- liftIO $ R.checkedConnect R.defaultConnectInfo
  r <- liftIO $ R.runRedis conn $ do
    R.get (LB.toStrict $ DB.encode jobId)
  case r of
    Left err -> undefined
    Right Nothing -> undefined
    Right (Just submission) -> do
      text $ TE.decodeUtf8 $ LB.fromStrict submission
  -- let response = Response jobId Done "" [Pass, Fail]
  -- json response

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
