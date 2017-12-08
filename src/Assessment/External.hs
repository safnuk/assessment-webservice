{-# LANGUAGE OverloadedStrings #-}

module Assessment.External where

import Assessment.Log
import Assessment.Types
import qualified Assessment.Config as Config
import Control.Exception (throwIO)
import Data.Aeson
import Data.Monoid ((<>))
import Network.HTTP.Req

instance MonadHttp IO where
  handleHttpException = throwIO

postLog :: LogEntry -> IO ()
postLog = postLogToUrl Config.logURL Config.logPort

postLogToUrl :: (Url scheme) -> Int -> LogEntry -> IO ()
postLogToUrl url urlPort le = do
  v <- req POST url (ReqBodyJson le) jsonResponse $
    port urlPort
  print (responseBody v :: Value)

getAssignmentTests :: Integer -> IO (Assignment)
getAssignmentTests = getAssignmentTestsFromUrl Config.testsURL Config.testsPort

getAssignmentTestsFromUrl :: (Url scheme) -> Int -> Integer -> IO (Assignment)
getAssignmentTestsFromUrl url urlPort ex = do
  bs <- req GET url NoReqBody lbsResponse $
    "id" =: ex <>
    port urlPort
  let parsed = (decode $ responseBody bs) :: (Maybe Assignment)
  case parsed of
    Nothing -> undefined
    Just a -> return a
