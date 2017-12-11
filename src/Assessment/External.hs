-- Code for making requests to external services
{-# LANGUAGE OverloadedStrings #-}

module Assessment.External where

import Assessment.Log
import Assessment.Types
import qualified Assessment.Config as Config
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
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

getAssignmentTests :: Integer -> MaybeT IO Assignment
getAssignmentTests = getAssignmentTestsFromUrl Config.testsURL Config.testsPort

getAssignmentTestsFromUrl :: (Url scheme) -> Int -> Integer -> MaybeT IO Assignment
getAssignmentTestsFromUrl url urlPort ex = do
  bs <- liftIO $ req GET url NoReqBody lbsResponse $
    "id" =: ex <>
    port urlPort
  let parsed = (decode $ responseBody bs) :: (Maybe Assignment)
  case parsed of
    Nothing -> mzero
    Just a -> return a
