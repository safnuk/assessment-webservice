-- Server mocking the logging and assignment services for testing purposes
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Assessment.Log (LogEntry)
import Assessment.Types
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status as Status
import Web.Scotty as WS

tests = [
    Assignment {inputs=["World", "John"], outputs=["Hello, World", "Hello, John"]}
  , Assignment {inputs=["1 2", "1 2 3"], outputs=["3", "6"]} ]

routes :: ScottyM ()
routes = do
  get "/assignment/tests" getTests
  post "/log/assessment" postLog
  post "/log/assessment" badPostLog
  

getTests :: ActionM ()
getTests = do
  t <- ((param "id") :: ActionM Integer)
  case t of
    0 -> json $ tests !! 0
    1 -> json $ tests !! 1
    _ -> WS.status Status.badRequest400 

postLog :: ActionM ()
postLog = do
  log <- (jsonData :: ActionM LogEntry) `rescue` (const next)
  liftIO $ print $ "Logged: " ++ (show log)
  json log


badPostLog :: ActionM ()
badPostLog = do
  WS.status Status.badRequest400

main :: IO ()
main = do
    putStrLn "Starting test server..."
    scotty 3001 $ do
        routes
