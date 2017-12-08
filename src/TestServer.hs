{-# LANGUAGE OverloadedStrings #-}

module Main where

import Assessment.Log (LogEntry)
import Assessment.Types
import Network.HTTP.Types.Status as Status
import Web.Scotty as WS

tests = [
    Assignment {inputs=["hello"], outputs=["world"]}
  , Assignment {inputs=["bill"], outputs=["nye"]} ]

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
  json log


badPostLog :: ActionM ()
badPostLog = do
  WS.status Status.badRequest400

main :: IO ()
main = do
    putStrLn "Starting test server..."
    scotty 3001 $ do
        routes
