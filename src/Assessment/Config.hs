{-# LANGUAGE OverloadedStrings #-}

module Assessment.Config where

import Network.HTTP.Req

-- logURL :: (Url scheme)
logPort :: Int
-- testsURL :: Url Http
testsPort :: Int
maxRequestFrequency :: Integer
requestCounterTimeout :: Integer
javaClass :: String
compileCmd :: String
runCmd :: String

logPort = 3001
testsPort = 3001
logURL = http "localhost" /: "log" /: "assessment"
testsURL = http "localhost" /: "assignment" /: "tests"
maxRequestFrequency = 2
requestCounterTimeout = 60

javaClass = "Test"
compileCmd = "javac"
runCmd = "java"
