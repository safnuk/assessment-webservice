-- Do not change ANYTHING between the horizontal lines.
-- You have been warned!
-- -------------------------------------------------
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

-- -------------------------------------------------
-- Change these values as needed, but preserve the formatting.

-- Access port for the logging service 
logPort = 3001
-- Access port for the assignment service
testsPort = 3001
-- URL for the logging service
logURL = http "localhost" /: "log" /: "assessment"
-- URL for the assignment service
testsURL = http "localhost" /: "assignment" /: "tests"

-- Maximum number of requests by a single user in the specified amount of time
maxRequestFrequency = 2
requestCounterTimeout = 60

-- Java class name for all code samples submitted to the service
javaClass = "Test"
-- Program used to compile code samples
compileCmd = "javac"
-- Program used to run the compiled binary
runCmd = "java"
