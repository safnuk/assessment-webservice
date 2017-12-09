{-# LANGUAGE OverloadedStrings #-}

module Assessment.Config where

import Network.HTTP.Req

-- logURL :: (Url scheme)
logPort :: Int
-- testsURL :: Url Http
testsPort :: Int
maxRequestFrequency :: Integer
requestCounterTimeout :: Integer

logURL = http "localhost" /: "log" /: "assessment"
logPort = 3001
testsURL = http "localhost" /: "assignment" /: "tests"
testsPort = 3001
maxRequestFrequency = 2
requestCounterTimeout = 60


