{-# LANGUAGE OverloadedStrings #-}

module Assessment.Config where

import Network.HTTP.Req

-- logURL :: (Url scheme)
logPort :: Int
-- testsURL :: (Url scheme)
testsPort :: Int

logURL = http "localhost" /: "log" /: "assessment"
logPort = 3001
testsURL = http "localhost" /: "assignment" /: "tests"
testsPort = 3001


