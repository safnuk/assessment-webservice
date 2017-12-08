{-# LANGUAGE DeriveGeneric         #-}

module Assessment.Log where

import Data.Aeson
import GHC.Generics (Generic)

testLog :: LogEntry
testLog = LogEntry {
    job_id = 2
  , exercise_id=1
  , code="hello"
  , compiler_msg = "error"
  , username = "bill"
  , timestamp = "2017" }

data LogEntry = LogEntry {
    job_id :: Integer
  , exercise_id :: Integer
  , code :: String
  , compiler_msg :: String
  , username :: String
  , timestamp :: String
} deriving (Show, Generic)

instance ToJSON LogEntry
instance FromJSON LogEntry
