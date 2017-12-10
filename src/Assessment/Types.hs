{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Assessment.Types where 

import Data.Aeson (
    FromJSON
  , parseJSON
  , toJSON
  , ToJSON
  , Value (String))
import GHC.Generics (Generic)

testSubmission :: Submission
testSubmission = Submission {
    username = "user"
  , exercise_id = 5
  , code = "print;"
}

testResponse :: Response
testResponse = Response {
    job_id = 2
  , status = Done
  , compiler_msg = ""
  , test_results = [Pass, Fail]
}

testAssignment :: Assignment
testAssignment = Assignment {
    inputs = ["hello", "world"]
  , outputs = ["hello, hello", "hello, world"]
}

data QueueEntry = QueueEntry Submission Assignment deriving (Show, Generic)
instance ToJSON QueueEntry
instance FromJSON QueueEntry

data Assignment = Assignment {
    inputs :: [String]
  , outputs :: [String]
} deriving (Show, Generic)
instance ToJSON Assignment
instance FromJSON Assignment

data Submission = Submission {
    username :: String
  , exercise_id :: Integer
  , code :: String
} deriving (Show, Generic)
instance ToJSON Submission
instance FromJSON Submission

data Status = Processing | Done deriving (Show)

instance ToJSON Status where
  toJSON Processing = String "PROCESSING"
  toJSON Done = String "DONE"

instance FromJSON Status where
  parseJSON (String "PROCESSING") = pure Processing
  parseJSON (String "DONE") = pure Done
  parseJSON _ = fail "Unknown status"


data TestResult = Pass | Fail deriving (Show)

instance ToJSON TestResult where
  toJSON Pass = String "PASS"
  toJSON Fail = String "FAIL"

instance FromJSON TestResult where
  parseJSON (String "PASS") = pure Pass
  parseJSON (String "FAIL") = pure Fail
  parseJSON _ = fail "Unknown test result"


data Response = Response {
    job_id :: Integer,
    status :: Status,
    compiler_msg :: String,
    test_results :: [TestResult]
} deriving (Show, Generic)

instance ToJSON Response
instance FromJSON Response
