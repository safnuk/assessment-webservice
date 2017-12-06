{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Assessment.Types where 

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Data.Aeson (
    FromJSON
  , object
  , parseJSON
  , toJSON
  , ToJSON
  , Value (String)
  , (.:)
  , (.=))
import qualified Data.Aeson as A
import GHC.Generics (Generic)
import System.Hworker

testSubmission :: Submission
testSubmission = Submission {
    username = "user"
  , exercise_id = 5
  , code = "print;"
}

data SubmissionResponse = Either TooManySubmissions JobID 
data TooManySubmissions = TooManySubmissions deriving (Show)

data JobID = JobID Integer deriving (Show)
instance ToJSON JobID where
  toJSON (JobID x) = object [ "job_id" .= x ]
instance FromJSON JobID where
  parseJSON (A.Object o) = JobID <$> o .: "job_id"
  parseJSON _ = fail "Failed to parse JobID!"

instance Job State JobID where
  job (State mvar) j =
    do v <- takeMVar mvar
       putMVar mvar (v + 1)
       submission <- getSubmission j
       processSubmission submission
       return Success

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

data State = State (MVar Int)

getSubmission:: JobID -> IO (Submission)
getSubmission (JobID j) = do
  return testSubmission

putSubmission :: JobID -> Submission -> IO () 
putSubmission = undefined

nextJobID :: IO (JobID)
nextJobID = undefined

hworker :: IO (Hworker State JobID)
hworker = do
  mvar <- newMVar 3
  create "submissions" (State mvar)

processSubmission :: Submission -> IO ()
processSubmission submission = do
  putStrLn $ show submission
