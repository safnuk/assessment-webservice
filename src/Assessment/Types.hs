{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Assessment.Types where 

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Data.Aeson (
    FromJSON
  , object
  , parseJSON
  , Value (String)
  , ToJSON
  , toJSON
  , (.=))
import GHC.Generics (Generic)
import System.Hworker


testSubmission :: Submission
testSubmission = Submission {
    username = "user"
  , exercise_id = 5
  , code = "print;"
}

data Submission = Submission {
    username :: String
  , exercise_id :: Int
  , code :: String
} deriving (Show, Generic)
instance ToJSON Submission
instance FromJSON Submission

-- allows Submission datatype to be added to worker queue
instance Job State Submission where
  job (State mvar) submission =
    do v <- takeMVar mvar
       putMVar mvar (v + 1)
       putStrLn $ show submission
       return Success

data SubmissionResponse = Either TooManySubmissions JobID

data TooManySubmissions = TooManySubmissions deriving (Show)
data JobID = JobID Int deriving (Show)
instance ToJSON JobID where
  toJSON (JobID x) = object [ "job_id" .= x ]

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
    job_id :: Int,
    status :: Status,
    compiler_msg :: String,
    test_results :: [TestResult]
} deriving (Show, Generic)

instance ToJSON Response
instance FromJSON Response


data PrintJob = Print deriving (Generic, Show)
data State = State (MVar Int)
instance ToJSON PrintJob
instance FromJSON PrintJob

-- instance Job State PrintJob where
--   job (State mvar) Print =
--     do v <- takeMVar mvar
--        putMVar mvar (v + 1)
--        putStrLn $ "A(" ++ show v ++ ")"
--        return Success

-- hworker :: IO (Hworker State PrintJob)
-- hworker = do
--   mvar <- newMVar 3
--   create "printer" (State mvar)

hworker :: IO (Hworker State Submission)
hworker = do
  mvar <- newMVar 3
  create "submissions" (State mvar)
