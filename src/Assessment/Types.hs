{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Assessment.Types where 

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Aeson (
    FromJSON
  , decode
  , object
  , parseJSON
  , toJSON
  , ToJSON
  , Value (String)
  , (.:)
  , (.=))
import qualified Data.Aeson as A
import qualified Data.Binary as DB 
import qualified Data.ByteString.Lazy as LB
import qualified Database.Redis as R
import GHC.Generics (Generic)
import System.Hworker

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

data SubmissionResponse = Either TooManySubmissions JobID 
data TooManySubmissions = TooManySubmissions deriving (Show)

data JobID = JobID Integer deriving (Show)
instance ToJSON JobID where
  toJSON (JobID x) = object [ "job_id" .= x ]
instance FromJSON JobID where
  parseJSON (A.Object o) = JobID <$> o .: "job_id"
  parseJSON _ = fail "Failed to parse JobID!"

jobToInteger :: JobID -> Integer
jobToInteger (JobID j) = j

instance Job State JobID where
  job (State mvar) j =
    do v <- takeMVar mvar
       putMVar mvar (v + 1)
       qe <- runMaybeT $ getQueueEntry j
       case qe of
         Nothing -> return $ Failure "Job not found"
         Just q -> do
           processQueueEntry q
           return Success

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

data State = State (MVar Int)

getQueueEntry :: JobID -> MaybeT IO QueueEntry
getQueueEntry (JobID j) = do
  conn <- liftIO $ R.checkedConnect R.defaultConnectInfo
  r <- liftIO $ R.runRedis conn $ do
    R.get (LB.toStrict $ DB.encode j)
  case r of
    Left _ -> mzero
    -- Right s -> MaybeT $ return $ (decode <*> LB.fromStrict <$> s)
    -- :: (Maybe QueueEntry)
    Right Nothing -> mzero
    Right (Just s) -> do
      let d = (decode $ LB.fromStrict s) :: (Maybe QueueEntry)
      case d of
        Nothing -> mzero
        Just qe -> return qe

processQueueEntry :: QueueEntry -> IO ()
processQueueEntry qe = do
  putStrLn $ show qe

hworker :: IO (Hworker State JobID)
hworker = do
  mvar <- newMVar 0
  create "submissions" (State mvar)
