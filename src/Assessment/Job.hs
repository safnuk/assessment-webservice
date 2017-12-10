{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Assessment.Job where 

import Assessment.External
import Assessment.Log
import qualified Assessment.Types as Types
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Aeson (
    FromJSON
  , decode
  , object
  , parseJSON
  , toJSON
  , ToJSON
  , (.:)
  , (.=))
import qualified Data.Aeson as A
import qualified Data.Binary as DB 
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Database.Redis as R
import System.Hworker


data State = State (MVar Int)

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

jobToByteString :: JobID -> ByteString
jobToByteString = LB.toStrict . DB.encode . jobToInteger

getQueueEntry :: JobID -> MaybeT IO Types.QueueEntry
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
      let d = (decode $ LB.fromStrict s) :: (Maybe Types.QueueEntry)
      case d of
        Nothing -> mzero
        Just qe -> return qe

processQueueEntry :: JobID -> Types.QueueEntry -> IO ()
processQueueEntry (JobID j) (Types.QueueEntry submission assignment) = do
  let codeSample = Types.code submission
  (msg, program) <- compileCode codeSample
  results <- sequence $ map (runProgram program) $ Types.inputs assignment
  let pairs = zip results $ Types.outputs assignment
  let testResults = map validateTest pairs
  let response = Types.Response {
    Types.job_id = j,
    Types.status = Types.Done,
    Types.compiler_msg = msg,
    Types.test_results = testResults
  }
  let logEntry = LogEntry {
      job_id = j
    , exercise_id = Types.exercise_id submission
    , code = codeSample
    , compiler_msg = msg
    , username = Types.username submission
    , timestamp = "2017"
  }
  putResponse response
  postLog logEntry
  putStrLn $ show response

compileCode :: String -> IO (String, String)
compileCode = undefined

runProgram :: String -> String -> IO (String)
runProgram = undefined

validateTest :: (Eq a) => (a, a) -> Types.TestResult
validateTest (x, y) = 
  if x == y
    then Types.Pass
    else Types.Fail

putResponse :: Types.Response -> IO ()
putResponse = undefined

instance Job State JobID where
  job (State mvar) j =
    do v <- takeMVar mvar
       putMVar mvar (v + 1)
       qe <- runMaybeT $ getQueueEntry j
       case qe of
         Nothing -> return $ Failure "Job not found"
         Just q -> do
           processQueueEntry j q
           return Success

hworker :: IO (Hworker State JobID)
hworker = do
  mvar <- newMVar 0
  create "submissions" (State mvar)
