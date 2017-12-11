{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Assessment.Job where 

import qualified Assessment.Config as Config
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
  , encode
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
import Data.Text (pack, strip, unpack)
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified Database.Redis as R
import System.IO (writeFile)
import System.Hworker
import Turtle (ExitCode(ExitSuccess), fromString, mktree, cd, procStrict, procStrictWithErr)


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
  uuid <- nextRandom
  let buildDir = "/tmp/assessment/" ++ (UUID.toString uuid)
  mktree $ fromString buildDir
  cd $ fromString buildDir
  let javaFile = Config.javaClass ++ ".java"
  saveJavaFile buildDir javaFile codeSample
  msg <- compileCode javaFile
  results <- if msg == ""
    then sequence $ map (runProgram Config.javaClass) $ Types.inputs assignment
    else pure $ replicate (length $ Types.inputs assignment) Nothing
  let pairs = zip results $ Types.outputs assignment
  let testResults = map validateTest pairs
  let response = Types.Response {
    Types.job_id = j,
    Types.status = Types.Done,
    Types.compiler_msg = msg,
    Types.test_results = testResults
  }
  current <- Time.getCurrentTime
  let logEntry = LogEntry {
      job_id = j
    , exercise_id = Types.exercise_id submission
    , code = codeSample
    , compiler_msg = msg
    , username = Types.username submission
    , timestamp =(show current)
  }
  putResponse (JobID j) response
  postLog logEntry

compileCode :: String -> IO (String)
compileCode javaFile = do
  (_, _, msg) <- procStrictWithErr (pack Config.compileCmd) [pack javaFile] ""
  return $ unpack msg

saveJavaFile :: String -> String -> String -> IO ()
saveJavaFile buildDir javaFile codeSample = 
  writeFile filename codeSample
  where filename = buildDir ++ "/" ++ javaFile

runProgram :: String -> String -> IO (Maybe String)
runProgram x y = do
  (err, msg) <- procStrict (pack Config.runCmd)  (map pack [x, y])  ""
  if err == ExitSuccess
    then return (Just $ unpack . strip $ msg)
    else return Nothing

validateTest :: (Eq a) => (Maybe a, a) -> Types.TestResult
validateTest (Just x, y) = 
  if x == y
    then Types.Pass
    else Types.Fail
validateTest _ = Types.Fail

putResponse :: JobID -> Types.Response -> IO ()
putResponse (JobID j) response = do
  conn <- R.checkedConnect R.defaultConnectInfo
  _ <- R.runRedis conn $ do
         R.hset "DONE" (LB.toStrict $ DB.encode j) (LB.toStrict $ encode response)
  return ()

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
