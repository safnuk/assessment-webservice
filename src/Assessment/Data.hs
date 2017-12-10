{-# LANGUAGE OverloadedStrings #-}

module Assessment.Data where

import Assessment.Config as Config
import Assessment.External
import Assessment.Job
import Assessment.Types as Types
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Aeson (decode, encode)
import qualified Data.Binary as DB 
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LB
import Data.List (findIndex)
import qualified Database.Redis as R
import System.Hworker

enqueue :: JobID -> MaybeT IO ()
enqueue j = do
  hw <- lift hworker
  _ <- lift $ queue hw j
  return ()

nextJobID :: MaybeT IO (JobID)
nextJobID = do
  conn <- lift $ R.checkedConnect R.defaultConnectInfo
  r <- liftIO $ R.runRedis conn $ do
    R.incr "job_id"
  case r of
    Left _ -> undefined
    Right j -> do
      return $ JobID j

putSubmission :: JobID -> Submission -> MaybeT IO () 
putSubmission (JobID j) submission = do
  assignment <- getAssignmentTests $ exercise_id submission
  let qe = QueueEntry submission assignment
  lift $ noteRequestMade submission Config.requestCounterTimeout
  conn <- lift $ R.checkedConnect R.defaultConnectInfo
  _ <- lift $ R.runRedis conn $ do
       R.set (LB.toStrict $ DB.encode j) (LB.toStrict $ encode qe)
  return ()

getStatusResponse :: JobID -> MaybeT IO (Response)
getStatusResponse j = getProcessedResult j <|> buildProcessingResponse j

getProcessedResult :: JobID -> MaybeT IO Response
getProcessedResult j = do
  conn <- lift $ R.checkedConnect R.defaultConnectInfo
  result <- liftIO $ R.runRedis conn $ 
    R.hget "DONE" (jobToByteString j)
  case result of
    Left _ -> mzero
    Right x -> MaybeT $ ((pure $ (LB.fromStrict <$> x) >>=  decode) :: IO (Maybe Response))
  
buildProcessingResponse :: JobID -> MaybeT IO Response
buildProcessingResponse j = do
  exists <- liftIO $ queueEntryExists j
  if exists
     then return Response {
            job_id = jobToInteger j
            , status = Processing
            , compiler_msg = ""
            , test_results = []}
     else mzero

queueEntryExists :: JobID -> IO (Bool)
queueEntryExists j = do
  qe <- runMaybeT $ getQueueEntry j
  case qe of
    Nothing -> return False
    _ -> return True

tooManyRequests :: Submission -> IO (Bool)
tooManyRequests = moreThanRequests Config.maxRequestFrequency

moreThanRequests :: Integer -> Submission -> IO (Bool)
moreThanRequests maxAllowed s = do
  index <- firstAvailableRequestIndex (username s)
  case index of
    Nothing -> return True
    Just i -> return $ i >= maxAllowed

noteRequestMade :: Submission -> Integer -> IO ()
noteRequestMade s expiration = do
  let user = username s
  index <- firstAvailableRequestIndex (user)
  conn <- R.checkedConnect R.defaultConnectInfo
  let key = BS.pack $ case index of
            Nothing -> undefined
            Just i -> user ++ (show i)
  _ <- R.runRedis conn $ do
    _ <- R.set key "set"
    R.expire key expiration
  return ()

keyExists :: String -> IO (Bool)
keyExists s = do
  conn <- R.checkedConnect R.defaultConnectInfo
  R.runRedis conn $ do
    r <- R.exists $ BS.pack s
    case r of
      Left _ -> return False
      Right b -> return b

firstAvailableRequestIndex :: String -> IO (Maybe Integer)
firstAvailableRequestIndex user = do
  let userKeys = [(user) ++ show x | x <- [(0 :: Integer)..5]]
  bools <- sequence $ map keyExists userKeys
  return $ firstFalse bools

firstFalse :: [Bool] -> (Maybe Integer)
firstFalse = (fmap toInteger) . (findIndex not) 
