{-# LANGUAGE OverloadedStrings #-}

module Assessment.Data where

import Assessment.Config as Config
import Assessment.Types as Types
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Aeson (encode)
import qualified Data.Binary as DB 
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LB
import Data.List (findIndex)
import qualified Database.Redis as R
import System.Hworker

enqueue :: JobID -> IO ()
enqueue j = do
  hw <- hworker
  _ <- queue hw j
  return ()

nextJobID :: IO (JobID)
nextJobID = do
  conn <- R.checkedConnect R.defaultConnectInfo
  r <- liftIO $ R.runRedis conn $ do
    R.incr "job_id"
  case r of
    Left _ -> undefined
    Right j -> do
      return $ JobID j

putSubmission :: JobID -> Submission -> IO () 
putSubmission (JobID j) submission = do
  assignment <- getAssignment $ exercise_id submission
  let qe = QueueEntry submission assignment
  noteRequestMade submission Config.requestCounterTimeout
  conn <- R.checkedConnect R.defaultConnectInfo
  _ <- R.runRedis conn $ do
       R.set (LB.toStrict $ DB.encode j) (LB.toStrict $ encode qe)
  return ()

getAssignment :: Integer -> IO (Assignment)
getAssignment _ = pure testAssignment

getStatusResponse :: JobID -> MaybeT IO (Response)
getStatusResponse j = getResult j <|> buildProcessingResponse j
  -- resp <- getResult j
  -- case resp of
  --   Just sr -> return sr
  --   Nothing -> do
  --     qe <- getQueueEntry j
  --       case qe of
  --         Nothing -> undefined
  --         Just q -> 
  --           return Response {
  --                     job_id = toInteger j
  --                   , status = Processing
  --                   , compiler_msg = ""
  --                   , test_results = []}

getResult :: JobID -> MaybeT IO Response
getResult _ = mzero

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
queueEntryExists _ = pure True

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
