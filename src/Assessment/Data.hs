{-# LANGUAGE OverloadedStrings #-}

module Assessment.Data where

import Assessment.Types
import Control.Monad.IO.Class (liftIO)
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
  noteRequestMade submission 60
  conn <- R.checkedConnect R.defaultConnectInfo
  _ <- R.runRedis conn $ do
       R.set (LB.toStrict $ DB.encode j) (LB.toStrict $ encode qe)
  return ()

getAssignment :: Integer -> IO (Assignment)
getAssignment _ = pure testAssignment

getStatusResponse :: JobID -> IO (Response)
getStatusResponse _ = pure testResponse

tooManyRequests :: Submission -> Integer -> IO (Bool)
tooManyRequests s maxAllowed = do
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
