{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Assessment.Types where 

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Data.Aeson              (FromJSON, ToJSON)
import           GHC.Generics            (Generic)
import           System.Hworker


data PrintJob = Print deriving (Generic, Show)
data State = State (MVar Int)
instance ToJSON PrintJob
instance FromJSON PrintJob

instance Job State PrintJob where
  job (State mvar) Print =
    do v <- takeMVar mvar
       putMVar mvar (v + 1)
       putStrLn $ "A(" ++ show v ++ ")"
       return Success

hworker :: IO (Hworker State PrintJob)
hworker = do
  mvar <- newMVar 3
  create "printer" (State mvar)

