{-# LANGUAGE DeriveGeneric         #-}

module Assessment.Log where

import Data.Aeson
import GHC.Generics (Generic)

data LogEntry = LogEntry {
    job_id :: Integer
  , exercise_id :: Integer
  , code :: String
  , compiler_msg :: String
  , username :: String
  , timestamp :: String
} deriving (Show, Generic)

instance ToJSON LogEntry
instance FromJSON LogEntry
