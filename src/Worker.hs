-- Executable for a worker process
-- Implementation for what the worker does is in Assessment/Job.hs
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Monad           (forever)
import           System.Hworker
import           Assessment.Job

main :: IO ()
main = do hw <- hworker
          _ <- forkIO (worker hw)
          forever (threadDelay 1000000)
