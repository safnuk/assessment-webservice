{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Monad           (forever)
import           System.Hworker
import           Assessment.Job
import           Assessment.Types


main :: IO ()
main = do hw <- hworker
          _ <- forkIO (worker hw)
          -- forkIO (forever $ queue hw Print >> threadDelay 1000000)
          forever (threadDelay 1000000)
