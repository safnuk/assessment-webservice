{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Monad           (forever)
import           System.Hworker
import           Assessment.Job
import           Turtle                  (cd)


main :: IO ()
main = do hw <- hworker
          _ <- forkIO (worker hw)
          forever (threadDelay 1000000)
