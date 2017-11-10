{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, (.=), object)
import Data.Monoid ((<>))
import GHC.Generics
import Network.HTTP.Types.Status (badRequest400)
import Web.Scotty

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

data Submission = Submission { username :: String, exercise_id :: Int, code :: String }
instance ToJSON Submission
instance FromJSON Submission

data Response = Response { job_id :: Int, status :: Status, compiler_msg :: String, test_results :: [Results] }
instance ToJSON Response
instance FromJSON Response

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

routes :: ScottyM ()
routes = do
  get "/hello" hello
  get "/hello/:name" helloPersonalized
  get "/users" $ do
    json allUsers
  get "/users/:id" $ do
    id <- param "id"
    json (filter (matchesId id) allUsers)
  post "/users" usersPost
  post "/users" usersPostFailure

hello :: ActionM ()
hello = do
  text "hello world!"

helloPersonalized :: ActionM ()
helloPersonalized = do
  name <- param "name"
  text ("hello " <> name <> "!")

usersPost :: ActionM ()
usersPost = do
  user <- (jsonData :: ActionM User) `rescue` (const next)
  json user 

usersPostFailure :: ActionM ()
usersPostFailure = do
  json $ object [ "error" .= ("Invalid request" :: String) ]
  status badRequest400

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

main :: IO ()
main = do
    putStrLn "Starting server..."
    scotty 3000 $ do
        routes
