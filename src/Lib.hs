{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Monoid (mconcat, (<>))
import qualified Data.Text.Lazy as Text (Text, pack, unpack)
import           Database.SQLite.Simple
import           GHC.Generics
import           Network.Wai (strictRequestBody)
import           Web.Scotty

data Contact = Contact {
    firstName :: Text.Text
  , lastName  :: Text.Text
  , email     :: Text.Text
} deriving (Generic, Show)

instance FromJSON Contact
instance ToJSON Contact

server :: IO ()
server = do
  db <- openDB
  scotty 3000 $ do
    post "/:word" $ do
      req  <- request
      body <- liftIO $ strictRequestBody req
      case eitherDecode body of
        Right contact -> do
          liftIO $ persist db contact
          html $ Text.pack $ show contact
        Left err -> html $ Text.pack err

openDB :: IO Connection
openDB = do
  conn <- open "rolodex.db"
  execute_ conn $ "CREATE TABLE IF NOT EXISTS contacts ("
             <>   "id INTEGER PRIMARY KEY"
             <> ", firstName TEXT"
             <> ", lastName TEXT"
             <> ", email TEXT"
             <> ")"
  return conn

persist :: Connection -> Contact -> IO ()
persist conn (Contact firstName lastName email) = do
  execute conn "INSERT INTO contacts (firstName, lastName, email) VALUES (?,?,?)"
    (Text.unpack firstName, Text.unpack lastName, Text.unpack email)
  return ()
