{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import           Data.Monoid

data Song = Song
    { name :: String
    , fileType :: String
    , tempo :: Int
    , editMode :: Int
    } deriving Show

instance FromJSON Song where
    parseJSON = withObject "song" $ \o -> Song
        <$> o .: "songName"
        <*> o .: "fileType"
        <*> o .: "tempo"
        <*> o .: "editMode"

main :: IO ()
main = do
    json <- ByteString.readFile "samples/minor-blues.json"
    print json
    let result = eitherDecode json :: Either String Song
    print result
