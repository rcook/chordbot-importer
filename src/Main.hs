{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import           Data.Monoid

data Song = Song
    { songName :: String -- TODO: Rename to "name"
    , fileType :: String
    , tempo :: Int
    , sections :: [Section]
    , editMode :: Int
    } deriving Show

instance FromJSON Song where
    parseJSON = withObject "song" $ \o -> Song
        <$> o .: "songName"
        <*> o .: "fileType"
        <*> o .: "tempo"
        <*> o .: "sections"
        <*> o .: "editMode"

data Section = Section
    { style :: Style
    , chords :: [Chord]
    , sectionName :: String -- TODO: Rename to "name"
    } deriving Show

instance FromJSON Section where
    parseJSON = withObject "section" $ \o -> Section
        <$> o .: "style"
        <*> o .: "chords"
        <*> o .: "name"

data Style = Style
    { tracks :: [Track]
    , preset :: Int
    , reverb :: Int
    , chorus :: Int
    } deriving Show

instance FromJSON Style where
    parseJSON = withObject "style" $ \o -> Style
        <$> o .: "tracks"
        <*> o .: "preset"
        <*> o .: "reverb"
        <*> o .: "chorus"

data Track = Track
    { volume :: Int
    , trackId :: Int -- TODO: Rename to "id"
    } deriving Show

instance FromJSON Track where
    parseJSON = withObject "track" $ \o -> Track
        <$> o .: "volume"
        <*> o .: "id"

data Chord = Chord
    { duration :: Int
    , chordType :: String -- TODO: Rename to "type"
    , root :: String
    , bass :: Maybe String
    } deriving Show

instance FromJSON Chord where
    parseJSON = withObject "chord" $ \o -> Chord
        <$> o .: "duration"
        <*> o .: "type"
        <*> o .: "root"
        <*> o .:? "bass"

main :: IO ()
main = do
    json <- ByteString.readFile "samples/minor-blues.json"
    print json
    let result = eitherDecode json :: Either String Song
    print result
