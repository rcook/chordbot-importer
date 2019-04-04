{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import           Data.Foldable
import           Data.List
import           Data.Monoid
import           Text.Printf

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

----------

data Beat = Beat
    { chord :: Chord
    } deriving Show

type Measure = [Beat]

type Line = [Measure]

chunked :: Int -> [a] -> [[a]]
chunked n items =
    let h = take n items
        t = drop n items
    in case t of
        [] -> [h]
        _ -> h : chunked n t

main :: IO ()
main = do
    json <- ByteString.readFile "samples/minor-blues.json"
    case eitherDecode json of
        Left e -> putStrLn $ "Error: " ++ e
        Right song -> putStrLn $ renderSong 4 4 song

expandBeats :: Section -> [Beat]
expandBeats = concatMap (\chord -> take (duration chord) (repeat $ Beat chord)) . chords

renderSong :: Int -> Int -> Song -> String
renderSong beatsPerMeasure measuresPerLine song =
    printf
        "# songName=%s\n\
        \# tempo=%d\n\
        \# beatsPerMeasure=%d\n\
        \# measuresPerLine=%d\n\
        \%s"
        (songName song)
        (tempo song)
        beatsPerMeasure
        measuresPerLine $ concat
        (map (renderSection beatsPerMeasure measuresPerLine) (sections song))

renderSection :: Int -> Int -> Section -> String
renderSection beatsPerMeasure measuresPerLine section =
    let beats = expandBeats section
        measures = chunked beatsPerMeasure beats
        lines = chunked measuresPerLine measures
    in printf
        ": %s\n%s\n"
        (sectionName section)
        (renderLines lines)

renderLines :: [Line] -> String
renderLines = intercalate "\n" . map renderLine

renderLine :: Line -> String
renderLine = intercalate " " . map renderMeasure

renderMeasure :: Measure -> String
renderMeasure m =
    intercalate "_" (map head (group (map (renderChord . chord) m)))

renderChord :: Chord -> String
renderChord chord = case bass chord of
    Nothing -> printf "%s%s" (root chord) (renderChordType $ chordType chord)
    Just n -> printf "%s%s/%s" (root chord) (renderChordType $ chordType chord) n

renderChordType :: String -> String
renderChordType "Maj" = ""
renderChordType "Min" = "m"
renderChordType "7" = "7"
