{-# LANGUAGE OverloadedStrings, LambdaCase #-}
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.IORef as Ref
import Control.Monad
import DBus
import DBus.Client
import Control.Concurrent
import System.Environment
import System.IO

allActions :: String -> String
allActions = action 4 "playerctl next" . action 6 "playerctl previous" . action 1 "playerctl play-pause"

action :: Int -> String -> String -> String
action a str text = "%{A"++show a++":"++str++":}" <> text <> "%{A}"

build :: Client -> Ref.IORef String -> Bool -> IO ()
build client lastMessage bool = do
    status <- getPlayStatus client
    case status of
        Just status' -> do
            track <- getTrack client
            case track of
                Just track' -> putMessage (allActions $ show status'  <> show track') lastMessage
                Nothing -> putMessage "\61884 " lastMessage
        Nothing -> putMessage "\61884 " lastMessage

putMessage :: String -> Ref.IORef String -> IO ()
putMessage str last = do
        last' <- Ref.readIORef last
        if last' == str 
            then pure ()
            else do
                Ref.writeIORef last str
                putStrLn str

main = do
    lastMessage <- Ref.newIORef []
    client <- connectSession
    args <- getArgs
    hSetBuffering stdout LineBuffering 
    case args of
        ["-v"] ->forever (build client lastMessage True>> threadDelay 1000000)
        _ -> forever (build client lastMessage False >> threadDelay 1000000)

getTrack :: Client -> IO (Maybe Track)
getTrack  client = do
    metadata <- getProperty  client (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "Metadata")
        { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
        }
    pure $ case metadata of
        Left a -> Nothing
        Right x -> parseTrack . M.fromList  =<<  mapM (\(a,b) -> (,) <$> fromVariant a <*> fromVariant b )  . dictionaryItems =<< fromVariant x


getPlayStatus :: Client -> IO (Maybe PlayStatus)
getPlayStatus client = do
    status <- getProperty  client (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "PlaybackStatus")
        { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
        }
    pure $ case status of
        Left a -> Nothing
        Right x -> fromVariant x >>= parsePlayStatus





data Track = Track { artist :: [String], title :: String, album :: String, albumArtist :: [String] }
data PlayStatus = Playing | Paused 

instance Show Track where
    show Track { title = title', artist = artist'} = title' <> " - " <> head artist'
instance Show PlayStatus where
    show Playing = "\61515 "
    show Paused = "\61516 "

parsePlayStatus :: String -> Maybe PlayStatus
parsePlayStatus = \case {
            "Playing" -> Just Playing;
            "Paused" -> Just Paused;
            _ -> Nothing;
        }

parseTrack :: M.Map String Variant -> Maybe Track
parseTrack map = 
        Track 
        <$> getMetaData "artist" map
        <*> getMetaData "title" map
        <*> getMetaData "album" map
        <*> getMetaData "albumArtist" map

getMetaData :: IsVariant a =>  String -> M.Map String Variant -> Maybe a
getMetaData str = M.lookup ("xesam:" <> str) >=> fromVariant
