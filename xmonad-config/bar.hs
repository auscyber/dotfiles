{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import DBus
import DBus.Client
import qualified Data.IORef as Ref
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe
import Polybar
import System.Environment
import System.IO
import System.Process

allActions :: String -> String
allActions = action 4 "playerctl next" . action 6 "playerctl previous" . action 1 "playerctl play-pause"

action :: Int -> String -> String -> String
action a str text = "%{A" ++ show a ++ ":" ++ str ++ ":}" <> text <> "%{A}"

bar :: Bool -> Client -> Ref.IORef String -> IO ()
bar bool client lastMessage = do
    status <- getPlayStatus client
    track <- getTrack client
    ifNothing lastMessage "\6884 " (liftM2 (\y x -> show x <> show y) track status)

ifNothing :: Ref.IORef String -> String -> Maybe String -> IO ()
ifNothing ref def may = putMessage (fromMaybe def may) ref

pausePlay :: Client -> Ref.IORef String -> IO ()
pausePlay client lastPlay = do
    status <- getPlayStatus client
    ifNothing lastPlay "" (show <$> status)

putMessage :: String -> Ref.IORef String -> IO ()
putMessage str last = do
    last' <- Ref.readIORef last
    if last' == str
        then pure ()
        else do
            Ref.writeIORef last str
            putStrLn str

statefulDbusAction :: (Monoid m) => (Ref.IORef m -> IO ()) -> IO ()
statefulDbusAction f = Ref.newIORef mempty >>= forever1Delay . f

main = do
    client <- connectSession
    args <- getArgs
    hSetBuffering stdout LineBuffering
    case args of
        ["play-pause"] -> statefulDbusAction $ pausePlay client -- client lastMessage True
        ["play-pause", "--instance"] -> Ref.newIORef [] >>= pausePlay client -- client lastMessage True
        ["--help"] -> putStrLn helpMessage
        ["polybar"] -> statefulDbusAction $ bar False client
        _ -> statefulDbusAction $ bar False client

helpMessage =
    unlines
        [ "a simple spotify statusbar for polybar"
        ]
forever1Delay = foreverWithDelay 1000000

foreverWithDelay :: Int -> IO a -> IO a
foreverWithDelay num mon = forever (mon >> threadDelay num)

getTrack :: Client -> IO (Maybe Track)
getTrack client = do
    metadata <-
        getProperty
            client
            (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "Metadata")
                { methodCallDestination = Just "org.mpris.MediaPlayer2.playerctld"
                }
    pure $ case metadata of
        Left a -> Nothing
        Right x -> parseTrack . M.fromList =<< mapM (\(a, b) -> (,) <$> fromVariant a <*> fromVariant b) . dictionaryItems =<< fromVariant x

getPlayStatus :: Client -> IO (Maybe PlayStatus)
getPlayStatus client = do
    status <-
        getProperty
            client
            (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "PlaybackStatus")
                { methodCallDestination = Just "org.mpris.MediaPlayer2.playerctld"
                }
    pure $ case status of
        Left a -> Nothing
        Right x -> fromVariant x >>= parsePlayStatus

data Track = Track
    { artist :: [String]
    , title :: String
    , album :: String
    , albumArtist :: [String]
    , albumUrl :: String
    }
data PlayStatus = Playing | Paused

instance Show Track where
    show Track{title = title', artist = artist'} = title' <> " - " <> head artist'
instance Show PlayStatus where
    show Playing = "\61515 "
    show Paused = "\61516 "

parsePlayStatus :: String -> Maybe PlayStatus
parsePlayStatus = \case
    "Playing" -> Just Playing
    "Paused" -> Just Paused
    _ -> Nothing

parseTrack :: M.Map String Variant -> Maybe Track
parseTrack =
    liftM5 Track
        <$> getMetaData "xesam:artist"
        <*> getMetaData "xesam:title"
        <*> getMetaData "xesam:album"
        <*> getMetaData "xesam:albumArtist"
        <*> ((parseUrl <$>) <$> getMetaData "mpris:artUrl")

displayImage :: String -> IO ()
displayImage image = do
    callCommand $ "curl \"" <> image <> "\" > /tmp/img.png"
    callCommand "feh /tmp/img.png"
    pure ()

parseUrl :: String -> String
parseUrl = ("https://i.scdn.co" <>) . drop (length ("https://open.spotify.com" :: String))

getMetaData :: (IsVariant a) => String -> M.Map String Variant -> Maybe a
getMetaData str = M.lookup str >=> fromVariant
