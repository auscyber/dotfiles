{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Media (playPause,previous,next) where
import Data.List (sort)
import qualified Data.Map as M
import Control.Monad
import DBus
import Data.Maybe
import DBus.Client
import Control.Concurrent

playerCommand :: MemberName -> Client  -> IO ()
playerCommand string client = callNoReply client (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" string)
        { methodCallDestination = Just "org.mpris.MediaPlayer2.playerctld"
        } 

next :: Client -> IO ()
next = playerCommand "Next"
previous :: Client -> IO ()
previous = playerCommand "Previous" 
playPause :: Client -> IO ()
playPause= playerCommand "PlayPause"



