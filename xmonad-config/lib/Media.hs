{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Media (playPause, previous, next) where

import Control.Concurrent
import Control.Monad
import DBus
import DBus.Client
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe

playerCommand :: MemberName -> Client -> IO ()
playerCommand string client =
    callNoReply
        client
        (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" string)
            { methodCallDestination = Just "org.mpris.MediaPlayer2.playerctld"
            }

next :: Client -> IO ()
next = playerCommand "Next"
previous :: Client -> IO ()
previous = playerCommand "Previous"
playPause :: Client -> IO ()
playPause = playerCommand "PlayPause"
