{-# LANGUAGE OverloadedStrings #-}

import DBus
import DBus.Client
import Data.List (sort)

main = do
    client <- connectSession

    -- Request a list of connected clients from the bus
    reply <-
        call_
            client
            (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
                { methodCallDestination = Just "org.freedesktop.DBus"
                }

    -- org.freedesktop.DBus.ListNames() returns a single value, which is
    -- a list of names (here represented as [String])
    let Just names = fromVariant (methodReturnBody reply !! 0)

    -- Print each name on a line, sorted so reserved names are below
    -- temporary names.
    mapM_ putStrLn (sort names)
