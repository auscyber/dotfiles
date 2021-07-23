{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
 Module      :  XMonad.Hooks.DynamicLog
 Copyright   :  (c) Don Stewart <dons@cse.unsw.edu.au>
 License     :  BSD3-style (see LICENSE)

 Maintainer  :  Don Stewart <dons@cse.unsw.edu.au>
 Stability   :  unstable
 Portability :  unportable

 xmonad calls the logHook with every internal state update, which is
 useful for (among other things) outputting status information to an
 external status bar program such as xmobar or dzen.  DynamicLog
 provides several drop-in logHooks for this purpose, as well as
 flexible tools for specifying your own formatting.
-}
module DynamicLog (
    DynamicLog.dynamicLogString,
    polyBarAction,
    xmonadPolybarAction,
    -- $todo
) where

-- Useful imports

import Control.Applicative (liftA2)
import Control.Monad (msum)
import Data.Char (isSpace, ord)
import Data.List (intercalate, isPrefixOf, sortBy, stripPrefix)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Ord (comparing)
import System.IO as IO
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as S

import ExtraState as ES
import Foreign.C (CChar)
import XMonad
import XMonad.Hooks.DynamicIcons
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

polyBarAction :: Int -> String -> String -> String
polyBarAction button command
    | button > 8 || button < 1 = id
    | otherwise = wrap ("%{A" ++ show button ++ ":" ++ command ++ ":}") "%{A}"

switchWS :: String -> String -> String
switchWS id = xmonadPolybarAction 1 ("view" ++ id)

moveToWS :: String -> String -> String
moveToWS id = xmonadPolybarAction 3 ("moveTo" ++ id)

xmonadPolybarAction :: Int -> String -> String -> String
xmonadPolybarAction but x = polyBarAction but ("~/.xmonad/xmonadctl " ++ x)
sepBy ::
    -- | separator
    String ->
    -- | fields to output
    [String] ->
    String
sepBy sep = intercalate sep . filter (not . null)

{- | Format the workspace information, given a workspace sorting function,
   a list of urgent windows, a pretty-printer format, and the current
   WindowSet but taking into account ws and an IconSet.
-}
dynamicLogString :: PP -> X String
dynamicLogString pp = do
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp

    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset

    -- workspace list
    let ws = pprWindowSet sort' urgents pp winset

    -- window title
    wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

    return $
        sepBy (ppSep pp) . ppOrder pp $
            [ ws
            , ppLayout pp ld
            , ppTitle pp $ ppTitleSanitize pp wt
            ]
                ++ catMaybes extras
