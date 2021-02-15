{-# LANGUAGE TupleSections,FlexibleContexts, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicLog
-- Copyright   :  (c) Don Stewart <dons@cse.unsw.edu.au>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Don Stewart <dons@cse.unsw.edu.au>
-- Stability   :  unstable
-- Portability :  unportable
--
-- xmonad calls the logHook with every internal state update, which is
-- useful for (among other things) outputting status information to an
-- external status bar program such as xmobar or dzen.  DynamicLog
-- provides several drop-in logHooks for this purpose, as well as
-- flexible tools for specifying your own formatting.
--
-----------------------------------------------------------------------------

module DynamicLog (
    DynamicLog.dynamicLogString,
    polyBarAction,xmonadPolybarAction,
    workspaceFilter
    -- $todo
  ) where

-- Useful imports
import XMonad.Hooks.DynamicLog
import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative (liftA2)
import Control.Monad (msum)
import Data.Char ( isSpace, ord )
import Data.List (intercalate, stripPrefix, isPrefixOf, sortBy)
import Data.Maybe ( isJust, catMaybes, mapMaybe, maybeToList, fromMaybe )
import Data.Ord ( comparing )
import qualified Data.Map as M
import qualified XMonad.StackSet as S
import System.IO as IO

import Foreign.C (CChar)
import XMonad
import ExtraState as ES
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicIcons
polyBarAction :: Int -> String -> String-> String
polyBarAction button command
    | button > 8 || button <1 = id
    | otherwise = wrap ("%{A" ++ show button ++ ":" ++ command ++ ":}") "%{A}"



switchWS :: String -> String -> String 
switchWS id = xmonadPolybarAction 1 ("view"++id) 

moveToWS :: String -> String -> String
moveToWS id = xmonadPolybarAction 3 ("moveTo"++id)

xmonadPolybarAction :: Int -> String -> String -> String
xmonadPolybarAction but x = polyBarAction but ("~/.xmonad/xmonadctl " ++x)
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = intercalate sep . filter (not . null)

-- | Format the workspace information, given a workspace sorting function,
--   a list of urgent windows, a pretty-printer format, and the current
--   WindowSet but taking into account ws and an IconSet.

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

    return $  sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppTitle  pp $ ppTitleSanitize pp wt
                        ]
                        ++ catMaybes extras
baseIconSet :: String -> Icon
baseIconSet x =
    Icon { iconCurrent = x
         , iconVisible = x
         , iconHidden = x
         , iconHiddenNoWindows = x
         }

workspaceFilter :: IconConfig -> X ([WindowSpace] -> [WindowSpace])
workspaceFilter iconConfig = do
    ws <- gets (S.workspaces . windowset)
    icons <- M.fromList . (maybeToList =<<) <$> mapM (getIcons (iconConfigIcons iconConfig)) ws
    pure $ map (\x -> x { S.tag =  workspace x icons})
  where
    workspace x icons 
        | isJust $ S.stack x =  case iconLookup (S.tag x) icons of
            [] -> S.tag x
            x -> concatIcons iconHidden x
        | otherwise = S.tag x
    iconLookup x icons = M.findWithDefault [baseIconSet x] x icons
    concatIcons f y
        | length y > 1 = iconConfigStack iconConfig $ map f y
        | otherwise = concatMap f y

getIcons :: IconSet -> WindowSpace -> X (Maybe (WorkspaceId, [Icon]))
getIcons is w = do
    validIcons <- sequence $ foldMap (runQuery is) . S.integrate <$> S.stack w
    pure $ (S.tag w,) <$> (validIcons >>= \x -> if null x then Nothing else Just x)


