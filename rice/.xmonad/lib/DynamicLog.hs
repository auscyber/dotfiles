{-# LANGUAGE FlexibleContexts, PatternGuards #-}

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
    dynamicLogIcons,
    polyBarAction,xmonadPolybarAction,

    -- $todo
  ) where

-- Useful imports
import XMonad.Hooks.DynamicLog
import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative (liftA2)
import Control.Monad (msum)
import Data.Char ( isSpace, ord )
import Data.List (intersperse, stripPrefix, isPrefixOf, sortBy)
import Data.Maybe ( isJust, catMaybes, mapMaybe, fromMaybe )
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
type IconSet = Query [WorkSpaceIconSet]
dynamicLogIcons :: PP -> IconSet -> X String
dynamicLogIcons pp set = do
 
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp
    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset
    -- workspace list
    ws <- pprIconWindowSet sort' urgents pp winset set
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
sepBy sep = concat . intersperse sep . filter (not . null)

-- | Format the workspace information, given a workspace sorting function,
--   a list of urgent windows, a pretty-printer format, and the current
--   WindowSet but taking into account ws and an IconSet.

checkWorkSpaceIcons :: M.Map String WorkSpaceIconSet -> WorkspaceId  -> Maybe WorkSpaceIconSet -> WorkSpaceIconSet
checkWorkSpaceIcons icons id icon' = 
        case M.lookup id icons of
            Just id_icon@NameSet { showOverlayIcons = True } -> fromMaybe id_icon icon'
            Just id_icon -> id_icon
            _ -> basicIconSet id
basicIconSet :: String -> ES.WorkSpaceIconSet
basicIconSet x = ES.NameSet { current = x,visible = x,hidden =x, showOverlayIcons = True }

pprIconWindowSet :: WorkspaceSort -> [Window] -> PP -> WindowSet -> IconSet -> X String 
pprIconWindowSet sort' urgents pp s iconset =
     sepBy (ppWsSep pp) <$> (mapM fmt . sort' $
            map S.workspace (S.current s : S.visible s) ++ S.hidden s)
   where this     = S.currentTag s
         visibles = map (S.tag . S.workspace) (S.visible s)
         fmt :: S.Workspace WorkspaceId l Window -> X String
         fmt w = do
            let windows'' stack' = [S.focus stack'] ++ S.up stack' ++ S.down stack'
            a <- sequence $ foldMap (runQuery iconset) . windows'' <$> S.stack w
            let a' = (\x -> if null x then Nothing else Just $ head x) =<< a
            ws' <- XS.gets (ES.workspaceNames)
            pure $ moveToWS (S.tag w) . switchWS (S.tag w) $ printer pp (checkWorkSpaceIcons ws' (S.tag w) a')
            where printer pp (NameSet { current = current',visible= visible',hidden = hidden'}) | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = ppUrgent pp current'
                        | S.tag w == this                                               =  ppCurrent pp current'
                        | S.tag w `elem` visibles && isJust (S.stack w)                 = ppVisible pp visible'
                        | S.tag w `elem` visibles                                       = liftA2 fromMaybe ppVisible ppVisibleNoWindows pp visible'
                        | isJust (S.stack w)                                            =  ppHidden pp hidden'
                        | otherwise                                                     = ppHiddenNoWindows pp hidden'

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


