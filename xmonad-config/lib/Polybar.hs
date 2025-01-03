{-# LANGUAGE TupleSections #-}

module Polybar (
    switchMoveWindowsPolybar,
    polybarColour,
    polybarUnderline,
    polybarUnderlineWithColor,
    polybarPP,
) where

import Control.Monad ((<=<), (>=>))
import Data.List
import qualified Data.Map as M
import Data.Maybe
import ExtraState
import qualified SysDependent
import System.IO.Unsafe
import XMonad
import XMonad.Hooks.DynamicIcons
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleConf as XC
import qualified XMonad.Util.ExtensibleState as XS

--Process Colours
type Colour = String
polyBarAction :: Int -> String -> String -> String
polyBarAction button command
    | button > 8 || button < 1 = id
    | otherwise = wrap ("%{A" ++ show button ++ ":" ++ command ++ ":}") "%{A}"

switchWS :: String -> String -> String
switchWS id = xmonadPolybarAction 1 ("view" ++ id)

moveToWS :: String -> String -> String
moveToWS id = xmonadPolybarAction 3 ("moveTo" ++ id)

xmonadPolybarAction :: Int -> String -> String -> String
xmonadPolybarAction but x = polyBarAction but ("xmonadctl " ++ x)

polybarColour :: Char -> Colour -> String -> String
polybarColour area color text
    | area `notElem` validAreas = error "Invalid Text Area"
    | otherwise = "%{" ++ [area] ++ color ++ "}" ++ text ++ "%{" ++ area : "-}"
  where
    validAreas = "FBRuoT"

colourCurrent = "#f9f9f9"

--colourVisible = "#5AB1BB"
colourVisible = "#ffd1dc"
colourHidden = "#ffeFdc"
colourHiddenNoWindows = "#A4A4A4"
polybarPP :: [String] -> X PP
polybarPP defaultIcons = do
    titleLength <- fmap SysDependent.titleLength <$> XC.ask
    let iconCurrent x
            | x `elem` defaultIcons = switchAndMoveF x (pad "\xf111")
            | otherwise = x
    let iconHidden x
            | x `elem` defaultIcons = switchAndMoveF x (pad "\xf10c")
            | otherwise = x
    let highlightEnds = polybarColour 'T' "2" . polybarColour 'F' "#3f3f3f" . polybarColour 'B' "#FF000000"
    pure $
        def
            { {-ppCurrent = polybarWorkspace (polybarUnderlineWithColor "#FFCFD1" . polybarColour 'F' "#FFDB9E" ) ws False
              , ppHidden = polybarWorkspace (polybarColour 'F' "#E88B84") ws True
              , ppVisible = polybarWorkspace (polybarColour 'F' "#FFC9AB"  . wrap "[" "]") ws True
              , ppHiddenNoWindows = polybarWorkspace (polybarColour 'F' "#5754B3") ws True -}
              ppCurrent = polybarUnderlineWithColor colourCurrent . iconCurrent
            , ppHidden = polybarUnderlineWithColor colourHidden . iconHidden
            , ppVisible = polybarUnderlineWithColor colourVisible . iconCurrent
            , ppHiddenNoWindows = polybarColour 'F' colourHiddenNoWindows . iconHidden
            , ppTitleSanitize = shorten 70 . ppTitle def
            , ppTitle = polybarColour 'F' "#FFFFFF" . take (fromMaybe 100 titleLength)
            , ppWsSep = ""
            , ppSep = polybarColour 'F' "#6D5656" " | "
            , ppOutput = io . appendFile "/tmp/.xmonad-workspace-log" . flip (++) "\n" . xmonadPolybarAction 4 "nextws" . xmonadPolybarAction 5 "prevws"
            , ppLayout = xmonadPolybarAction 1 "next-layout" . xmonadPolybarAction 3 "default-layout"
            , ppOrder = \(x : y : z : xs) ->
                (highlightEnds "\57526" ++ "%{B#FF3f3f3f}" ++ x) : y : (z ++ highlightEnds "\57524") : xs
                --"\57524"
                --
                --  , ppOrder = \(x:xs) -> wrap "%{T2}" "%{T-}" x:xs
                --    , ppOrder = \(x:_:y) -> x:y
            }

switchMoveWindowsPolybar :: [WorkspaceId] -> PP -> PP
switchMoveWindowsPolybar defaultIcons pp =
    pp
        { ppRename = ppRename pp >=> switchAndMove
        }
  where
    switchAndMove b a
        | b `notElem` defaultIcons = switchAndMoveF (S.tag a) b
        | otherwise = b

switchAndMoveF x = xmonadPolybarAction 1 ("view\\\"" ++ x ++ "\\\"") . xmonadPolybarAction 3 ("moveTo\"" ++ x ++ "\"")

--iconCurrent = \xf10c""\xf10c"

--polybarWorkspace :: (String -> String)  -> Bool -> String -> String
--polybarWorkspace format bool str = (if bool then moveToWS str . switchWS str  else id) . format $ str
--Underline Text
polybarUnderline :: String -> String
polybarUnderline text = "%{+u}" ++ text ++ "%{-u}"

--Underline Polybar Text with Colour
polybarUnderlineWithColor :: Colour -> String -> String
polybarUnderlineWithColor color = polybarColour 'u' color . polybarUnderline
