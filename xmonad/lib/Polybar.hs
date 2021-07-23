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
import DynamicLog
import ExtraState
import System.IO.Unsafe
import XMonad
import XMonad.Hooks.DynamicIcons
import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

--Process Colours
type Colour = String
polybarColour :: Char -> Colour -> String -> String
polybarColour area color text
    | area `notElem` validAreas = error "Invalid Text Area"
    | length color == 7 = "%{" ++ [area] ++ color ++ "}" ++ text ++ "%{" ++ area : "--}"
    | otherwise = error "Invalid colour"
  where
    validAreas = "FBRuoT"

colourCurrent = "#f9f9f9"
colourVisible = "#5AB1BB"
colourHidden = "#ffd1dc"
colourHiddenNoWindows = "#343434"
polybarPP ws =
    def
        { {-ppCurrent = polybarWorkspace (polybarUnderlineWithColor "#FFCFD1" . polybarColour 'F' "#FFDB9E" ) ws False
          , ppHidden = polybarWorkspace (polybarColour 'F' "#E88B84") ws True
          , ppVisible = polybarWorkspace (polybarColour 'F' "#FFC9AB"  . wrap "[" "]") ws True
          , ppHiddenNoWindows = polybarWorkspace (polybarColour 'F' "#5754B3") ws True -}
          ppCurrent = polybarColour 'F' colourCurrent . iconCurrent
        , ppHidden = polybarColour 'F' colourHidden . iconHidden
        , ppVisible = polybarColour 'F' colourVisible . iconCurrent
        , ppHiddenNoWindows = polybarColour 'F' colourHiddenNoWindows . iconHidden
        , ppTitleSanitize = take 70 . ppTitle def
        , ppTitle = polybarColour 'F' "#FFFFFF"
        , ppSep = polybarColour 'F' "#4D3636" " | "
        , ppOutput = io . appendFile "/tmp/.xmonad-workspace-log" . flip (++) "\n" . xmonadPolybarAction 4 "nextws" . xmonadPolybarAction 5 "prevws"
        , ppLayout = xmonadPolybarAction 1 "next-layout" . xmonadPolybarAction 3 "default-layout"
        --  , ppOrder = \(x:xs) -> wrap "%{T2}" "%{T-}" x:xs
        --    , ppOrder = \(x:_:y) -> x:y
        }

switchMoveWindowsPolybar :: PP -> PP
switchMoveWindowsPolybar pp =
    pp
        { ppRename = ppRename pp >=> switchAndMove
        }
  where
    switchAndMove b a
        | b `notElem` defaultIcons = switchAndMoveF (S.tag a) b
        | otherwise = b

switchAndMoveF x = xmonadPolybarAction 1 ("view\\\"" ++ x ++ "\\\"") . xmonadPolybarAction 3 ("moveTo\"" ++ x ++ "\"")

defaultIcons = map show [1 .. 10]
iconCurrent x
    | x `elem` defaultIcons = switchAndMoveF x "\xf111"
    | otherwise = x

iconHidden x
    | x `elem` defaultIcons = switchAndMoveF x "\xf10c"
    | otherwise = x

--iconCurrent = \xf10c""\xf10c"

--polybarWorkspace :: (String -> String)  -> Bool -> String -> String
--polybarWorkspace format bool str = (if bool then moveToWS str . switchWS str  else id) . format $ str
--Underline Text
polybarUnderline :: String -> String
polybarUnderline text = "%{+u}" ++ text ++ "%{-u}"

--Underline Polybar Text with Colour
polybarUnderlineWithColor :: Colour -> String -> String
polybarUnderlineWithColor color = polybarColour 'u' color . polybarUnderline
