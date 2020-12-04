module Polybar (
polybarColour,polybarUnderline,polybarUnderlineWithColor,polybarPP) 
where
import DynamicLog
import XMonad
import qualified XMonad.StackSet as S
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import Data.Maybe (catMaybes)
import Data.List (intersperse)
--Process Colours
type Colour = String
polybarColour :: Char -> Colour -> String -> String
polybarColour area color text = "%{" ++ [area] ++ color ++ "}" ++ text ++ "%{" ++ area:"--}"

polybarPP =  def {
    ppCurrent = polybarColour 'F' "ffffff" .  polybarUnderlineWithColor "FFCFD1" . stripNumbers
    , ppTitle = polybarColour 'F' "--"
    , ppHidden = polybarColour 'F' "E8B8B0" . stripNumbers
    , ppOutput = io . appendFile "/tmp/.xmonad-workspace-log" . flip (++) "\n"
    , ppVisible = polybarColour 'F' "804144"  . wrap "[" "]" . stripNumbers
    , ppHiddenNoWindows = polybarColour 'F' "80696e" . stripNumbers
    , ppSep = polybarColour 'F' "804144" " | "
    , ppOrder = \(x:_:y) -> x:y

}
--Underline Text
polybarUnderline :: String -> String
polybarUnderline text = "%{+u}" ++ text ++ "%{-u}"
polybarUnderlineWithColor :: Colour -> String -> String
polybarUnderlineWithColor color = polybarColour 'u' color . polybarUnderline



stripNumbers :: String -> String
stripNumbers x
    | ':' `elem` x = let (_:_:xs) = x in xs
    | otherwise = x


