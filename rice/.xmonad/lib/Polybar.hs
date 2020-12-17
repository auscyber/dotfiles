module Polybar (
polybarColour,polybarUnderline,polybarUnderlineWithColor,polyBarAction,polybarPP) 
where
import DynamicLog
import XMonad
--Process Colours
type Colour = String
polybarColour :: Char -> Colour -> String -> String
polybarColour area (_:color) text = "%{" ++ [area] ++ color ++ "}" ++ text ++ "%{" ++ area:"--}"

--FFC9AB
--E88B84
--804144
polybarPP =  def {
    ppCurrent = polybarColour 'F' "#FFDB9E" .  polybarUnderlineWithColor "#FFCFD1" . stripNumbers
    , ppTitle = polybarColour 'F' "#--" . take 90
    , ppHidden = polybarColour 'F' "#E88B84" . stripNumbers
    , ppVisible = polybarColour 'F' "#FFC9AB"  . wrap "[" "]" . stripNumbers
    , ppHiddenNoWindows = polybarColour 'F' "#804144" . stripNumbers
    , ppSep = polybarColour 'F' "#5754B3" " | "
    , ppOutput = io . appendFile "/tmp/.xmonad-workspace-log" . flip (++) "\n"
--    , ppOrder = \(x:_:y) -> x:y

}

--Underline Text
polybarUnderline :: String -> String
polybarUnderline text = "%{+u}" ++ text ++ "%{-u}"
--Underline Polybar Text with Colour
polybarUnderlineWithColor :: Colour -> String -> String
polybarUnderlineWithColor color = polybarColour 'u' color . polybarUnderline


polyBarAction :: Int -> String -> String-> String
polyBarAction button command
    | button > 8 || button <1 = id
    | otherwise = wrap ("%{A" ++ show button ++ ':':command) "%{A}"


stripNumbers :: String -> String
stripNumbers x
    | ':' `elem` x = let (_:_:xs) = x in xs
    | otherwise = x


