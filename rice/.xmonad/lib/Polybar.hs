module Polybar (
polybarColour,polybarUnderline,polybarUnderlineWithColor,polyBarAction,polybarPP) 
where
import DynamicLog
import XMonad
import qualified Data.Map                            as M

--Process Colours
type Colour = String
polybarColour :: Char -> Colour -> String -> String
polybarColour area (_:color) text = "%{" ++ [area] ++ color ++ "}" ++ text ++ "%{" ++ area:"--}"

getWorkspaceText :: M.Map Int String -> String -> String
getWorkspaceText xs n = 
    case M.lookup (read n) xs of
        Just x ->  x
        _ ->  n


--FFC9AB
--E88B84
--804144
polybarPP ws =  def {
    ppCurrent = polybarColour 'F' "#FFDB9E" .  polybarUnderlineWithColor "#FFCFD1" . getWorkspaceText ws
    , ppTitle = polybarColour 'F' "#--" . take 90 
    , ppHidden = polybarColour 'F' "#E88B84" . getWorkspaceText ws
    , ppVisible = polybarColour 'F' "#FFC9AB"  . wrap "[" "]" . getWorkspaceText ws
    , ppHiddenNoWindows = polybarColour 'F' "#5754B3" . getWorkspaceText ws
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




