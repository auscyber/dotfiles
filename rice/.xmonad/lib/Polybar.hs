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


polybarPP ws =  def {
    {-ppCurrent = polybarWorkspace (polybarUnderlineWithColor "#FFCFD1" . polybarColour 'F' "#FFDB9E" ) ws False
    , ppHidden = polybarWorkspace (polybarColour 'F' "#E88B84") ws True
    , ppVisible = polybarWorkspace (polybarColour 'F' "#FFC9AB"  . wrap "[" "]") ws True
    , ppHiddenNoWindows = polybarWorkspace (polybarColour 'F' "#5754B3") ws True -}
    ppCurrent = \x ->  switchWS x $ polybarColour 'F' "#FFDB9E" "\xf111 "
    , ppHidden = \x ->  switchWS x $ polybarColour 'F' "#E88B84" "\xf10c "
    , ppVisible  = \x -> switchWS x $ polybarColour 'F' "#FFC9AB" "\xf10c "
    , ppHiddenNoWindows = \x -> switchWS x $ polybarColour 'F' "#5754B3" "\xf10c "
    , ppTitleSanitize = take 90
    , ppTitle = polybarColour 'F' "#--" 
    , ppSep = polybarColour 'F' "#5754B3" " | "
    , ppOutput = io . appendFile "/tmp/.xmonad-workspace-log" . flip (++) "\n" . xmonadAction 4 "nextws" . xmonadAction 5 "prevws"
    , ppLayout =  xmonadAction 1 "next-layout" . xmonadAction 3 "default-layout" 
                        
--    , ppOrder = \(x:_:y) -> x:y

}

polybarWorkspace :: (String -> String)  -> M.Map Int String -> Bool -> String -> String
polybarWorkspace format ws bool str = (if bool then moveToWS str . switchWS str  else id) . format $ getWorkspaceText ws str
--Underline Text
polybarUnderline :: String -> String
polybarUnderline text = "%{+u}" ++ text ++ "%{-u}"
--Underline Polybar Text with Colour
polybarUnderlineWithColor :: Colour -> String -> String
polybarUnderlineWithColor color = polybarColour 'u' color . polybarUnderline


switchWS :: String -> String -> String 
switchWS id = xmonadAction 1 ("view"++id) 

moveToWS :: String -> String -> String
moveToWS id = xmonadAction 3 ("moveTo"++id)

xmonadAction :: Int -> String -> String -> String
xmonadAction but x = polyBarAction but ("~/.xmonad/xmonadctl " ++x)


polyBarAction :: Int -> String -> String-> String
polyBarAction button command
    | button > 8 || button <1 = id
    | otherwise = wrap ("%{A" ++ show button ++ ":" ++ command ++ ":}") "%{A}"




