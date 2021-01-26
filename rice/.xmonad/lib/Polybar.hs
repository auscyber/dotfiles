{-# LANGUAGE TupleSections #-}
module Polybar (
switchMoveWindowsPolybar, dynamicLogIcons',polybarColour,polybarUnderline,polybarUnderlineWithColor,polybarPP,IconConfig(..)) 
where

import DynamicLog
import XMonad.Hooks.DynamicLog
import XMonad
import Data.Maybe
import qualified Data.Map                            as M
import Data.List
import ExtraState
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS
--Process Colours
type Colour = String
polybarColour :: Char -> Colour -> String -> String
polybarColour area (_:color) text = "%{" ++ [area] ++ color ++ "}" ++ text ++ "%{" ++ area:"--}"


polybarPP ws =  def {
    {-ppCurrent = polybarWorkspace (polybarUnderlineWithColor "#FFCFD1" . polybarColour 'F' "#FFDB9E" ) ws False
    , ppHidden = polybarWorkspace (polybarColour 'F' "#E88B84") ws True
    , ppVisible = polybarWorkspace (polybarColour 'F' "#FFC9AB"  . wrap "[" "]") ws True
    , ppHiddenNoWindows = polybarWorkspace (polybarColour 'F' "#5754B3") ws True -}
    ppCurrent =  polybarColour 'F' "#FFDB9E" . current .  checkIcon ws
    , ppHidden =  polybarColour 'F' "#E88B84" . hidden . checkIcon ws
    , ppVisible  = polybarColour 'F' "#FFC9AB" . hidden . checkIcon ws
    , ppHiddenNoWindows = polybarColour 'F' "#5754B3" . hidden . checkIcon ws
    , ppTitleSanitize = take 90
    , ppTitle = polybarColour 'F' "#--" 
    , ppSep = polybarColour 'F' "#5754B3" " | "
    , ppOutput = io . appendFile "/tmp/.xmonad-workspace-log" . flip (++) "\n"  . xmonadPolybarAction 4 "nextws" . xmonadPolybarAction 5 "prevws"
   , ppLayout =  xmonadPolybarAction  1 "next-layout" . xmonadPolybarAction 3 "default-layout" 
                        
--    , ppOrder = \(x:_:y) -> x:y

}
checkIcon :: M.Map WorkspaceId WorkSpaceIconSet -> String -> WorkSpaceIconSet
checkIcon ws wsid = M.findWithDefault (basicIconSet wsid) wsid ws

type IconSet = Query [WorkSpaceIconSet]
data IconConfig = IconConfig {
        iconConfigIcons :: Query [WorkSpaceIconSet] 
        , iconConfigStack :: [String] -> String
        }


instance Default IconConfig where
    def = IconConfig {
    iconConfigIcons = composeAll [
        className =? "discord" --> appIcon "\xfb6e" 
        , className =? "Spotify" <||> className =? "spotify" -->  appIcon "阮"]
    , iconConfigStack = ("["<>) . (<>"]") . unwords 
    }
appIcon :: String -> IconSet
appIcon = pure . (:[]) . basicIconSet

basicIconSet :: String -> WorkSpaceIconSet
basicIconSet x = NameSet { current = x,visible = x,hidden =x, showOverlayIcons = True }

dynamicLogIcons' :: IconConfig -> PP -> X PP
dynamicLogIcons' IconConfig{ iconConfigIcons = is, iconConfigStack = stack } pp = do
        ws <- gets windowset   
        let workspaces' = map S.workspace (S.current ws : S.visible ws) <> S.hidden ws
        icons <- M.fromList . (maybeToList =<<) <$> mapM (getIcons is) workspaces' 
        pure $ pp {
            ppCurrent = ppCurrent pp . concatIcons current . iconLookup icons 
            ,ppVisible =  ppVisible pp . concatIcons visible . iconLookup icons
            ,ppHidden = ppHidden pp . concatIcons hidden . iconLookup icons
            ,ppHiddenNoWindows = ppHiddenNoWindows pp . concatIcons hidden . iconLookup icons
        }
        where iconLookup icons x = M.findWithDefault [basicIconSet x] x icons
              concatIcons f y 
                    | length y > 1 = stack $ map f y
                    | otherwise = concatMap f y
                              
getIcons :: IconSet -> S.Workspace WorkspaceId l Window -> X (Maybe (WorkspaceId,[WorkSpaceIconSet]))
getIcons is w = do
            let windows'' stack' = [S.focus stack'] ++ S.up stack' ++ S.down stack'
            a <- sequence $ foldMap (runQuery is) . windows'' <$> S.stack w
            pure $ (S.tag w,) <$> (a >>= \x -> if null x then Nothing else Just  x)
            
switchMoveWindowsPolybar :: PP -> PP
switchMoveWindowsPolybar pp = pp
            { ppCurrent = switchAndMove (ppCurrent pp)
            , ppHidden = switchAndMove (ppHidden pp)
            , ppVisible = switchAndMove (ppVisible pp)
            , ppHiddenNoWindows = switchAndMove (ppHiddenNoWindows pp)
            }
            where switchAndMove f x =  xmonadPolybarAction 1 ("view"++x) . xmonadPolybarAction 3 ("moveTo"++x) $ f x



    

--polybarWorkspace :: (String -> String)  -> Bool -> String -> String
--polybarWorkspace format bool str = (if bool then moveToWS str . switchWS str  else id) . format $ str
--Underline Text
polybarUnderline :: String -> String
polybarUnderline text = "%{+u}" ++ text ++ "%{-u}"
--Underline Polybar Text with Colour
polybarUnderlineWithColor :: Colour -> String -> String
polybarUnderlineWithColor color = polybarColour 'u' color . polybarUnderline

