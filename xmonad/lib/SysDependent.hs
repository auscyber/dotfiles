module SysDependent (sysDependent) where

import XMonad

hook = do
    spawn "polychromatic-tray-applet"

sysDependent :: XConfig l -> XConfig l
sysDependent conf =
    conf
        { logHook = logHook conf >> hook
        }
