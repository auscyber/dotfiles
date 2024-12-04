module Laptop where

import SysDependent

dependentConf =
    ExtraConfig
        { titleLength = 32
        , launchApps =
            [ "libinput-gestures-setup start"
            ]
        , onceApps = []
        }
