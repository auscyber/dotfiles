module Desktop where

import SysDependent (ExtraConfig (..))

rclonemounts =
    [ ("driveschool", "~/school_drive", ["--vfs-cache-mode full", "--vfs-cache-max-age 10m"])
    , --  [ ("cache", "~/school_drive", []),
      ("drivepersonal", "~/drive", ["--vfs-cache-mode full", "--vfs-cache-max-age 10m"])
    , ("photos", "~/Pictures", [])
    , ("onedrive_school", "~/onedrive_school", [])
    ]

mountRclone :: (String, String, [String]) -> String
mountRclone (name, location, extra_args) = concat ["rclone mount ", name, ": ", location, " ", unwords extra_args, " --daemon"]

dependentConf =
    ExtraConfig
        { titleLength = 50
        , launchApps =
            [ "libinput-gestures-setup start"
            ]
        , onceApps = map mountRclone rclonemounts
        }
