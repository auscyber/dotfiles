import Xmobar

config :: Config
config =
    defaultConfig
        { font = "xft:Roboto Mono Nerd Font:style=Medium:size=10;2"
        , commands =
            [ Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
            , Run $
                Network
                    "enps40"
                    [ "-L"
                    , "0"
                    , "-H"
                    , "32"
                    , "--normal"
                    , "green"
                    , "--high"
                    , "red"
                    ]
                    10
            ]
        }

main :: IO ()
main = xmobar config
