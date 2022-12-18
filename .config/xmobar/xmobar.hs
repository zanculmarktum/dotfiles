import Xmobar

main :: IO ()
main = xmobar =<< configFromArgs defaultConfig
  { font = "-misc-fixed-medium-r-normal--13-*-*-*-*-*-*-*"
  , additionalFonts = ["-wuncon-siji-medium-r-normal--11-*-*-*-*-*-*-*", "xft:FontAwesome:pixelsize=13"]
  , bgColor = "#2e3440"
  , fgColor = "#e5e9f0"
  , lowerOnStart = True
  , persistent = True
  , commands = [ Run $ Battery ["-L","40","-H","90"
                               ,"--low",yellow
                               ,"--high",green
                               ,"-t","<acstatus><left>% / <timeleft>"
                               ,"--"
                               ,"-O","<fc=" ++ white ++ "><icon=power-ac.xbm/></fc>"
                               ,"-i","<fc=" ++ white ++ "><icon=power-ac.xbm/></fc>"
                               ,"-o","<fc=" ++ white ++ "><icon=power-bat.xbm/></fc>"
                               ] 10
               , Run $ Network "wlp3s0" ["-L","0","-H","32"
                                        ,"--normal",green
                                        ,"--high",red
                                        ,"-t","<icon=net-wifi.xbm/><rx>KB|<tx>KB"
                                        ] 10
               , Run $ Network "enp0s25" ["-L","0","-H","32"
                                         ,"--normal",green
                                         ,"--high",red
                                         ,"-t"," <icon=net-wired2.xbm/><rx>KB|<tx>KB"
                                         ] 10
               , Run $ Cpu ["-L","3","-H","50"
                           ,"--normal",green
                           ,"--high",red
                           ,"-t","<icon=cpu.xbm/><total>%"
                           ] 10
               , Run $ Memory ["-t","<icon=mem.xbm/><usedratio>%"] 10
               , Run $ Swap ["-t","<usedratio>%"] 10
               , Run $ Com "uname" ["-s","-r"] "" 36000
               , Run $ Date "<fn=2>\xf073 </fn>%a %b %_d %Y %H:%M:%S" "date" 10
               , Run $ Com ".config/xmobar/padding-icon.sh" ["panel"] "trayer" 10
               , Run $ CoreTemp ["-t", "<icon=temp.xbm/><core0>C"
                                ,"-L", "40", "-H", "60"
                                ,"-l", blue, "-h", red
                                ] 50
               , Run XMonadLog
               ]
  , template = " %XMonadLog% }{ \
               \%wlp3s0%%enp0s25% %battery% %cpu% %coretemp% %memory% / %swap% %date% %trayer%"
  , iconRoot = ".config/xmobar/icons"
  }

black = "#4b5262"
red = "#bf616a"
green = "#a3be8c"
yellow = "#ebcb8b"
blue = "#81a1c1"
magenta = "#b48ead"
cyan = "#89d0bA"
white = "#e5e9f0"
