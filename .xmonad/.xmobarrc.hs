Config {

   -- appearance
     font =         "xft:JetBrains Mono:pixelsize=16:antialias=true:hinting=true"
   , bgColor =      "#2E3440"
   , fgColor =      "#ABABAB"
   , position =     Top
   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)
   , template = " %StdinReader% %music%}{ %default:Master%| %battery% | %date% | %kbd% "

   , commands =
        [
          Run Com "/bin/bash" ["-c", "~/.scripts/spotify_info.sh"] "music" 10
          --volume monitor
         , Run Volume "default" "Master" ["--template", "<fc=#ECEFF4>Vol:</fc> <volume>% <status>"
                                         ,"--"
                                            , "--onc", "#ABABAB"
                                            , "--offc", "#ABABAB"

                                         ] 5

        -- battery monitor
        , Run Battery        [ "--template" , "<fc=#ECEFF4>Batt:</fc> <acstatus>"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "Charging"
                                       -- charged status
                                       , "-i"	, "Charged"
                             ] 50

        -- time and date indicator
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run Date           "<fc=#ECEFF4>%F (%a) %I:%M</fc>" "date" 10

        -- keyboard layout indicator
        , Run Kbd            [ ("us(alt-intl)" , "<fc=#ECEFF4>INT</fc> ")
                             , ("us"         , "<fc=#ECEFF4>US</fc> ")
                             ]
        , Run StdinReader
        ]
   }
