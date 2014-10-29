Config { 
  -- appearance
  font        = "xft:Cantarell Bold:size=11:bold:antialias=true",
  bgColor     = "#002B36",
  fgColor     = "#646464",
  position    = Top,
  border      = BottomB,
  borderColor = "#646464",

  -- layout
  sepChar  = "%",   -- delineator between plugin names and straight text
  alignSep = "}{",  -- separator between left-right alignment
  template = "%battery% | %multicpu% | %memory% } %date% { %KJAX% ",

  -- general behavior
  lowerOnStart     = True,    -- send to bottom of window stack on start
  hideOnStart      = False,   -- start with window unmapped (hidden)
  allDesktops      = True,    -- show on all desktops
  overrideRedirect = True,    -- set the Override Redirect flag (Xlib)
  pickBroadest     = False,   -- choose widest display (multi-monitor)
  persistent       = True,    -- enable/disable hiding (True = disabled)

  -- plugins
  --   Numbers can be automatically colored according to their value. xmobar
  --   decides color based on a three-tier/two-cutoff system, controlled by
  --   command options:
  --     --Low sets the low cutoff
  --     --High sets the high cutoff
  --
  --     --low sets the color below --Low cutoff
  --     --normal sets the color between --Low and --High cutoffs
  --     --High sets the color above --High cutoff
  --
  --   The --template option controls how the plugin is displayed. Text
  --   color can be set by enclosing in <fc></fc> tags. For more details
  --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
  commands =
    -- weather monitor
    [Run Weather "KJAX"
                ["--template",
                 "<fc=#4682B4><tempF></fc>°F | <skyCondition>"]
                36000,

     -- cpu activity monitor
     Run MultiCpu ["--template", "Cpu: <total0>%, <total1>%",
                   "--Low", "50",         -- units: %
                   "--High", "85",        -- units: %
                   "--low", "darkgreen",
                   "--normal", "darkorange",
                   "--high", "darkred"]
                  10,

     -- memory usage monitor
     Run Memory ["--template", "Mem: <usedratio>%",
                 "--Low", "20",        -- units: %
                 "--High", "90",       -- units: %
                 "--low", "darkgreen",
                 "--normal", "darkorange",
                 "--high", "darkred"]
                10,

        -- battery monitor
     Run Battery [ "--template", "Battery: <acstatus>",
                   "--Low", "10",        -- units: %
                   "--High", "80",        -- units: %
                   "--low", "darkred",
                   "--normal", "darkorange",
                   "--high", "darkgreen",

                   "--", -- battery specific options
                   -- discharging status
                   "-o", "<left>% (<timeleft>)",
                   -- AC "on" status
                   "-O", "<fc=#dAA520>Charging</fc>",
                   -- charged status
                   "-i", "<fc=#006000>Charged</fc>"]
                  50,

     -- time and date indicator 
     --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
     Run Date "<fc=#AAAAAA>%A, %m/%d/%Y, %H:%M</fc>" "date" 10]]
  }
