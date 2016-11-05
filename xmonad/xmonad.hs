import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Replace

import System.IO

main :: IO ()
main = replace >> (xmobar' . additionalKeys') config' >>= xmonad

xmobar' = xmobar

config' = defaultConfig {
  layoutHook  = avoidStruts $ layoutHook defaultConfig,
  manageHook  = manageDocks <+> manageHook defaultConfig,
  startupHook = setWMName "LG3D",

  borderWidth        = 1,
  focusFollowsMouse  = False,
  focusedBorderColor = "yellow",
  modMask            = mod4Mask,
  normalBorderColor  = "#000000",
  terminal           = "urxvt"
  }

additionalKeys' = flip additionalKeys $
  [((mod4Mask .|. shiftMask, xK_l), spawn "sxlock -f '-*-ubuntu-medium-r-normal-*-30-*-*-*-*-*-iso8859-1'"),
   ((0, xK_Print), spawn "import -window root /home/sgillespie/documents/screenshots/$(date +%Y%m%d%H%M%S).png"),
   ((0, 0x1008FF12), spawn "amixer sset Master toggle"),
   ((0, 0x1008FF11), spawn "amixer sset Master 5%-"),
   ((0, 0x1008FF13), spawn "amixer sset Master 5%+"),
   ((mod4Mask, xK_Print), spawn "import -window 0 /home/sgillespie/documents/screenshots/$(date +%Y%m%d%H%M%S).png"),
   ((mod4Mask .|. mod1Mask, xK_Return), spawn "urxvt -e bash -c \"tmux -q has-session && exec tmux attach-session -d || exec tmux new-session -s$USER@$HOSTNAME\""),
   ((mod4Mask .|. mod1Mask, xK_c), spawn "chromium"),
   ((mod4Mask .|. mod1Mask, xK_e), spawn "emacs"),
   ((mod4Mask .|. mod1Mask, xK_f), spawn "firefox"),
   ((mod4Mask .|. mod1Mask, xK_i), spawn "idea.sh"),
   ((mod4Mask .|. mod1Mask, xK_t), spawn "tor-browser-en"),
   ((mod4Mask .|. mod1Mask, xK_v), spawn "gvim")]

