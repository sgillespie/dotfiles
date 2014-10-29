import XMonad
import XMonad.Hooks.DynamicLog

main :: IO ()
main = xmonad =<< xmobar' config'

xmobar' = xmobar
config' = defaultConfig {
  terminal = "urxvt",
  modMask = mod4Mask
  }

       
