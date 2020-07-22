import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import Data.Monoid
import System.Exit
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.CustomKeys

myStartupHook :: X ()
myStartupHook = do
      spawnOnce "nitrogen --restore &"
      spawnOnce "picom -f &"

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [
    manageDocks
  ]

myDeletedKeys :: XConfig l -> [(KeyMask, KeySym)]
myDeletedKeys XConfig {modMask = modm} =
    [
      (modm, xK_Return) -- was swap focused window with master, now open terminal
    , (modm .|. shiftMask, xK_Return) -- was open terminal, now swap focused window with master
    , (modm, xK_p) -- was open prompt
    , (modm .|. shiftMask, xK_q) -- was quit XMonad, now close focused window
    , (modm, xK_comma) -- was increment number of windows in master area
    , (modm, xK_period) -- was decrement number of windows in master area
    , (modm, xK_m) -- was move focus to master window, now open emacs

    ]

myInsertedKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
myInsertedKeys conf@(XConfig {modMask = modm}) =
    -- Application Shortcuts
    [
      ((modm, xK_x), spawn "firefox"),
      ((modm, xK_c), spawn "code"),
      ((modm, xK_n), spawn "thunar"),
      ((modm, xK_m), spawn "emacs")
    ]
    ++

    -- Volume, Brightness Manipulation and Keyboard Change
    [
      ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 5%-"),
      ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 5%+"),
      ((0, xF86XK_AudioMute       ), spawn "amixer set Master toggle"),
      ((0, xF86XK_MonBrightnessDown), spawn "$HOME/.scripts/decrement_screen_brightness.sh"),
      ((0, xF86XK_MonBrightnessUp), spawn "$HOME/.scripts/increment_screen_brightness.sh"),
      ((mod1Mask .|.  controlMask, xK_k), spawn "~/.scripts/change_keyboard_layout.sh")
    ]
    ++

    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- launch rofi (application launcher)
    , ((modm,               xK_slash     ), spawn "rofi -show run -lines 5 -eh 2 -width 20 -padding 10 -theme $HOME/.config/rofi/arc-dark")

    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_z     ), io (exitWith ExitSuccess))
    ]

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc"

    xmonad $ docks def
        { terminal = "kitty"
        , startupHook        = myStartupHook
        , manageHook = myManageHook <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#7895b3" "" . wrap "[""]"
                        , ppTitle = xmobarColor "#ABABAB" "" . shorten 50
                        , ppUrgent = xmobarColor "yellow" "red"
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , keys    = customKeys myDeletedKeys myInsertedKeys
        }
