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
import XMonad.Hooks.EwmhDesktops
import Control.Monad (when, join)
import Data.Maybe (maybeToList)

myStartupHook :: X ()
myStartupHook = do
      spawnOnce "nitrogen --restore"
      spawnOnce "picom -f" >> addEWMHFullscreen
      spawnOnce "nm-applet"
      spawnOnce "blueman-applet"
      spawnOnce "~/.scripts/toggle_trayer.sh"

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

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

    -- Volume, Brightness Manipulation, Keyboard and Systray Change
    [
      ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 5%-"),
      ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 5%+"),
      ((0, xF86XK_AudioMute       ), spawn "amixer set Master toggle"),
      ((0, xF86XK_MonBrightnessDown), spawn "$HOME/.scripts/decrement_screen_brightness.sh"),
      ((0, xF86XK_MonBrightnessUp), spawn "$HOME/.scripts/increment_screen_brightness.sh"),
      ((mod1Mask .|.  controlMask, xK_k), spawn "~/.scripts/change_keyboard_layout.sh"),
      ((mod1Mask .|. controlMask, xK_b), spawn
      "~/.scripts/toggle_trayer.sh")
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
    ++

    [
      ((modm, key), (windows $ W.greedyView ws))
      | (key, ws) <- myExtraWorkspaces
    ]
    ++

    [
      ((modm .|. shiftMask, key), (windows $ W.shift ws))
      | (key, ws) <- myExtraWorkspaces
    ]

myExtraWorkspaces :: [(KeySym, WorkspaceId)]
myExtraWorkspaces = [(xK_0, "0")]
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2","3","4","5","6","7","8","9"] ++ (map snd myExtraWorkspaces)

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc"

    xmonad $ ewmh $ docks def
        { terminal = "kitty"
        , startupHook        = myStartupHook
        , manageHook = myManageHook <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , handleEventHook = fullscreenEventHook <+> handleEventHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#7895b3" "" . wrap "[""]"
                        , ppTitle = xmobarColor "#ABABAB" "" . shorten 50
                        , ppUrgent = xmobarColor "yellow" "red"
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , keys    = customKeys myDeletedKeys myInsertedKeys
        , workspaces = myWorkspaces
        , borderWidth = 0
        }
