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
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.Submap

myTerminal :: String
myTerminal = "kitty"

myStartupHook :: X ()
myStartupHook = do
      spawnOnce "nitrogen --restore"
      spawnOnce "picom -f" >> addEWMHFullscreen
      spawnOnce "nm-applet"
      spawnOnce "blueman-applet"
      spawnOnce "pasystray"
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

myLayoutHook = avoidStruts $ layoutHook def

myKeys :: [(String, X ())]
myKeys =
    -- Application Shortcuts
    [
      ("M-x", spawn "firefox"),
      ("M-c", spawn "code"),
      ("M-n", spawn "thunar"),
      ("M-m", spawn "emacs")
    ]
    ++

    -- Volume, Brightness Manipulation, Keyboard and Systray Change
    [
      ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-"),
      ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+"),
      ("<XF86AudioMute>", spawn "amixer sset Master toggle"),
      ("<XF86MonBrightnessDown>", spawn "light -U 10"),
      ("<XF86MonBrightnessUp>", spawn "light -A 10"),
      ("M-C-k", spawn "~/.scripts/change_keyboard_layout.sh"),
      ("M-C-b", spawn "~/.scripts/toggle_trayer.sh")
    ]
    ++

    -- launch a terminal
    [ ("M-<Return>", spawn myTerminal)

    -- launch rofi (application launcher)
    , ("M-/", spawn "rofi -show run -lines 5 -eh 2 -width 20 -padding 10 -theme $HOME/.config/rofi/arc-dark")

    -- close focused window
    , ("M-S-q", kill)

    -- Swap the focused window and the master window
    , ("M-S-<Return>", windows W.swapMaster)

    -- Quit xmonad
    , ("M-S-z", io (exitWith ExitSuccess))
    ]
    ++

    [
      ("M-" ++ key, (windows $ W.greedyView ws))
      | (key, ws) <- myExtraWorkspaces
    ]
    ++

    [
      ("M-S-" ++ key, (windows $ W.shift ws))
      | (key, ws) <- myExtraWorkspaces
    ]

myExtraWorkspaces :: [(String, WorkspaceId)]
myExtraWorkspaces = [("0", "0")]
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2","3","4","5","6","7","8","9"] ++ (map snd myExtraWorkspaces)

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc"

    xmonad $ ewmh $ docks def
        { terminal = myTerminal
        , startupHook        = myStartupHook
        , manageHook = myManageHook <+> manageHook def
        , layoutHook = myLayoutHook
        , handleEventHook = fullscreenEventHook <+> handleEventHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#7895b3" "" . wrap "[""]"
                        , ppTitle = xmobarColor "#ABABAB" "" . shorten 50
                        , ppUrgent = xmobarColor "yellow" "red"
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        --, keys    = customKeys myDeletedKeys myInsertedKeys
        , workspaces = myWorkspaces
        , borderWidth = 0
        } `additionalKeysP` myKeys
