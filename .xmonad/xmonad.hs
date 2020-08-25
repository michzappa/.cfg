import XMonad
import System.Exit
import qualified XMonad.StackSet as W

import Data.Monoid
import qualified Data.Map        as M
import Data.Maybe (maybeToList)
import Data.Tree

import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.SetWMName

import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed

import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.TreeSelect as TS

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Ssh

import Control.Monad (when, join)
import Control.Arrow (first)

myTerminal :: String
myTerminal = "kitty"

myFont :: String
myFont = "xft:JetBrains Mono:pixelsize=16:antialias=true:hinting=true"

myStartupHook :: X ()
myStartupHook = do
      spawnOnce "nitrogen --restore"
      spawnOnce "picom -f" >> addEWMHFullscreen
      spawnOnce "nm-applet"
      spawnOnce "blueman-applet"
      spawnOnce "pasystray"
      spawnOnce "~/.scripts/toggle_trayer.sh"
      spawnOnce "spotify"
      setWMName "LG3D"

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [
    manageDocks
  ]

myHandleEventHook :: Event -> X All
myHandleEventHook = fullscreenEventHook <+> spotifyEventHook

-- Because Spotify is special and has to be dealt with differently
spotifyEventHook :: Event -> X All
spotifyEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift "0")

myLayoutHook = avoidStruts $ myLayouts

myTall :: Tall a
myTall = Tall 1 (3/100) (1/2)

myTallGaps = renamed[Append "-S"] $ renamed [CutWordsLeft 1] $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ myTall

myGrid :: Grid a
myGrid = Grid

myGridGaps = renamed[Append "-S"] $ renamed [CutWordsLeft 1] $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ myGrid

myMirror :: Mirror Tall a
myMirror = Mirror (Tall 1 (3/100) (3/5))

myMirrorGaps = renamed[Append "-S"] $ renamed [CutWordsLeft 1] $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ myMirror

myFull :: Full a
myFull = Full

myLayouts = myTall ||| myTallGaps ||| myGrid ||| myGridGaps ||| myMirror ||| myMirrorGaps ||| myFull
--myLayouts = myTall ||| myGrid ||| myMirror ||| myFull

myLogHook xmproc = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#ECEFF4" "" . wrap "[""]"
                        , ppLayout = xmobarColor "#ECEFF4" ""
                        , ppTitle = xmobarColor "#ECEFF4" "" . shorten 100
                        , ppUrgent = xmobarColor "yellow" "red"
                        }

myKeys :: [(String, X ())]
myKeys =
    -- Application Shortcuts
    [
      ("M-x", spawn "firefox"),
      ("M-c", spawn "code"),
      ("M-n", spawn "thunar"),
      ("M-m", spawn "emacs"),
      ("M-S-/", treeselectAction)
    ]
    ++

    -- Volume, Player, Brightness Manipulation, Keyboard and Systray Change
    [
      ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-"),
      ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+"),
      ("<XF86AudioMute>", spawn "amixer sset Master toggle"),
      ("<XF86AudioPlay>", spawn "playerctl play-pause"),
      ("<XF86AudioStop>", spawn "playerctl stop"),
      ("<XF86AudioPrev>", spawn "playerctl previous"),
      ("<XF86AudioNext>", spawn "playerctl next"),
      ("<XF86MonBrightnessDown>", spawn "light -U 5"),
      ("<XF86MonBrightnessUp>", spawn "light -A 5"),
      ("M-C-k", spawn "~/.scripts/change_keyboard_layout.sh"),
      ("M-C-b", spawn "~/.scripts/toggle_trayer.sh"),
      ("M-C-v", spawn "~/.scripts/toggle_conky.sh")
    ]
    ++

    -- launch a terminal
    [ ("M-<Return>", spawn myTerminal)

    -- launch rofi (application launcher)
    , ("M-/", spawn "rofi -show run -theme $HOME/.config/rofi/grayscale")

    -- close focused window
    , ("M-S-q", kill)

    -- Swap the focused window and the master window
    , ("M-S-<Return>", windows W.swapMaster)

    -- Quit xmonad
    , ("M-S-z", io (exitWith ExitSuccess))

    -- Toggle window tiled or floating
    , ("M-f", withFocused toggleFloatFullscreen)
    ]
    ++

    [
    -- switch to extra workspaces
      ("M-" ++ key, (windows $ W.greedyView ws))
      | (key, ws) <- myExtraWorkspaces
    ]
    ++

    [
    -- shift to extra workspaces
      ("M-S-" ++ key, (windows $ W.shift ws))
      | (key, ws) <- myExtraWorkspaces
    ]
    -- search engine
    ++ [("M-s " ++ key, S.promptSearch myPromptConfig' engine) | (key, engine) <- searchList ]
    ++ [("M-S-s " ++ key, S.selectSearch engine) | (key, engine) <- searchList ]
    -- prompts
    ++

    [
      ("M-p s", sshPrompt myPromptConfig)
    ]

myMouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod right-click, Set the window to floating mode and move by dragging
    [ ((modMask, button3), (\w -> focus w >> mouseMoveWindow w))

    -- mod left-click, Raise the window to the top of the stack
    , ((modMask, button1), (\w -> focus w >> windows W.swapMaster))

    -- mod shift right-click, Set the window to floating mode and resize by dragging
    , ((modMask .|. shiftMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- mod click-scroll, toggle float
    , ((modMask, button2), (\w -> focus w >> toggleFloat w))
    ]

myExtraWorkspaces :: [(String, WorkspaceId)]
myExtraWorkspaces = [("0", "0")]

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2","3","4","5","6","7","8","9"] ++ (map snd myExtraWorkspaces)

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

toggleFloat :: Window -> X ()
toggleFloat w = windows (\s -> if M.member w (W.floating s)
                               then W.sink w s
                               else (W.float w (W.RationalRect (1/4) (1/8) (1/2) (3/4)) s))

toggleFloatFullscreen :: Window -> X ()
toggleFloatFullscreen w = windows (\s -> if M.member w (W.floating s)
                               then W.sink w s
                               else (W.float w (W.RationalRect (0) (0) (1) (1)) s))

myPromptConfig :: XPConfig
myPromptConfig = def
      { font                = myFont
      , bgColor             = "#2E3440"
      , fgColor             = "#d0d0d0"
      , bgHLight            = "#7895b3"
      , fgHLight            = "#000000"
      , borderColor         = "#2E3440"
      , promptBorderWidth   = 0
      , promptKeymap        = myPromptKeymap
      , position            = CenteredAt { xpCenterY = 0.42, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

-- autocomplete turned off for Searching, not launching
myPromptConfig' :: XPConfig
myPromptConfig' = myPromptConfig
      { autoComplete        = Nothing
      }
 
myPromptKeymap :: M.Map (KeyMask,KeySym) (XP ())
myPromptKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) mod1Mask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

archwiki, reddit :: S.SearchEngine

archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("d", S.duckduckgo)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("r", reddit)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

treeselectAction :: X()
treeselectAction = TS.treeselectAction myTreeConfig
   [ Node (TS.TSNode "Keyboard" "" (return ()))
     [
       Node (TS.TSNode "US Default Keyboard" "" (spawn "setxkbmap -layout us")) []
     , Node (TS.TSNode "US International Keyboard" "" (spawn "setxkbmap -layout 'us(intl)'")) []
     ]
   , Node (TS.TSNode "Monitor Layout" "" (return ()))
     [
       Node (TS.TSNode "Laptop" "" (return ()))
       [
        Node (TS.TSNode "No Monitor" "" (spawn "~/.scripts/screenlayout/laptop_no_monitor.sh")) []
        , Node (TS.TSNode "One HDMI Monitor" "" (spawn "~/.scripts/screenlayout/laptop_extra_monitor.sh")) []
       ]
     , Node (TS.TSNode "Desktop" "" (return ()))
       [
        Node (TS.TSNode "One VGA Monitor" "" (spawn "~/.scripts/screenlayout/desktop_one_monitor_vga.sh")) []
       ]
     ]
   , Node (TS.TSNode "Shutdown" "" (spawn "shutdown now")) []
   , Node (TS.TSNode "Suspend" "" (spawn "systemctl suspend")) []
   , Node (TS.TSNode "Restart" "" (spawn "reboot")) []
   , Node (TS.TSNode "Redshift" "" (return ()))
     [
       Node (TS.TSNode "Full" "" (spawn "redshift -O 3500")) []
     , Node (TS.TSNode "Off" "" (spawn "redshift -x")) []
     ]
   ]

myTreeConfig :: TS.TSConfig a
myTreeConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xd02E3440
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffECEFF4, 0xd02E3440)
                              , TS.ts_nodealt      = (0xffECEFF4, 0xd02E3440)
                              , TS.ts_highlight    = (0xffECEFF4, 0xff171A20)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    ]

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc.hs"

    xmonad $ ewmh $ docks def
        { terminal = myTerminal
        , startupHook = myStartupHook
        , manageHook = myManageHook <+> manageHook def
        , layoutHook = myLayoutHook
        , handleEventHook = myHandleEventHook <+> handleEventHook def
        , logHook = myLogHook xmproc
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , workspaces = myWorkspaces
        , mouseBindings = myMouseBindings
        , borderWidth = 0
        } `additionalKeysP` myKeys
