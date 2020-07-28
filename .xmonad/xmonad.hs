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
import XMonad.Hooks.EwmhDesktops
import Control.Monad (when, join)
import Data.Maybe (maybeToList)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.Submap
import qualified XMonad.Actions.Search as S
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import Control.Arrow (first)
import qualified XMonad.Actions.TreeSelect as TS
import Data.Tree
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed

myTerminal :: String
myTerminal = "kitty"

myFont :: String
myFont = "xft:System San Fransisco Display:pixelsize=16:antialias=true:hinting=true"

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

treeselectAction = TS.treeselectAction myTreeConfig
   [ Node (TS.TSNode "Keyboard" "" (return ()))
     [
       Node (TS.TSNode "US Default Keyboard" "" (spawn "setxkbmap -layout us")) []
     , Node (TS.TSNode "US International Keyboard" "" (spawn "setxkbmap -layout 'us(intl)'")) []
     ]
   , Node (TS.TSNode "Shutdown" "" (spawn "shutdown now")) []
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
                              , TS.ts_node         = (0xffd0d0d0, 0xd02E3440)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xd02E3440)
                              , TS.ts_highlight    = (0xff88C0D0, 0xff2E3440)
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

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [
    manageDocks
  ]

--myLayoutHook = avoidStruts $ layoutHook def
myLayoutHook = avoidStruts $ myLayouts

myTall = Tall 1 (3/100) (1/2)
myGrid = Grid
myFull = Full
myMirror = Mirror (Tall 1 (3/100) (3/5))

myLayouts = renamed [CutWordsLeft 1] $ spacing 5 $ myTall ||| myGrid ||| myFull ||| myMirror

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
    , ("M-/", spawn "rofi -show run -theme $HOME/.config/rofi/nord")

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
    ++ [("M-s " ++ key, S.promptSearch myPromptConfig' engine) | (key, engine) <- searchList ]
    ++ [("M-S-s " ++ key, S.selectSearch engine) | (key, engine) <- searchList ]

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
                        , ppCurrent = xmobarColor "#88C0D0" "" . wrap "[""]"
                        , ppTitle = xmobarColor "#ABABAB" "" . shorten 50
                        , ppUrgent = xmobarColor "yellow" "red"
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        --, keys    = customKeys myDeletedKeys myInsertedKeys
        , workspaces = myWorkspaces
        , borderWidth = 0
        } `additionalKeysP` myKeys
