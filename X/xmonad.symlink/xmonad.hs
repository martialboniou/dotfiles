{-# OPTIONS_GHC -fglasgow-exts #-}

{-
        xmonad.hs
        Author: Martial Boniou <hondana@gmx.com>
        "I tried to respect the feeling of larswm/marswm by importing RotSlaves"
        note: Layout.SubLayouts should be tried
        note2: two configs here; change 'desktopize mars': True is for XFCE desktop
               and False is for minimalist XMonad with a simple panel (xmobar actually)
-}


import XMonad
import XMonad.Core
import XMonad.Config.Xfce
import qualified XMonad.StackSet as W
import Control.Monad (liftM2)
import qualified Data.Map as M
import Data.Monoid (mappend)
import System.Exit
import System.IO

-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.Themes
import XMonad.Util.Dmenu

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

-- layout
import XMonad.Layout.Gaps
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.LimitWindows
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- actions
import XMonad.Actions.RotSlaves                      
                                                     -- larswm beh (a,semicolon)
import XMonad.Actions.CycleWS                        
                                                     -- new keys (s,-,=,backspace)
import XMonad.Actions.DynamicWorkspaces hiding (selectWorkspace)
import XMonad.Actions.UpdatePointer                  
                                                     -- no titlebar so follow focus but
                                                     -- mouse need management when (j,k)
                                                     -- bindings do the focus

-- general settings
--

type CommandLine = String
type FontName = String

data Confiture = Confiture
     { maxW                    :: Int            -- maximum window to display
     , themeMin, themeMax      :: ThemeInfo      -- themes according to configuration
     , wThickness              :: Dimension      -- window border thickness
     , controlKey, numPadKey   :: KeyMask        -- numPadKey unused
     , spaceList               :: [WorkspaceId]  -- list of workspaces
     , extraSpace              :: WorkspaceId    -- additional workspace
     , logrusMin, logrusMax    :: String         -- minimalist and full terminal
     , desktopize              :: Bool           -- True if maximal configuration
     , defaultFont             :: FontName       -- default font
     , lockMin, lockMax        :: String         -- basic and extra X locker
     , panel                   :: CommandLine    -- panel used in minimalist configuration
     }

mars = Confiture
     { desktopize    = True                      -- True for maximal configuration
     , maxW          = 5
     , defaultFont   = "xft:terminus:size=12"    -- use the same font EVERYWHERE
     , themeMin      = hackATheme deiflTheme
     , themeMax      = hackATheme oxymor00nTheme
     , wThickness    = 1
     , controlKey    = mod1Mask .|. controlMask  -- Key is 'Ctrl+Alt' b/c 
-- Ctrl (Emacs+X11 (default)+URxvt)
-- Alt  (accented chars on Dvorak/MacDV/QwertyCA)
-- Meta (Apple Quartz/Gnome (should not use Alt))
-- if IBM keyboard, change Caps to Ctrl, Ctrl to Alt and Alt to Meta
     , numPadKey     = mod2Mask
     , spaceList     = [ "Code", "COED", "eDoc"] -- infinite workspace = mess
     , extraSpace    = "dECO" 
     , logrusMax     = "gnome-terminal"          -- my own logrus with xft, unicode, "transparency" and xterm-256color name; should be urxvt
     , logrusMin     = logrusMax mars
     , lockMin       = "xlock"
     , lockMax       = "xflock4 || xlock"        -- need 'apt-get install xlockmore' too
     , panel         = "~/.cabal/bin/xmobar"     -- Config in "~/.xmobarrc"
     }
     where
     hackATheme :: ThemeInfo -> ThemeInfo        -- add personal font to a themeInfo
     hackATheme t = TI { themeName = themeName t
                       , themeAuthor = themeAuthor t
                       , themeDescription = themeDescription t
                       , theme = (theme t) { fontName = defaultFont mars }
                       }

-- Keys
--
marsKeys (XConfig {modMask = modm}) = M.fromList $
          [ ((modm .|. shiftMask, xK_KP_Add),       kill) -- methuselah@bbs.archlinux.org advices EZConfig.additionalKeys
          , ((modm,               xK_KP_Subtract),  windows W.focusDown)
          , ((modm .|. shiftMask, xK_KP_Subtract),  windows W.swapDown)
          , ((modm,               xK_KP_Multiply),  windows W.focusUp)
          , ((modm .|. shiftMask, xK_KP_Multiply),  windows W.swapUp)
          , ((modm,               xK_Page_Down),    sendMessage Shrink)
          , ((modm,               xK_Page_Up),      sendMessage Expand)
          , ((modm .|. shiftMask, xK_Page_Up),      increaseLimit)
          , ((modm .|. shiftMask, xK_Page_Down),    decreaseLimit)
          , ((modm,               xK_bracketleft),  sendMessage Shrink)
          , ((modm,               xK_bracketright), sendMessage Expand)
          , ((modm .|. shiftMask, xK_bracketright), increaseLimit)
          , ((modm .|. shiftMask, xK_bracketleft),  decreaseLimit)
          ]
          ++ -- numpad key assoc
          [((m .|. modm, k), windows $ f i)
          | (i,k) <- zip (spaceList mars) numPadKeys
          , (f,m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
          ++ -- larswm-like windows cycle
          [ ((modm,               xK_a),            rotAllUp)
          , ((modm,               xK_semicolon),    rotAllDown)
          , ((modm .|. shiftMask, xK_a),            rotSlavesUp)
          , ((modm .|. shiftMask, xK_semicolon),    rotSlavesDown)
          ]
          ++ -- toggle layouts
          [ ((modm .|. shiftMask, xK_apostrophe),   sendMessage $ Toggle MIRROR)
          , ((modm,               xK_apostrophe),   sendMessage $ Toggle FULL)
          ]
          ++ -- manage/browse workspaces
          [ ((modm,               xK_minus),        nextWS)              -- shift to move current
          , ((modm,               xK_s),            prevWS)
          , ((modm,               xK_equal),        moveTo Next EmptyWS) -- next empty place
          , ((modm .|. shiftMask, xK_minus),        shiftToNext)
          , ((modm .|. shiftMask, xK_s),            shiftToPrev)
          ]
          ++ -- utility keys
          [ ((modm,               xK_Scroll_Lock),  spawn $ lockMin mars)
          , ((modm,               xK_Break),        spawn $ lockMin mars)
          ]

marsExtendedKeys (XConfig {XMonad.modMask = modm}) = M.fromList $
          [ ((modm .|. shiftMask, xK_Home ), spawn "thunar")
          , ((modm,               xK_Home ), spawn "thunar")
          ]
          ++ -- create/destroy workspaces
          [ ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
          , ((modm .|. shiftMask, xK_equal),     nextWS) -- TODO: create new
          ]
         -- ++ -- scratchpad
         -- [ ((modMask , xK_grave), scratchpadSpawnAction xfceConfig { terminal = logrusMax mars }) ]
          ++ -- utility keys
          [ ((0,                  xK_Print),       spawn "scrot")
          , ((modm,               xK_Print),       spawn "sleep 0.2; scrot")
          , ((modm,               xK_apostrophe),  spawn "xfdesktop --windowlist")
          , ((modm,               xK_Scroll_Lock), spawn $ lockMax mars)
          , ((modm,               xK_Break),       spawn $ lockMax mars)
          ]

-- X Classes
--
marsClassManager, marsExtendedClassManager :: ManageHook -- minimalist and full classes
marsClassManager =  composeAll
                    [ className =? "Firefox" <&&> resource =? "Dialog"    --> doFloat
                    , className =? "Firefox" <&&> resource =? "Extension" --> doFloat
                    , className =? "Firefox" <&&> resource =? "Browser"   --> doFloat
                    , className =? "Firefox" <&&> resource =? "Download"  --> doFloat
                    , className =? "Vncviewer"                            --> doFloat
                    ]

marsExtendedClassManager = scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35) -- a nice scratchpad
                         <+> marsClassManager                                        -- my basic classes
                         <+> (composeAll 
                         [ className =? "Gimp"                                 --> doDesignBoard
                         , className =? "Inkscape"                             --> doDesignBoard
                         , className =? "Xfce4-appearance-settings"            --> doFloat
                         , className =? "Xfce4-screenshooter-plugin"           --> doFloat
                         , className =? "Xfrun4"                               --> doFloat
                         , className =? "Xarchiver"                            --> doFloat
                         , className =? "File-roller"                          --> doFloat
                         , className =? "Vncviewer"                            --> (doFloat <+> viewOnLast)
                         ])
                         <+> manageHook xfceConfig
                         <+> manageDocks
                         where
                         doDesignBoard = (liftX $ addUniqueWS (extraSpace mars)) >> doShift (extraSpace mars)
                         addUniqueWS tg = withWindowSet $ \s -> 
                                    if null (filter((== tg) . W.tag)(W.workspaces s))
                                       then addWorkspace tg
                                       else return() -- intensedebate.com/people/neh
                         viewOnLast = viewShift (last $ spaceList mars)
                         viewShift = doF . liftM2 (.) W.greedyView W.shift

-- Log
--
marsSimpleLogHook :: Handle -> X()
marsSimpleLogHook h = (dynamicLogWithPP $ xmobarPP
        { ppHidden  = xmobarColor "#00FF00" ""
	    , ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]"
	    , ppUrgent  = xmobarColor "#FF0000" "" . wrap "*" "*"
        , ppLayout  = xmobarColor "#FF0000" ""
        , ppTitle   = xmobarColor "#00FF00" "" . shorten 80
        , ppSep     = "<fc=#0033FF> | </fc>"
        , ppOutput  = hPutStrLn h
        }) >> marsLogHook -- the simplest log includes a xmobar panel

marsLogHook = ewmhDesktopsLogHook >> updatePointer (Relative 0.10 0.10)

-- Hook
--
marsCommonSystemHook :: XConfig a -> XConfig a
marsCommonSystemHook c = c
        { borderWidth      = wThickness mars
        , modMask          = controlKey mars
        , workspaces       = spaceList mars
        , startupHook      = startupHook c +++ setWMName "LG3D"
        , handleEventHook  = handleEventHook c
        }
        where
        x +++ y = mappend x y

-- Mains                  
--
marsMinSystem :: IO () -- minimalist system using xmobar
marsMinSystem = do
                xpanel <- spawnPipe $ panel mars
                xmonad $ marsCommonSystemHook (ewmh defaultConfig
                       { terminal           = logrusMin mars
                       , keys               = \c -> marsKeys c `M.union` keys defaultConfig c
                       , manageHook         = marsClassManager
                       , layoutHook         = marsMinStyle
                       , logHook            = marsSimpleLogHook xpanel
                       , normalBorderColor  = inactiveBorderColor marsMinTheme
                       , focusedBorderColor = activeBorderColor marsMinTheme
                       })
                       where
                       marsMinTheme = theme $ themeMin mars
                       marsMinStyle = commonStyle
                                    $ onWorkspace (last (spaceList mars)) (readStyle dockStyle)
                                    $ dockStyle -- use tabStyle b/c no task panel

marsMaxSystem :: IO () -- full system using XFCE desktop
marsMaxSystem = do
                xmonad $ marsCommonSystemHook xfceConfig -- ewmh included here
                       { terminal           = logrusMax mars
                       , keys               = \c -> marsKeys c `M.union` marsExtendedKeys c `M.union` keys xfceConfig c
                       , manageHook         = marsExtendedClassManager
                       , layoutHook         = marsMaxStyle
                       , logHook            = marsLogHook
                       , normalBorderColor  = inactiveBorderColor marsMaxTheme
                       , focusedBorderColor = activeBorderColor marsMaxTheme
                       } -- `additionalKeys` marsExtendedKeys
                       where
                       marsMaxTheme = theme $ themeMax mars
                       marsMaxStyle = commonStyle 
                                    $ onWorkspace (last (spaceList mars)) (readStyle extendedStyle)
                                    $ onWorkspace (extraSpace mars) gimpStyle
                                    $ extendedStyle -- if XFCE then no tabs

-- Run xmonad with all the defaults we set up
--
main :: IO () -- full (XFCE'd + dynamic workspaces) or minimalist (xmobar'd) system
main = if (desktopize mars)
          then marsMaxSystem
          else marsMinSystem

-- Layout
--
commonStyle s = avoidStruts $ limitWindows (maxW mars)
                            $ mkToggle (single MIRROR)
                            . mkToggle (NOBORDERS ?? FULL ?? EOT) $ s
readStyle s   = columnStyle ||| fancyStyle ||| s -- read: column / fancy / other
dockStyle     = extendedStyle ||| tabStyle -- dock: base / book / tabs
extendedStyle = baseStyle ||| bookStyle
bookStyle     = TwoPane delta ratio
        where
        delta = 3/100   -- percent of screen to increment by when resizing panes
        ratio = 1/2     -- default proportion of screen occupied by master pane
baseStyle     = Tall nmaster delta ratio
        where
        nmaster = 1      -- the default number of windows in the master pane
        delta   = 3/100  -- percent of screen to increment by when resizing panes
        ratio   = 65/100 -- default proportion of screen occupied by master pane
columnStyle   = gaps [(R,50),(L,50)] $ Full -- FIXME: (replace me)
fancyStyle    = Circle
tabStyle      = tabbed shrinkText $ theme $ themeMin mars -- tabs are for marsMinSystem
gimpStyle = withIM(0.11) (Role "gimp-toolbox") $ reflectHoriz 
          $ withIM(0.15) (Role "gimp-dock") Full

-- Non-numeric num pad keys, sorted by number
--
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1 2 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4 5 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7 8 9
             , xK_KP_Insert] -- 0

-- hack selectWorkspace (65.254.53.221:8000/6431)
--
selectWorkspace :: X ()
selectWorkspace = do
        s <- gets windowset
        w <- dmenu $ map W.tag $ W.workspaces s
        if W.tagMember w s
           then windows $ W.greedyView w
           else addWorkspace w
                
-- http://www.haskell.org/haskellwiki/HaskellWiki:Copyrights
