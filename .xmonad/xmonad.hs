-- Written for xmonad-0.17.0 and xmonad-contrib-0.17.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}

import XMonad

import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarProp)
import XMonad.Hooks.StatusBar
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier

import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.FuzzyMatch

import qualified XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.EwmhDesktops hiding (ewmhFullscreen,
                                         ewmhDesktopsStartup,
                                         fullscreenEventHook)
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils (fi)
import XMonad.Util.WindowProperties (getProp32)
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid

import XMonad.Layout.NoBorders

import XMonad.Hooks.ManageHelpers

import qualified Data.Map as M

import XMonad.Actions.WithAll

import qualified XMonad.Util.ExtensibleState as XS
import System.Posix.Signals
import System.Posix.Types
import Control.Concurrent (threadDelay)

import Data.Char

import Control.Exception (SomeException, try)

import XMonad.Util.NamedWindows
import System.Directory

-- Remember the spawned PIDs
newtype SpawnOnce = SpawnOnce { unspawnOnce :: M.Map String ProcessID }
    deriving (Typeable, Read, Show)

instance ExtensionClass SpawnOnce where
    initialValue = SpawnOnce M.empty
    extensionType = PersistentExtension

-- | The first time 'spawnOnce' is executed on a particular command,
-- that command is executed.  Subsequent invocations for a command do
-- nothing.
spawnOnce :: String -> X ()
spawnOnce s = do
  b <- XS.gets (M.member s . unspawnOnce)
  unless b $ do
    pid <- spawnPID s
    XS.modify (SpawnOnce . M.insert s pid . unspawnOnce)

killOnce :: X ()
killOnce = do
  void $ M.traverseWithKey (const killPID) =<< XS.gets unspawnOnce
  --XS.modify (SpawnOnce . const M.empty)

killPID :: ProcessID -> X ()
killPID pid = io $ do
  void $ try @SomeException (signalProcessGroup sigTERM pid)
  --threadDelay 50000

-- Remember floating windows positions
newtype FloatingRectangle = FloatingRectangle (M.Map Window W.RationalRect)
  deriving (Typeable, Read, Show)

instance ExtensionClass FloatingRectangle where
  initialValue = FloatingRectangle M.empty
  extensionType = PersistentExtension

fromFloatRect :: FloatingRectangle -> M.Map Window W.RationalRect
fromFloatRect (FloatingRectangle m) = m

ewmhFullscreen :: XConfig a -> XConfig a
ewmhFullscreen c = (XMonad.Hooks.EwmhDesktops.ewmhFullscreen c)
  { handleEventHook = handleEventHook c <+> fullscreenEventHook
  }

fullscreenEventHook :: Event -> X All
fullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  managed <- isClient win
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] <$> getProp32 wmstate win

  let isFull = fromIntegral fullsc `elem` wstate

      -- Constants for the _NET_WM_STATE protocol:
      remove = 0
      add = 1
      toggle = 2
      chWstate f = io $ changeProperty32 dpy win wmstate aTOM propModeReplace (f wstate)

      fullRect = W.RationalRect 0 0 1 1
      noRect = W.RationalRect (-100) (-100) (-100) (-100)

  do
    rect <- withWindowSet $ pure . M.findWithDefault noRect win . W.floating

    when (rect /= fullRect && rect /= noRect) $
      XS.modify (FloatingRectangle . M.insert win rect . fromFloatRect)

  rect <- M.findWithDefault noRect win . fromFloatRect <$> XS.get

  when (managed && typ == wmstate && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWstate (fi fullsc:)
      windows $ W.float win fullRect
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ delete (fi fullsc)
      when (rect /= fullRect && rect /= noRect) $
        windows $ W.float win rect
      when (rect == noRect) $
        windows $ W.sink win

  return $ All True

fullscreenEventHook _ = return $ All True

xmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar = statusBar "xmobar"
         xmobarPP { ppTitle = xmobarColor "green"  "" . shorten 1000
                  }
         toggleStrutsKey

xmobarProp :: LayoutClass l Window
           => XConfig l  -- ^ The base config
           -> XConfig (ModifiedLayout AvoidStruts l)
xmobarProp =
  withEasySB (statusBarProp "xmobar"
              (pure xmobarPP { ppCurrent = xmobarColor yellow "" . wrap "[" "]"
                             , ppTitle   = xmobarColor green "" . shorten 1000
                             , ppUrgent  = xmobarColor red yellow
                             })) toggleStrutsKey

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

escapeSingleQuotes :: String -> String
escapeSingleQuotes = f . span (/= '\'')
  where
    f (first, "")       = first
    f (first, (x:last)) = first ++ ("'\\" ++ [x] ++ "'") ++ escapeSingleQuotes last

black = "#4b5262"
red = "#bf616a"
green = "#a3be8c"
yellow = "#ebcb8b"
blue = "#81a1c1"
magenta = "#b48ead"
cyan = "#89d0bA"
white = "#e5e9f0"

main :: IO ()
main = xmonad . xmobarProp . ewmhFullscreen . ewmh $ def
  { normalBorderColor  = "#a6a6a6"
  , focusedBorderColor = white
  , terminal           = "urxvt"
  , layoutHook         = lessBorders OnlyScreenFloat tiled ||| Mirror tiled ||| Full
  , manageHook         = let doWindow = map . (. (className =?)) . (flip (-->))
                             float = doWindow doFloat
                             centerFloat = doWindow doCenterFloat
                             match p s = (== s) . map toLower . take (length s) <$> p
                         in composeAll (float ["mpv", "MEGAsync", "factorio", "Display", "gzdoom"
                                              ,"explorer.exe", "regedit.exe", "control.exe"
                                              ,"kmplot", "qt5ct", "milkytracker", "vlc"
                                              ,"ollydbg.exe", "ZeGrapher"]
                                        ++ centerFloat ["Sxiv","Zathura","Org.gnome.Nautilus"
                                                       ,"Qemu-system-x86_64","Qemu-system-i386"
                                                       ,"Lxappearance"]
                                        ++ [ stringProperty "_GTK_APPLICATION_ID" =? "io.otsaloma.gaupol" --> doFloat
                                           , match className "minecraft" --> doFloat -- filter out "Minecraft n.m"
                                           , match title "gloss" --> doFloat
                                           , match title "opengl" --> doFloat
                                           , match title "keyman" --> doCenterFloat
                                           , match title "devtools" --> doFloat
                                           , isDialog --> doCenterFloat
                                           , isFullscreen --> doFullFloat
                                           , checkDock --> doLower
                                           ])
                         -- <+> doCenterFloat
                         <+> manageHook def
  --, handleEventHook    = fullscreenEventHook <+> handleEventHook def
  --, workspaces         = map ((" "++) . (++" "). show) [1 .. 9 :: Int]
  , modMask            = mod4Mask
  , keys               = \conf -> M.fromList [ ((modMask conf              , xK_p), spawn $ "j4-dmenu-desktop --no-generic --term=" ++ terminal conf)
                                             , ((modMask conf .|. shiftMask, xK_p), spawn "dmenu_run")
                                             , ((modMask conf              , xK_f), withFocused float)
                                             , ((modMask conf .|. shiftMask, xK_x), xmonadPrompt def { font = "-misc-fixed-medium-r-normal--13-*-*-*-*-*-*-*"
                                                                                                     , searchPredicate = fuzzyMatch
                                                                                                     })
                                             , ((modMask conf .|. shiftMask, xK_q), killOnce
                                                                                    >> M.findWithDefault (return ()) (modMask conf .|. shiftMask, xK_q) (keys def conf))
                                             --, ((modMask conf              , xK_q), killOnce
                                             --                                       >> M.findWithDefault (return ()) (modMask conf, xK_q) (keys def conf))
                                             , ((modMask conf .|. shiftMask, xK_t), sinkAll)
                                             , ((modMask conf, xK_u), spawn "xclip -o 2>/dev/null | xargs chromium")
                                             , ((modMask conf .|. shiftMask, xK_Return), do
                                                   ws <- gets windowset
                                                   let maybeStack = W.stack . W.workspace . W.current $ ws
                                                       maybeFocus = maybe Nothing (pure . W.focus) maybeStack

                                                   title <- maybe mempty (fmap show . getName) maybeFocus

                                                   -- You can also use this to get the title, but `getName` method
                                                   -- is the one being used in xmobar, `title` and `getName` code
                                                   -- are similar enough but who knows.
                                                   --
                                                   -- title' <- maybe mempty (runQuery title) maybeFocus

                                                   app <- maybe mempty (runQuery appName) maybeFocus
                                                   cls <- maybe mempty (runQuery className) maybeFocus

                                                   path <- maybe (pure title) ((<$> io getHomeDirectory) . (flip (++)))
                                                     (stripPrefix "~" title)

                                                   titleIsPath <- io $ doesDirectoryExist path
                                                   if titleIsPath && app == "urxvt"
                                                     then spawn $ terminal conf ++ " -cd '" ++ escapeSingleQuotes path ++ "'"
                                                     else spawn $ terminal conf)
                                             ]
                                  <+> keys def conf
  , borderWidth        = 2
  --, logHook            = catchIO (putStrLn "foo") <+> logHook def
  , startupHook        = spawnOnce (unwords [ "trayer", "-l"
                                            , "--edge", "top"
                                            , "--align", "right"
                                            , "--widthtype", "request"
                                            , "--height", "16"
                                            , "--SetDockType", "true"
                                            , "--SetPartialStrut", "true"
                                            , "--transparent", "true"
                                            , "--alpha", "0"
                                            , "--tint", "0x2e3440"
                                            , "--expand", "true"
                                            ])
                         <+> spawnOnce "hsetroot -fill .xmonad/wall.png"
                         <+> spawnOnce "redshift-gtk"
                         <+> spawnOnce "parcellite"
                         <+> startupHook def
  }
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
