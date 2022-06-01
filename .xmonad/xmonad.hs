-- Written for xmonad-0.17.0 and xmonad-contrib-0.17.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}

import XMonad

import XMonad.Hooks.DynamicLog hiding (xmobar)
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier

import XMonad.Prompt
import XMonad.Prompt.XMonad

import qualified XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.EwmhDesktops hiding (ewmh,
                                         ewmhFullscreen,
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

ewmh :: XConfig a -> XConfig a
ewmh c = (XMonad.Hooks.EwmhDesktops.ewmh c)
  { startupHook = ewmhDesktopsStartup <+> startupHook c
  }

ewmhFullscreen :: XConfig a -> XConfig a
ewmhFullscreen c = (XMonad.Hooks.EwmhDesktops.ewmhFullscreen c)
  { handleEventHook = handleEventHook c <+> fullscreenEventHook
  }

ewmhDesktopsStartup :: X ()
ewmhDesktopsStartup = setSupported

-- Add _NET_WM_STATE_FULLSCREEN to _NET_SUPPORTED to allow
-- programs that don't support _NET_WM_STATE protocol
-- go fullscreen.
setSupported :: X ()
setSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_DEMANDS_ATTENTION"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ,"_NET_WM_STATE_FULLSCREEN"
                         ]
    io $ changeProperty32 dpy r a aTOM propModeReplace (fmap fromIntegral supp)

    setWMName "xmonad"

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

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

main :: IO ()
main = (xmonad =<<) . xmobar . ewmhFullscreen . ewmh $ def
  { normalBorderColor  = "#a6a6a6"
  , focusedBorderColor = "#e5e9f0"
  , terminal           = "termonad"
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
                                             , ((modMask conf .|. shiftMask, xK_x), xmonadPrompt def { font = "xft:Terminus:size=8" })
                                             , ((modMask conf .|. shiftMask, xK_q), killOnce
                                                                                    >> M.findWithDefault (return ()) (modMask conf .|. shiftMask, xK_q) (keys def conf))
                                             --, ((modMask conf              , xK_q), killOnce
                                             --                                       >> M.findWithDefault (return ()) (modMask conf, xK_q) (keys def conf))
                                             , ((modMask conf .|. shiftMask, xK_t), sinkAll)
                                             , ((modMask conf, xK_u), spawn "xclip -o 2>/dev/null | xargs chromium")
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
