{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import XMonad

import XMonad.Hooks.DynamicLog hiding ( xmobar )
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier

import XMonad.Prompt
import XMonad.Prompt.XMonad

import XMonad.Hooks.EwmhDesktops hiding ( ewmh, ewmhDesktopsStartup )
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W

import XMonad.Layout.NoBorders

import XMonad.Hooks.ManageHelpers

import XMonad.Util.SpawnOnce

import qualified Data.Map as M

import XMonad.Actions.WithAll

import qualified XMonad.Util.ExtensibleState as XS
import System.Posix.Signals
import System.Posix.Types
import Control.Concurrent ( threadDelay )

-- Remember the spawned PIDs.
data SpawnedPIDs = SpawnedPIDs [ProcessID] deriving ( Typeable, Read, Show )

instance ExtensionClass SpawnedPIDs where
  initialValue = SpawnedPIDs []
  extensionType = PersistentExtension

fromSpawnedPIDs :: SpawnedPIDs -> [ProcessID]
fromSpawnedPIDs (SpawnedPIDs pids) = pids

spawnState :: String -> X ()
spawnState cmd = do
  pid <- spawnPID cmd
  pids <- fromSpawnedPIDs <$> XS.get
  XS.put (SpawnedPIDs $ pids ++ [pid])

killPIDs :: X ()
killPIDs = do
    pids <- fromSpawnedPIDs <$> XS.get
    io $ mapM_ kill pids
    XS.put (SpawnedPIDs [])
  where
    kill pid = do
      signalProcess sigTERM pid
      --threadDelay 50000

ewmh :: XConfig a -> XConfig a
ewmh c = c { startupHook     = startupHook c +++ ewmhDesktopsStartup
           , handleEventHook = handleEventHook c +++ ewmhDesktopsEventHook
           , logHook         = logHook c +++ ewmhDesktopsLogHook }
 where x +++ y = mappend y x

ewmhDesktopsStartup :: X ()
ewmhDesktopsStartup = setSupported

-- Add _NET_WM_STATE_FULLSCREEN to _NET_SUPPORTED to allow
-- programs that don't support _NET_WM_STATE protocol
-- go fullscreen.
setSupported :: X ()
setSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
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
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

    setWMName "xmonad"

xmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar conf = statusBar "xmobar"
              xmobarPP { ppTitle = xmobarColor "green"  "" . shorten 80
                       }
              toggleStrutsKey
              conf

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

-- | Lower an unmanaged window. Useful together with 'doIgnore' to lower
-- special windows that for some reason don't do it themselves.
doLower :: ManageHook
doLower = ask >>= \w -> liftX $ withDisplay $ \dpy -> io (lowerWindow dpy w) >> mempty

main :: IO ()
main = xmonad =<< (xmobar . ewmh) def
  { normalBorderColor  = "#a6a6a6"
  , focusedBorderColor = "#e5e9f0"
  , terminal           = "termite"
  , layoutHook         = lessBorders OnlyScreenFloat tiled ||| Mirror tiled ||| Full
  , manageHook         = composeAll [ className =? "mpv" --> doFloat
                                    , isDialog --> doCenterFloat
                                    , isFullscreen --> doFullFloat
                                    , checkDock --> doLower
                                    ]
                         -- <+> doCenterFloat
                         <+> manageHook def
  , handleEventHook    = fullscreenEventHook <+> handleEventHook def
  --, workspaces         = map ((" "++) . (++" "). show) [1 .. 9 :: Int]
  , modMask            = mod4Mask
  , keys               = \conf -> M.fromList [ ((modMask conf              , xK_p), spawn $ "j4-dmenu-desktop --no-generic --term=" ++ terminal conf)
                                             , ((modMask conf .|. shiftMask, xK_p), spawn "dmenu_run")
                                             , ((modMask conf              , xK_f), withFocused float)
                                             , ((modMask conf .|. shiftMask, xK_x), xmonadPrompt def { font = "xft:Terminus:size=8" })
                                             --, ((modMask conf              , xK_q), spawn $ unwords [ "if type xmonad; then"
                                             --                                                       ,   "if xmonad --recompile; then"
                                             --                                                       ,     "ps=$(ps -e -o pid,comm);"
                                             --                                                       ,     "pids=$(echo \"$ps\" | awk '$2 == \"trayer\" { print $1 }');"
                                             --                                                       ,     "kill -9 $pids;"
                                             --                                                       ,     "xmonad --restart;"
                                             --                                                       ,   "fi;"
                                             --                                                       , "else xmessage xmonad not in \\$PATH: \"$PATH\";"
                                             --                                                       , "fi"
                                             --                                                       ])
                                             , ((modMask conf              , xK_q), killPIDs
                                                                                    >> M.findWithDefault (return ()) (modMask conf, xK_q) (keys def conf))
                                             , ((modMask conf .|. shiftMask, xK_t), sinkAll)
                                             ]
                                  <+> keys def conf
  , borderWidth        = 2
  --, logHook            = catchIO (putStrLn "foo") <+> logHook def
  , startupHook        = spawnState (unwords [ "trayer", "-l"
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
                         <+> spawnState "parcellite"
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
