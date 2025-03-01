{-# LANGUAGE OverloadedStrings #-}

module Main where

import Termonad
import Termonad.Prelude ( void, readMVar, fromMaybe )
import Termonad.Config.Colour

import Termonad.Types
import Termonad.Startup ( defaultMain )
import GI.Gtk ( windowSetTitle )
import GI.Vte
  ( Terminal
  , onTerminalWindowTitleChanged
  , terminalGetWindowTitle
  )
import Control.Lens ( (^.) )

import Data.Text ( unpack )

printConfigOptions :: ConfigOptions -> IO ()
printConfigOptions = f
  where
    f (ConfigOptions
        fontConfig
        showScrollbar
        scrollbackLen
        confirmExit
        wordCharExceptions
        showMenu
        showTabBar
        cursorBlinkMode
        boldIsBright
        enableSixel
        allowBold) =
      putStrLn $ unlines
        [ "ConfigOptions { fontConfig = " ++ g fontConfig
        , "              , showScrollbar = " ++ show showScrollbar
        , "              , scrollbackLen = " ++ show scrollbackLen
        , "              , confirmExit = " ++ show confirmExit
        , "              , wordCharExceptions = \"" ++ unpack wordCharExceptions ++ "\""
        , "              , showMenu = " ++ show showMenu
        , "              , showTabBar = " ++ show showTabBar
        , "              , cursorBlinkMode = " ++ show cursorBlinkMode
        , "              , boldIsBright = " ++ show boldIsBright
        , "              , enableSixel = " ++ show enableSixel
        , "              , allowBold = " ++ show allowBold
        , "              }"]

    g (FontConfig fontFamily fontSize) =
      init $ unlines
        [ "FontConfig { fontFamily = \"" ++ unpack fontFamily ++ "\""
        , "                                        , fontSize = " ++ show fontSize
        , "                                        }"]

createColourHex :: String -> AlphaColour Double
createColourHex hex
  | head hex == '#' = g $ f (tail hex)
  | otherwise       = g $ f hex
  where
    f [] = []
    f (h:g:hex) = read ("0x" ++ [h,g]) : f hex

    g rgb = createColour (rgb !! 0) (rgb !! 1) (rgb !! 2)

nord :: ColourConfig (AlphaColour Double)
nord =
  defaultColourConfig
    { cursorFgColour = Set $ createColourHex "#2e3440"
    , cursorBgColour = Set $ createColourHex "#d8dee9"
    , foregroundColour = Set $ createColourHex "#d8dee9"
    , backgroundColour = Set $ createColourHex "#2e3440"
    , highlightBgColour = Set $ createColourHex "#4c566a"
    , palette = ExtendedPalette standardColours lightColours
    }
  where
    standardColours :: List8 (AlphaColour Double)
    standardColours = maybe defaultStandardColours id $ mkList8
      [ createColourHex "#3b4252" -- 0
      , createColourHex "#bf616a" -- 1
      , createColourHex "#a3be8c" -- 2
      , createColourHex "#ebcb8b" -- 3
      , createColourHex "#81a1c1" -- 4
      , createColourHex "#b48ead" -- 5
      , createColourHex "#88c0d0" -- 6
      , createColourHex "#e5e9f0" -- 7
      ]

    lightColours :: List8 (AlphaColour Double)
    lightColours = maybe defaultLightColours id $ mkList8
      [ createColourHex "#4c566a" --  8
      , createColourHex "#bf616a" --  9
      , createColourHex "#a3be8c" -- 10
      , createColourHex "#ebcb8b" -- 11
      , createColourHex "#81a1c1" -- 12
      , createColourHex "#b48ead" -- 13
      , createColourHex "#8fbcbb" -- 14
      , createColourHex "#eceff4" -- 15
      ]

configOptions :: ConfigOptions
configOptions =
  defaultConfigOptions
    { fontConfig = defaultFontConfig { fontFamily = "Fira Code"
                                     , fontSize = FontSizePoints 10 }
    , showScrollbar = ShowScrollbarNever
    , scrollbackLen = 100000
    , confirmExit = False
    , showMenu = False
    , cursorBlinkMode = CursorBlinkModeOff }

-- -- Taken from https://github.com/cdepillabout/termonad/issues/186
-- customCreateTermHook :: TMState -> Terminal -> IO ()
-- customCreateTermHook tmState vteTerm = do
--   void $ onTerminalWindowTitleChanged vteTerm $ do
--     maybeTitle <- terminalGetWindowTitle vteTerm
--     let title = fromMaybe "shell" maybeTitle

--     tmState <- readMVar tmState
--     let win = tmState ^. lensTMStateAppWin

--     windowSetTitle win title

config :: TMConfig
config = TMConfig
  configOptions
  defaultConfigHooks -- (ConfigHooks customCreateTermHook)

main :: IO ()
main = do
  defaultMain . addColourExtension config =<< createColourExtension nord
