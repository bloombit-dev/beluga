module Binja.Utils
  ( toBool,
    ptrToMaybe,
    Colors (..),
    getColors,
  )
where

import Binja.Types
import System.IO (hIsTerminalDevice, stdout)

toBool :: CBool -> Bool
toBool (CBool 0) = False
toBool _ = True

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe p
  | p == nullPtr = Nothing
  | otherwise = Just p

data Colors = Colors
  { red :: String -> String,
    green :: String -> String,
    yellow :: String -> String,
    blue :: String -> String,
    cyan :: String -> String,
    magenta :: String -> String,
    orange :: String -> String
  }

-- |
-- Populate Colors with:
-- * identity function
-- * color coded string
-- dependent on hIsTerminalDevice stdout
getColors :: IO Colors
getColors = do
  isTty <- hIsTerminalDevice stdout
  let esc :: String -> String -> String
      esc color text =
        case isTty of
          True -> "\ESC[" ++ color ++ "m" ++ text ++ "\ESC[0m"
          False -> text
  pure $
    Colors
      { red = esc "38;5;196",
        green = esc "38;5;46",
        yellow = esc "38;5;226",
        blue = esc "38;5;33",
        cyan = esc "38;5;51",
        magenta = esc "38;5;201",
        orange = esc "38;5;208"
      }
