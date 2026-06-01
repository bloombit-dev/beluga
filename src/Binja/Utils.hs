module Binja.Utils
  ( toBool,
    ptrToMaybe,
    red,
    green,
    yellow,
    blue,
    cyan,
    magenta,
    orange,
  )
where

import Binja.Types
import System.IO (hIsTerminalDevice, stdout)
import System.IO.Unsafe (unsafePerformIO)

toBool :: CBool -> Bool
toBool (CBool 0) = False
toBool _ = True

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe p
  | p == nullPtr = Nothing
  | otherwise = Just p

{-# NOINLINE esc #-}
esc :: String -> String -> String
esc code s = unsafePerformIO $ do
  isTty <- hIsTerminalDevice stdout
  case isTty of
    True -> pure $ "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m"
    False -> pure s

red, green, yellow, blue, cyan, magenta, orange :: String -> String
red = esc "38;5;196"
green = esc "38;5;46"
yellow = esc "38;5;226"
blue = esc "38;5;33"
cyan = esc "38;5;51"
magenta = esc "38;5;201"
orange = esc "38;5;208"
