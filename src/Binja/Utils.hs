module Binja.Utils
  ( toBool,
    ptrToMaybe,
  )
where

import Binja.Types

toBool :: CBool -> Bool
toBool (CBool 0) = False
toBool _ = True

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe p
  | p == nullPtr = Nothing
  | otherwise = Just p
