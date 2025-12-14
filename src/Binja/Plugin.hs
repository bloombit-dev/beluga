module Binja.Plugin
  ( initPlugins,
    getInstallDirectory,
    getBundledPluginDirectory,
    getUserDirectory,
    getRepositoriesDirectory,
    getUserPluginDirectory,
  )
where

import Binja.FFI
import Binja.Types
import Binja.Utils
import Data.Bool (bool)

initPlugins :: Bool -> IO Bool
initPlugins = fmap toBool . c_BNInitPlugins . bool (CBool 0) (CBool 1)

getInstallDirectory :: IO String
getInstallDirectory = peekCString =<< c_BNGetInstallDirectory

getBundledPluginDirectory :: IO String
getBundledPluginDirectory = peekCString =<< c_BNGetBundledPluginDirectory

getUserDirectory :: IO String
getUserDirectory = peekCString =<< c_BNGetUserDirectory

getUserPluginDirectory :: IO String
getUserPluginDirectory = peekCString =<< c_BNGetUserPluginDirectory

getRepositoriesDirectory :: IO String
getRepositoriesDirectory = peekCString =<< c_BNGetRepositoriesDirectory
