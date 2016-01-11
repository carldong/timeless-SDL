module FRP.Timeless.Framework.Debug where

import System.IO

import Linear (V2)

putBug :: String -> IO ()
putBug s = hPutStr stderr $ "[Bug] " ++ s

putError :: String -> IO ()
putError s = hPutStr stderr $ "[Error] " ++ s

putWarning :: String -> IO ()
putWarning s = hPutStr stderr $ "[Warning] " ++ s
