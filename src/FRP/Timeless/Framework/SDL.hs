{-|
Module:     FRP.Timeless.Framework.SDL
Copyright:  (c) 2015 Rongcui Dong

License:    BSD3
Maintainer: Rongcui Dong <karl_1702@188.com>
-}

module FRP.Timeless.Framework.SDL
    where

import Prelude hiding ((.), id)

import FRP.Timeless
import FRP.Timeless.Framework.UI.Scene
import FRP.Timeless.Framework.UI.Components

import qualified SDL as SDL
import SDL (($=))
import Linear

fwSession = clockSession_

testFWBox :: SDL.Renderer -> Signal s IO () ()
testFWBox = undefined

initApp :: IO (Signal s IO () ())
initApp = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "SDL Framework" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  return $ testFWBox renderer

testApp :: IO ()
testApp = do
  box <- initApp
  runBox fwSession box

