{-|
Module:     FRP.Timeless.Framework.SDL
Copyright:  (c) 2015 Rongcui Dong

License:    BSD3
Maintainer: Rongcui Dong <karl_1702@188.com>
-}

module FRP.Timeless.Framework.SDL
    where

import Prelude hiding ((.), id)

import Control.Monad.IO.Class

import FRP.Timeless
import FRP.Timeless.Framework.UI.Scene
import FRP.Timeless.Framework.UI.Components
import FRP.Timeless.Framework.UI.Events
import FRP.Timeless.Framework.SDL.Components

import qualified SDL as SDL
import qualified SDL.TTF as TTF
import SDL (($=))
import Linear

data TestAPP = TestAPP {
  testImg :: ImgInput SDL.Surface
  }

fwSession = clockSession_

-- | Updates the window to its surface. Make sure to put this after anything that updates window surface: only data dependency ensures order
sUpdateWindow :: MonadIO m => SDL.Window -> Signal s m a a
sUpdateWindow window = proc a -> do
  mkActM (SDL.updateWindowSurface window) -< ()
  returnA -< a

sTestContainer :: ImgInput SDL.Surface -> SDL.Surface -> Container IO
sTestContainer img master = Container $ proc input -> do
  componentBox $ sdlImageC_SW master -< (input, img)
  returnA -< input

-- | Currently displays an image at (0,0)
testFWBox :: TestAPP -> SDL.Surface -> SDL.Window -> Signal s IO () ()
testFWBox app master window = proc _ -> do
  sUpdateWindow window <<< containerBox (sTestContainer (testImg app) master) -< UIInput
  returnA -< ()

loadTest :: IO TestAPP
loadTest = do
  let imgName = "test/test.bmp"
  surf <- SDL.loadBMP imgName
  return $ TestAPP $ ImgInput surf (V2 0 0)

initApp :: IO (Signal s IO () ())
initApp = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "SDL Framework" SDL.defaultWindow
  --renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  master <- SDL.getWindowSurface window
  -- SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend

  app <- loadTest
  return $ testFWBox app master window

testApp :: IO ()
testApp = TTF.withInit $ do
  box <- initApp
  runBox fwSession box

