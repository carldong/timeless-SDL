{-# LANGUAGE Arrows #-}
module Main where

--import FRP.Timeless
import FRP.Timeless.Framework.SDL
import qualified SDL as SDL
import qualified SDL.TTF as TTF
import SDL (($=))
import Linear hiding(trace)
import Control.Monad.IO.Class

import Debug.Trace

data TestAPP = TestAPP {
  testImg :: ImgInput SDL.Surface
  , testUI :: SDLUIConfig
  }

fwSession = clockSession_

-- | Updates the window to its surface. Make sure to put this after anything that updates window surface: only data dependency ensures order
sUpdateWindow :: MonadIO m => SDL.Window -> Signal s m a a
sUpdateWindow window = proc a -> do
  mkActM (SDL.updateWindowSurface window) -< ()
  returnA -< a

testImage :: IO (ImgInput SDL.Surface)
testImage = do
  surf <- SDL.loadBMP "test/test.bmp"
  return $ ImgInput surf (V2 0 0)

testLabel :: LabelInput
testLabel = LabelInput "Hello World" (V2 0 100)

sTestContainer :: SDLUIConfig -> ImgInput SDL.Surface -> SDL.Surface -> Container IO
sTestContainer uiconfig img master = Container $ proc input -> do
  (i',_) <- componentBox $ sdlImageComp_SW master -< (input, img)
  (i'',_) <- componentBox $ cTestLabel -< (i', testLabel)
  returnA -< i''
    where
      cTestLabel :: LabelComponent s IO
      cTestLabel = sdlLabelComp_SW uiconfig master

-- | Currently displays an image at (0,0), and a text label at (0, 100)
testFWBox :: TestAPP -> SDL.Surface -> SDL.Window -> Signal s IO () ()
testFWBox app master window = proc _ -> do
  evs <- mkActM SDL.pollEvents -< ()
  quit <- iswitch () <<< arr (any (\(SDL.Event _ e) -> e == SDL.QuitEvent)) -< evs
  sUpdateWindow window <<< containerBox (sTestContainer (testUI app) (testImg app) master) -< UIInput
  returnA -< quit

loadTest :: IO TestAPP
loadTest = do
  let fontSize = 36
      fontFF = 0
  font <- TTF.openFont "test/DejaVuSans.ttf" fontSize
  let uiconfig = SDLUIConfig font fontSize fontFF
  img <- testImage
  return $ TestAPP img (uiconfig)

testApp :: IO ()
testApp = TTF.withInit $ do
  box <- initApp
  runBox fwSession box

initApp :: IO (Signal s IO () ())
initApp = do
  --SDL.initialize [SDL.InitEverything]
  SDL.initializeAll
  window <- SDL.createWindow "SDL Framework" SDL.defaultWindow
  --renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  master <- SDL.getWindowSurface window
  -- SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  --success <- TTF.init
  inited <- TTF.wasInit
  if not inited then error "TTF init failed" else return ()

  app <- loadTest
  return $ testFWBox app master window

main :: IO ()
main = testApp
