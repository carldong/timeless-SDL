module FRP.Timeless.Framework.SDL.Components
  where

import Prelude hiding ((.), id)

import Control.Monad.IO.Class
import Control.Monad.Reader

import FRP.Timeless
import FRP.Timeless.Framework.UI.Scene
import FRP.Timeless.Framework.UI.Components
import FRP.Timeless.Framework.SDL.Utils

import FRP.Timeless.Framework.Debug

import Linear.V2
import Linear.Affine (Point(..))

import qualified SDL as SDL
import SDL.Raw.Types (Color(..))
import qualified SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
import SDL (($=))

data SDLUIConfig = SDLUIConfig {
    labelFont :: TTFFont
  , labelFontPTSize :: Int
  , labelFontFF :: Int -- ^ Font face index
  }

{-| This function is the renderer of an SDL2 Software rendered label.
 - It draws on the SDL surface with upper-left corner specified.
 -
 - @TODO: Somehow TTF is initialized but not passes the `wasInit`. Will fix it
 -}
sdlLabelRendererSW :: (MonadIO m) => SDLUIConfig
                   -> SDL.Surface -- ^ The surface to draw on
                   -> LabelInput
                   -> m ()
sdlLabelRendererSW config master (LabelInput txt pos) = liftIO $ do
  -- config <- ask
  inited <- TTF.wasInit
  -- if not inited then error "Font system not initialized! " else return ()
  -- v Testing, White
  surf' <- TTF.renderUTF8Solid (labelFont config) txt (Color 255 255 255 255)
  SDL.surfaceBlit surf' Nothing master (Just $ P $ toCInt <$> pos)
  SDL.freeSurface surf'

{-| This function creates a Label component
-}
sdlLabelComp_SW
  :: MonadIO m => SDLUIConfig
  -> SDL.Surface
  -> LabelComponent s m
sdlLabelComp_SW config master =
  mkLblComp $ Renderable $ sdlLabelRendererSW config master


{-| This function renders an image onto the given surface as is.
-}
sdlImageRendererSW :: (MonadIO m) => SDL.Surface
                   -> ImgInput SDL.Surface
                   -> m ()
sdlImageRendererSW dest img = liftIO $ do
  let src = imgData img
      pos = P $ imgPos img
  SDL.surfaceBlit src Nothing dest (Just $ toCInt <$> pos)

{-| This function creates an Image component
-}
sdlImageComp_SW :: MonadIO m => SDL.Surface -> ImageComponent s m SDL.Surface
sdlImageComp_SW master = mkImgComp $ Renderable $ sdlImageRendererSW master

