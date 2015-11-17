module FRP.Timeless.Framework.SDL.Components
  where

import Prelude hiding ((.), id)

import Control.Monad.IO.Class
import Control.Monad.Reader

import FRP.Timeless
import FRP.Timeless.Framework.UI.Scene
import FRP.Timeless.Framework.UI.Components

import Linear (V2)

import qualified SDL as SDL
import SDL (($=))

{-| This function is the renderer of an SDL2 Software rendered button.
 - It draws on the SDL surface with upper-left corner specified. The
 - 'Reader' monad here is used to read font configurations
 -}
sdlLabelRendererSW :: forall r m. (MonadIO m) => V2 Int
                   -> SDL.Surface -- ^ The surface to draw on
                   -> String -- ^ Label
                   -> ReaderT r m ()
sdlLabelRendererSW = undefined
