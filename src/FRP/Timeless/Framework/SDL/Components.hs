module FRP.Timeless.Framework.SDL.Components
  where

import Prelude hiding ((.), id)

import FRP.Timeless
import FRP.Timeless.Framework.UI.Scene
import FRP.Timeless.Framework.UI.Components

import qualified SDL as SDL
import SDL (($=))

--sdlLabelRendererSW :: MonadIO m => Point V2 IntString
--                   -> SDL.Surface -- ^ The surface to draw on
--                   -> String -- ^ Label
--                   -> m ()
