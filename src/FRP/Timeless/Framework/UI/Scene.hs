{-| 
Module:     FRP.Timeless.Framework.UI.Scene
Copyright:  (c) 2015 Rongcui Dong

License:    BSD3
Maintainer: Rongcui Dong <karl_1702@188.com>
-}

module FRP.Timeless.Framework.UI.Scene
       where

import FRP.Timeless
import FRP.Timeless.Framework.UI.Events

newtype Scene = Scene {
  sceneBox :: forall m s. Monad m => Signal s m UIInput ()
  }
