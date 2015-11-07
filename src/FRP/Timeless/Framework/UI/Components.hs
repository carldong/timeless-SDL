{-|
Module:     FRP.Timeless.Framework.UI.Components
Copyright:  (c) 2015 Rongcui Dong

License:    BSD3
Maintainer: Rongcui Dong <karl_1702@188.com>
-}

module FRP.Timeless.Framework.UI.Components
       where

import FRP.Timeless

newtype Component = Component {
  componentBox :: Monad m => Signal s m (UIInput,a) (UIInput,b)
  }
