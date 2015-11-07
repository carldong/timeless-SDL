{-|
Module:     FRP.Timeless.Framework.UI.Components
Copyright:  (c) 2015 Rongcui Dong

License:    BSD3
Maintainer: Rongcui Dong <karl_1702@188.com>
-}

module FRP.Timeless.Framework.UI.Components
       where

import FRP.Timeless
import FRP.Timeless.Framework.UI.Events

newtype Container = Container {
  containerBox :: forall m s. Monad m => Signal s m UIInput UIInput
  }

newtype Component a b = Component {
  componentBox :: forall m s. Monad m => Signal s m (UIInput,a) (UIInput,b)
  }

type Label = Component String ()

type Button = Component String Bool

type TextField = Component String String

