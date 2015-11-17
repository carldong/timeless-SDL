{-|
Module:     FRP.Timeless.Framework.UI.Components
Copyright:  (c) 2015 Rongcui Dong

License:    BSD3
Maintainer: Rongcui Dong <karl_1702@188.com>
-}

module FRP.Timeless.Framework.UI.Components
       where

import Prelude hiding ((.), id)

import Control.Monad.Fix

import FRP.Timeless
import FRP.Timeless.Framework.UI.Events

newtype Container = Container {
    containerBox :: forall m s. Monad m => Signal s m UIInput UIInput
    }

newtype Monad m => Component s m a b = Component {
    componentBox :: Signal s m (UIInput, a) (UIInput, b)
    }

instance Monad m => Category (Component s m) where
    id = Component SId
    c2 . c1 = Component $ (componentBox c2) . (componentBox c1)


data Button m = Button
              {
                btnRender :: Monad m =>
                             String -- ^ Button label
                          -> Bool -- ^ If button is pressed
                          -> m () -- ^ Render Action
              , btnParse :: Bool -- ^ Previously pressed or not
                         -> (Bool, UIInput) -- ^ (Focused, Input)
                         -> Bool -- ^ If button is pressed
              , btnStream :: Bool -- ^ If button is pressed
                          -> (Bool, UIInput) -- ^ (Focused, Input)
                          -> UIInput -- ^ Manipulated input
              }

mkBtn :: Monad m => Button m -> Component s m a b
mkBtn = undefined

data TextField m = TextField
                   {
                     tfRender :: Monad m =>
                                 String -- ^ Content
                              -> Bool -- ^ Focused or not
                              -> m ()
                   , tfParse :: String -- ^ Previouis Content
                             -> (Bool, UIInput) -- ^ (Focused, input)
                             -> String
                   , tfStream :: String -- ^ Content
                              -> (Bool, UIInput) -- ^ (Focused, Input)n
                              -> UIInput
                   }

mkTF :: Monad m => TextField m -> Component s m a b
mkTF = undefined

