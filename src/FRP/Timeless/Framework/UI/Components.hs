{-|
Module:     FRP.Timeless.Framework.UI.Components
Copyright:  (c) 2015 Rongcui Dong

License:    BSD3
Maintainer: Rongcui Dong <karl_1702@188.com>
-}

module FRP.Timeless.Framework.UI.Components
       where

import Control.Monad.Fix

import FRP.Timeless
import FRP.Timeless.Framework.UI.Events

newtype Container = Container {
  containerBox :: forall m s. Monad m => Signal s m UIInput UIInput
  }

-- newtype Component a b = Component {
--   componentBox :: forall m s. Monad m => Signal s m (UIInput,a) (UIInput,b)
--   }

class Component c m a b where
  componentBox :: forall s.
                  c -> Signal s m (UIInput, a) (UIInput, b)

data Label = Label {renderLabel :: forall m. Monad m => String -> m ()}
instance Monad m => Component Label m String () where
  -- | Just renders the string, and passes input transparently
  componentBox l =
    let renderer = renderLabel l in
    second (mkKleisli_ renderer)

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
instance Monad m => Component (Button m) m (String,Bool) Bool where
  -- | Renders input string as button label, checks if focused from
  -- input Bool, outputs whether button is pressed
  componentBox b =
    let renderer = btnRender b
        parser = btnParse b
        stream = btnStream b
    in proc (i, (lbl,focused)) -> do
      pressed <- mkSW_ False parser -< (focused, i)
      i' <- arr (uncurry stream) -< (pressed, (focused, i))
      mkKleisli_ $ uncurry renderer -< (lbl, pressed)
      returnA -< (i', pressed)

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
instance Monad m => Component (TextField m) m (String,Bool) String where
  componentBox tf =
    let renderer = tfRender tf
        parser = tfParse tf
        stream = tfStream tf
    in proc (i, (txt,focused)) -> do
      txt' <- mkSW_ "" parser -< (focused, i)
      i' <- arr (uncurry stream) -< (txt', (focused, i))
      mkKleisli_ $ uncurry renderer -< (txt', focused)
      returnA -< (i', txt')
