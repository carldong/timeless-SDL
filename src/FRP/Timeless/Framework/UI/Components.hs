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
              , btnParse :: (Bool,Bool) -- ^ Previously (pressed,focused) or not
                         -> UIInput -- ^ Input from UI
                         -> (Bool,Bool) -- ^ If button is (pressed,focused)
              , btnStream :: Bool -- ^ If button is pressed
                          -> UIInput -- ^ In-input stream
                          -> UIInput -- ^ Out-input stream
              }
instance Monad m => Component (Button m) m (String,Bool) Bool where
  -- | Renders input string as button label, checks if focused from
  -- input Bool, outputs whether button is pressed
  componentBox b =
    let renderer = btnRender b
        parser = btnParse b
        stream = btnStream b
    in proc (i, (lbl,focused)) -> do
      (pressed, _) <- mkSW_ (False,False) parser -< i
      i' <- arr (uncurry stream) -< (pressed, i)
      mkKleisli_ $ uncurry renderer -< (lbl, pressed)
      returnA -< (i', pressed)


