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

{-| A Container is just a 'Signal' that takes UI input and streams it
 through performing side effects.
 * Input: UIInput
 * Output: UIInput to be passed
 -}
newtype Container = Container {
    containerBox :: forall m s. Monad m => Signal s m UIInput UIInput
    }

{-| A Component is a 'Signal' that takes in some UI input, streams it,
 - and has some extra input/outputs
 -}
newtype Monad m => Component s m a b = Component {
    componentBox :: Signal s m (UIInput, a) (UIInput, b)
    }

-- | This makes threading 'Component's less verbose
instance Monad m => Category (Component s m) where
    id = Component SId
    c2 . c1 = Component $ (componentBox c2) . (componentBox c1)

-- | A Label is a 'Component' which renders a 'String'
newtype Label m = Label {
    lblRender :: Monad m => String -> m ()
    }

-- | Create a 'Component' from a 'Label'
mkLbl :: Monad m => Label m -> Component s m String ()
mkLbl l =
  let renderer = lblRender l
   in Component $ second $ mkKleisli_ renderer

{-| A Button is a stateful 'Component' which renders a label, checks
 - whether it is focused, and outputs a 'Bool' indicating whether it is
 - currently pressed

 * Input: (Label, Focused)
 * Output: (IsPressed)
 -}
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

-- | Create a 'Component' from a 'Button'
mkBtn :: Monad m => Button m -> Component s m (String, Bool) Bool
mkBtn b =
    let renderer = btnRender b
        parser = btnParse b
        stream = btnStream b
    in Component $ proc (input, (lbl, focused)) -> do
        pressed <- mkSW_ False parser -< (focused, input)
        input' <- arr (uncurry stream) -< (pressed, (focused, input))
        mkKleisli_ (uncurry renderer) -< (lbl, pressed)
        returnA -< (input', pressed)

{-| A TextField is a 'Component' which changes contents according to an
 - input string, updates according to UI input when this input is
 - inhibited and the textfield itself isfocused, and outputs the current
 - content

 * Input: (Forced Content, Focused)
 * Output: (Content)
 -}
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
                              -> (Bool, UIInput) -- ^ (Focused, Input)
                              -> UIInput
                   }

{-| Creates a 'Component' from a 'TextField'
 -}
mkTF :: Monad m => TextField m -> Component s m (String, Bool) String
mkTF tf =
    let renderer = tfRender tf
        parser = tfParse tf
        stream = tfStream tf
    in Component $ proc (input, (txt, focused)) -> do
        txt' <- mkSW_ "" parser -< (focused, input)
        input' <- arr (uncurry stream) -< (txt', (focused, input))
        mkKleisli_ (uncurry renderer) -< (txt', focused)
        returnA -< (input', txt')

