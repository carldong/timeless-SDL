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

import Linear.V2

import FRP.Timeless
import FRP.Timeless.Framework.UI.Events

{-| A Container is just a 'Signal' that takes UI input and streams it
 through performing side effects.
 * Input: UIInput
 * Output: UIInput to be passed
 -}
newtype Monad m => Container m = Container {
    containerBox :: forall s. Monad m => Signal s m UIInput UIInput
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

-- | A 'Renderable' is a rendering function
newtype Monad m => Renderable m a = Renderable {
  execRenderable :: a -> m ()
  }

-- NOTE: Component is probably NOT an Arrow.
-- instance Monad m => Arrow (Component s m) where
--   arr = Component . arr . second
--   first f =

-- | A basic data structure for arbitrary image data input
data ImgInput a = ImgInput {
    imgData :: a
  , imgPos  :: V2 Integer
  }

-- | An 'Image' renders an image
type Image m a = Renderable m (ImgInput a)
-- | The 'Image' 'Component'
type ImageComponent s m a = Component s m (ImgInput a) ()

-- | Create a 'Renderable' 'Component' for a framework
mkRenderable :: Monad m => Renderable m a -> Component s m a ()
mkRenderable rdr = Component $ second . mkKleisli_ $ execRenderable rdr

-- | Creates an 'Image' 'Component' for a framework
mkImgComp :: (Monad m) => Image m a -> ImageComponent s m a
mkImgComp = mkRenderable

-- | The data structure for a label
data LabelInput = LabelInput {
    lblString :: String
  , lblPos :: V2 Integer
  }

-- | A Label is a 'Component' which renders a 'String'
-- newtype Label m = Label {
--     lblRender :: Monad m => LabelInput -> m ()
--     }
type Label m = Renderable m LabelInput

type LabelComponent s m = Component s m LabelInput ()

-- | Create a 'Label' 'Component' for a framework
mkLblComp :: Monad m => Label m -> Component s m LabelInput ()
mkLblComp = mkRenderable

{-
  TODO: Test this: Make the button with a "label" arrow and a logic arrow.
-}

-- | Button input includes a string of label and the focus
type ButtonInput = (String, Bool)
-- | Retrieve label from button input
btnLabel :: ButtonInput -> String
btnLabel = fst
-- | Retrieve focus status from button input
btnFocus :: ButtonInput -> Bool
btnFocus = snd

-- | Button output is whether it is pressed
type ButtonOutput = Bool

{-| A Button is a stateful 'Component' which renders a label, checks
 - whether it is focused, and outputs a 'Bool' indicating whether it is
 - currently pressed

 * Input: (Label, Focused)
 * Output: (IsPressed)
 -}
mkButton :: (Monad m) => Maybe String -> Component s m ButtonInput ButtonOutput
mkButton mLbl = undefined

-- {-| A Button is a stateful 'Component' which renders a label, checks
--  - whether it is focused, and outputs a 'Bool' indicating whether it is
--  - currently pressed
--
--  * Input: (Label, Focused)
--  * Output: (IsPressed)
--  -}
-- data Button m = Button
--               {
--                 -- | Label -> (Focused, Pressed) -> Render
--                 btnRender :: Monad m => String -> (Bool, Bool) -> m ()
--                 -- | Pressed -> (Focused, input) -> Pressed'
--               , btnParse :: Bool -> (Bool, UIInput) -> Bool
--                 -- | Input -> (Focused, Pressed) -> Input'
--               , btnStream :: UIInput -> (Bool, Bool) -> UIInput
--               }
--
-- -- | Create a 'Component' from a 'Button'
-- mkBtn :: Monad m => Button m -> Component s m (String, Bool) Bool
-- mkBtn b =
--     let renderer = btnRender b
--         parser = btnParse b
--         stream = btnStream b
--     in Component $ proc (input, (lbl, focused)) -> do
--         pressed <- mkSW_ False parser -< (focused, input)
--         input' <- arr (uncurry stream) -< (input, (focused, pressed))
--         mkKleisli_ (uncurry renderer) -< (lbl, (focused, pressed))
--         returnA -< (input', pressed)

--{-| A TextField is a 'Component' which changes contents according to an
-- - input string, updates according to UI input when this input is
-- - inhibited and the textfield itself isfocused, and outputs the current
-- - content
--
-- * Input: (Forced Content, Focused)
-- * Output: (Content)
-- -}
--data TextField m = TextField
--                   {
--                     -- | Content -> Focused -> Render
--                     tfRender :: Monad m => String -> Bool -> m ()
--                     -- | Fixme: Forgot inhibition here
--                   , tfParse :: String -> (Bool, UIInput) -> String
--                   , tfStream :: String -> (Bool, UIInput) -> UIInput
--                   }
--
--{-| Creates a 'Component' from a 'TextField'
-- -}
--mkTF :: Monad m => TextField m -> Component s m (String, Bool) String
--mkTF tf =
--    let renderer = tfRender tf
--        parser = tfParse tf
--        stream = tfStream tf
--    in Component $ proc (input, (txt, focused)) -> do
--        txt' <- mkSW_ "" parser -< (focused, input)
--        input' <- arr (uncurry stream) -< (txt', (focused, input))
--        mkKleisli_ (uncurry renderer) -< (txt', focused)
--        returnA -< (input', txt')
--
