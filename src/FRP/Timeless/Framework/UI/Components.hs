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

-- | An 'ImageRender' renders an image
type ImageRender m a = Renderable m (ImgInput a)
-- | The 'Image' 'Component' for a framework
type ImageComponent s m a = Component s m (ImgInput a) ()

-- | Create a 'Renderable' 'Component' for a framework
mkFWRenderable :: Monad m => Renderable m a -> Component s m a ()
mkFWRenderable rdr = Component $ second . mkKleisli_ $ execRenderable rdr

-- | Creates an 'Image' 'Component' for a framework
mkFWImgComp :: (Monad m) => ImageRender m a -> ImageComponent s m a
mkFWImgComp = mkFWRenderable

-- | The data structure for a label
data LabelInput = LabelInput {
    lblString :: String
  , lblPos :: V2 Integer
  }

-- | Rendering function for a label
type LabelRender m = Renderable m LabelInput
-- | The 'Component' for a label
type LabelComponent s m = Component s m LabelInput ()

-- | Create a 'LabelComponent' for a framework
mkFWLblComp :: Monad m => LabelRender m -> LabelComponent s m
mkFWLblComp = mkFWRenderable

{-
  TODO: Implement Menu, and MenuEntry
 -}

{-|
  The data structure for input of Menu
-}
data MenuInput = MenuInput {
  menuPos :: V2 Integer
  , menuFocused :: Bool
  }

{-|
  The data structure for output of Menu.

  menuEntrySelected is an impulse output which activates only when user presses the entry
-}
data MenuOutput = MenuOutput {
  menuEntriesCount :: Integer
  , menuEntrySelected :: Integer
  }

-- | Rendering function for a menu
type MenuRender m = Renderable m MenuInput
-- | The 'Component' for a menu
type MenuComponent s m = Component s m MenuInput MenuOutput

-- | Create a 'MenuComponent' for a framework
mkFWMenuComp :: Monad m => MenuRender m -> MenuComponent s m
mkFWMenuComp = undefined

