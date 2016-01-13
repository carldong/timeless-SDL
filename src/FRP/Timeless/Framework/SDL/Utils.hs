module FRP.Timeless.Framework.SDL.Utils where

import qualified SDL as SDL

import Foreign.C.Types (CInt)

import Linear.V2
import Linear.Affine (Point(..))

makeCIntP :: (Integral n) => n -> n -> Point V2 CInt
makeCIntP w h = toCInt <$> (P $ V2 w h)

toCInt :: Integral n => n -> CInt
toCInt = fromIntegral
