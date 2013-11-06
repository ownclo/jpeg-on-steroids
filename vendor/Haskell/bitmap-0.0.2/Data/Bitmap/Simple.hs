
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.Simple
-- Version     : 0.0.2
-- License     : BSD3
-- Copyright   : (c) 2009-2010 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI and CPP
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

-- | This is the same as the pure API, without the annoying alignment stuff. 
-- All functions use the default alignment.

{-# LANGUAGE CPP #-}
module Data.Bitmap.Simple 
  ( 
    module Data.Bitmap.Base
    
    -- * Creating bitmaps
  , emptyBitmap
  , emptyCloneBitmap
  , createSingleChannelBitmap
    -- * Using bitmaps
  , withBitmap
    -- * Mapping over bitmaps
  , componentMap
  , componentMap'
    -- * Cropping and extending
  , copySubImage
  , copySubImage'  
    -- * Flipping and mirroring
  , flipBitmap
  , mirrorBitmap
    -- * Casting
  , castBitmap
  -- , castChannel
    -- * Manipulating channels
  , combineChannels 
  , extractChannels 
  , extractSingleChannel 
    -- * Bilinear resampling
  , bilinearResample
  , bilinearResampleChannel  
    -- * Blending
  , blendBitmaps  
  , blendChannels  
    -- * Gamma correction
  , powerlawGammaCorrection
  , powerlawGammaCorrectionChannel   
{-  
    -- * Conversion to ByteString
  , bitmapToByteString  
-}  
  ) 
  where

--------------------------------------------------------------------------------

import Data.Word

import Foreign

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Data.Bitmap.Base
import Data.Bitmap.Internal
import qualified Data.Bitmap.Pure as P

--------------------------------------------------------------------------------

-- | A bitmap filled with zero values.
emptyBitmap 
  :: PixelComponent t 
  => Size             -- ^ (width,height)
  -> NChn             -- ^ number of channels (components\/pixel)
  -> Bitmap t
emptyBitmap siz nchn = P.emptyBitmap siz nchn Nothing

-- | Creates a single channel bitmap from a function.
-- This is probably a bit slow.
createSingleChannelBitmap
  :: PixelComponent t 
  => Size               -- ^ (width,height)
  -> (Int -> Int -> t)  -- ^ the function used to create the bitmap
  -> Bitmap t
createSingleChannelBitmap siz fun = P.createSingleChannelBitmap siz Nothing fun

-- | Creates an empty bitmap with the same properties as the source.
emptyCloneBitmap 
  :: PixelComponent t 
  => Bitmap t           -- ^ source (only dimensions and such is used)
  -> Bitmap t           -- ^ new empty bitmap
emptyCloneBitmap bm = P.emptyCloneBitmap bm Nothing
  
--------------------------------------------------------------------------------

-- | Warning: this is probably slow.
componentMap :: PixelComponent s => (s -> s) -> Bitmap s -> Bitmap s
componentMap = P.componentMap

-- | Warning: this is probably slow.
componentMap' :: (PixelComponent s, PixelComponent t) => (s -> t) -> Bitmap s -> Bitmap t
componentMap' f bm = P.componentMap' f bm Nothing

--------------------------------------------------------------------------------

-- | Copies a subrectangle of the source image into a new image.  
copySubImage
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Bitmap t
copySubImage = P.copySubImage

-- | Copy into a new \"black\" bitmap; common generalization of crop and extend.
copySubImage'
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Size             -- ^ target image size
  -> Offset           -- ^ target rectangle offset
  -> Bitmap t
copySubImage' = P.copySubImage'
  
--------------------------------------------------------------------------------

-- | Flips the bitmap vertically.
flipBitmap 
  :: PixelComponent t 
  => Bitmap t
  -> Bitmap t
flipBitmap bm = P.flipBitmap bm Nothing

-- | Flips the bitmap horizontally.
mirrorBitmap 
  :: PixelComponent t 
  => Bitmap t
  -> Bitmap t
mirrorBitmap bm = P.mirrorBitmap bm Nothing

--------------------------------------------------------------------------------

-- | Converts between different component types.
castBitmap 
  :: (PixelComponent s, PixelComponent t)
  => Bitmap s               -- ^ source image
  -> Bitmap t 
castBitmap bm1 = P.castBitmap bm1 Nothing

--------------------------------------------------------------------------------

extractSingleChannel 
  :: PixelComponent t 
  => Bitmap t               -- ^ source image
  -> Int                    -- ^ source channel index
  -> Bitmap t
extractSingleChannel bm1 j = P.extractSingleChannel bm1 Nothing j

extractChannels :: PixelComponent t => Bitmap t -> [Bitmap t]
extractChannels bm = P.extractChannels bm Nothing

combineChannels :: PixelComponent t => [Bitmap t] -> Bitmap t
combineChannels bms = P.combineChannels bms Nothing

--------------------------------------------------------------------------------

bilinearResample 
  :: PixelComponent t 
  => Bitmap t           -- ^ source image
  -> Size               -- ^ target image size
  -> Bitmap t  
bilinearResample bm siz = P.bilinearResample bm siz Nothing 

bilinearResampleChannel
  :: PixelComponent t 
  => Bitmap t           -- ^ source image
  -> Int                -- ^ source channel indexe
  -> Size               -- ^ target image size
  -> Bitmap t  
bilinearResampleChannel bm j siz = P.bilinearResampleChannel bm j siz Nothing
  
--------------------------------------------------------------------------------

-- | Blends two bitmaps with the given weights; that is, the result is
-- the specified linear combination. If the values are outside the allowed
-- range (this can happen with the Word8, Word16, Word32 types and weights
-- whose sum is bigger than 1, or with a negative weight), then they are
-- clipped. The clipping /does not/ happen with the Float component type.
blendBitmaps
  :: PixelComponent t 
  => Float           -- ^ weight 1
  -> Float           -- ^ weight 2
  -> Bitmap t        -- ^ source image 1 
  -> Bitmap t        -- ^ source image 2
  -> Bitmap t
blendBitmaps w1 w2 bm1 bm2 = P.blendBitmaps w1 w2 bm1 bm2 Nothing

blendChannels
  :: PixelComponent t 
  => Float           -- ^ weight 1
  -> Float           -- ^ weight 2
  -> Bitmap t        -- ^ source image 1 
  -> Int             -- ^ channel index 1
  -> Bitmap t        -- ^ source image 2
  -> Int             -- ^ channel index 2
  -> Bitmap t
blendChannels w1 w2 bm1 ofs1 bm2 ofs2 = P.blendChannels w1 w2 bm1 ofs1 bm2 ofs2 Nothing
  
--------------------------------------------------------------------------------
  
-- | This is equivalent to @componentMap (\c -> c^gamma)@, except that
-- @(^)@ is defined only for integral exponents; but should be faster anyway.
powerlawGammaCorrection
  :: PixelComponent t 
  => Float              -- ^ gamma
  -> Bitmap t           -- ^ source image
  -> Bitmap t  
powerlawGammaCorrection gamma bm = P.powerlawGammaCorrection gamma bm Nothing

powerlawGammaCorrectionChannel
  :: PixelComponent t 
  => Float              -- ^ gamma
  -> Bitmap t           -- ^ source image
  -> Int                -- ^ source channel indexe
  -> Bitmap t  
powerlawGammaCorrectionChannel gamma bm j = P.powerlawGammaCorrectionChannel gamma bm j Nothing
  
--------------------------------------------------------------------------------

