
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.Pure
-- Version     : 0.0.2
-- License     : BSD3
-- Copyright   : (c) 2009-2010 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI and CPP
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

-- | The pure, inmutable API.

{-# LANGUAGE CPP #-}
module Data.Bitmap.Pure 
  ( 
    module Data.Bitmap.Base
    
    -- * Creating bitmaps
  , emptyBitmap
  , cloneBitmap
  , emptyCloneBitmap
  , createSingleChannelBitmap
  , bitmapFromForeignPtrUnsafe
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

-- import Foreign.ForeignPtr

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Data.Bitmap.Base
import Data.Bitmap.Internal
import qualified Data.Bitmap.IO as IO

import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | A bitmap filled with zero values.
-- Note: we /cannot/ guarantee the alignment
-- of the memory block (but typically it is aligned at least to machine word boundary),
-- but what we /can/ guarantee is that the rows are properly padded.
emptyBitmap 
  :: PixelComponent t 
  => Size             -- ^ (width,height)
  -> NChn             -- ^ number of channels (components\/pixel)
  -> Maybe Alignment  -- ^ the row alignment of the new image
  -> Bitmap t
emptyBitmap siz nchn malign = unIOBitmap $ unsafePerformIO $ IO.newIOBitmap siz nchn malign

-- | Creates a single channel bitmap from a function.
-- This is probably a bit slow.
createSingleChannelBitmap
  :: PixelComponent t 
  => Size               -- ^ (width,height)
  -> Maybe Alignment    -- ^ the row alignment of the new image
  -> (Int -> Int -> t)  -- ^ the function used to create the bitmap
  -> Bitmap t
createSingleChannelBitmap siz maling fun = unIOBitmap $ unsafePerformIO $ 
  IO.createSingleChannelBitmap siz maling fun 

-- | Creates an empty bitmap with the same properties as the source.
emptyCloneBitmap 
  :: PixelComponent t 
  => Bitmap t           -- ^ source (only dimensions and such is used)
  -> Maybe Alignment    -- ^ target alignment
  -> Bitmap t           -- ^ new empty bitmap
emptyCloneBitmap bm1 malign = unIOBitmap $ unsafePerformIO $ 
  IO.emptyCloneBitmap (IOBitmap bm1) malign

-- | Clones a bitmap. The only effect of this in the pure 
-- setting should be that the alignment/padding can change.
-- You shouldn't normally use this function.
cloneBitmap 
  :: PixelComponent t 
  => Bitmap t           -- ^ source image
  -> Maybe Alignment    -- ^ target alignment
  -> Bitmap t
cloneBitmap bm1 malign = unIOBitmap $ unsafePerformIO $ 
  IO.cloneBitmap (IOBitmap bm1) malign

--------------------------------------------------------------------------------

-- | Warning: this is probably slow.
componentMap :: PixelComponent s => (s -> s) -> Bitmap s -> Bitmap s
componentMap f bm = unIOBitmap $ unsafePerformIO $ 
  IO.componentMap f (IOBitmap bm)

-- | Warning: this is probably slow.
componentMap' :: (PixelComponent s, PixelComponent t) => (s -> t) -> Bitmap s -> Maybe Alignment -> Bitmap t
componentMap' f bm malign = unIOBitmap $ unsafePerformIO $ 
  IO.componentMap' f (IOBitmap bm) malign

--------------------------------------------------------------------------------

-- | Copies a subrectangle of the source image into a new image.  
copySubImage
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Bitmap t
copySubImage bm ofs siz = unIOBitmap $ unsafePerformIO $ 
  IO.copySubImage (IOBitmap bm) ofs siz

-- | Copy into a new \"black\" bitmap; common generalization of crop and extend.
copySubImage'
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Size             -- ^ target image size
  -> Offset           -- ^ target rectangle offset
  -> Bitmap t
copySubImage' bm1 ofs1 rsiz tsiz ofs2 = unIOBitmap $ unsafePerformIO $
  IO.copySubImage' (IOBitmap bm1) ofs1 rsiz tsiz ofs2
  
--------------------------------------------------------------------------------

-- | Flips the bitmap vertically.
flipBitmap 
  :: PixelComponent t 
  => Bitmap t
  -> Maybe Alignment
  -> Bitmap t
flipBitmap bm malign = unIOBitmap $ unsafePerformIO $ IO.flipBitmap (IOBitmap bm) malign

-- | Flips the bitmap horizontally.
mirrorBitmap 
  :: PixelComponent t 
  => Bitmap t
  -> Maybe Alignment
  -> Bitmap t
mirrorBitmap bm malign = unIOBitmap $ unsafePerformIO $ IO.mirrorBitmap (IOBitmap bm) malign
  
--------------------------------------------------------------------------------

-- | Converts between different component types.
castBitmap 
  :: (PixelComponent s, PixelComponent t)
  => Bitmap s               -- ^ source image
  -> Maybe Alignment        -- ^ target image row alignment
  -> Bitmap t 
castBitmap bm1 malign = unIOBitmap $ unsafePerformIO $ 
  IO.castBitmap (IOBitmap bm1) malign

--------------------------------------------------------------------------------

extractSingleChannel 
  :: PixelComponent t 
  => Bitmap t               -- ^ source image
  -> Maybe Alignment        -- ^ target image row alignment
  -> Int                    -- ^ source channel index
  -> Bitmap t
extractSingleChannel bm1 malign j = unIOBitmap $ unsafePerformIO $ 
  IO.extractSingleChannel (IOBitmap bm1) malign j 

extractChannels :: PixelComponent t => Bitmap t -> Maybe Alignment -> [Bitmap t]
extractChannels bm malign = map unIOBitmap $ unsafePerformIO $
  IO.extractChannels (IOBitmap bm) malign 

combineChannels :: PixelComponent t => [Bitmap t] -> Maybe Alignment -> Bitmap t
combineChannels bms malign = unIOBitmap $ unsafePerformIO $
  IO.combineChannels (map IOBitmap bms) malign

--------------------------------------------------------------------------------

bilinearResample 
  :: PixelComponent t 
  => Bitmap t           -- ^ source image
  -> Size               -- ^ target image size
  -> Maybe Alignment    -- ^ target image row alignment
  -> Bitmap t  
bilinearResample bm siz malign = unIOBitmap $ unsafePerformIO $ 
  IO.bilinearResample (IOBitmap bm) siz malign

bilinearResampleChannel
  :: PixelComponent t 
  => Bitmap t           -- ^ source image
  -> Int                -- ^ source channel indexe
  -> Size               -- ^ target image size
  -> Maybe Alignment    -- ^ target image row alignment
  -> Bitmap t  
bilinearResampleChannel bm j siz malign = unIOBitmap $ unsafePerformIO $ 
  IO.bilinearResampleChannel (IOBitmap bm) j siz malign
  
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
  -> Maybe Alignment -- ^ target alignment
  -> Bitmap t
blendBitmaps w1 w2 bm1 bm2 malign = unIOBitmap $ unsafePerformIO $ 
  IO.blendBitmaps w1 w2 (IOBitmap bm1) (IOBitmap bm2) malign

blendChannels
  :: PixelComponent t 
  => Float           -- ^ weight 1
  -> Float           -- ^ weight 2
  -> Bitmap t        -- ^ source image 1 
  -> Int             -- ^ channel index 1
  -> Bitmap t        -- ^ source image 2
  -> Int             -- ^ channel index 2
  -> Maybe Alignment -- ^ target alignment
  -> Bitmap t
blendChannels w1 w2 bm1 ofs1 bm2 ofs2 malign = unIOBitmap $ unsafePerformIO $ 
  IO.blendChannels w1 w2 (IOBitmap bm1) ofs1 (IOBitmap bm2) ofs2 malign
  
--------------------------------------------------------------------------------
  
-- | This is equivalent to @componentMap (\c -> c^gamma)@, except that
-- @(^)@ is defined only for integral exponents; but should be faster anyway.
powerlawGammaCorrection
  :: PixelComponent t 
  => Float              -- ^ gamma
  -> Bitmap t           -- ^ source image
  -> Maybe Alignment    -- ^ target image row alignment
  -> Bitmap t  
powerlawGammaCorrection gamma bm malign = unIOBitmap $ unsafePerformIO $ 
  IO.powerlawGammaCorrection gamma (IOBitmap bm) malign

powerlawGammaCorrectionChannel
  :: PixelComponent t 
  => Float              -- ^ gamma
  -> Bitmap t           -- ^ source image
  -> Int                -- ^ source channel indexe
  -> Maybe Alignment    -- ^ target image row alignment
  -> Bitmap t  
powerlawGammaCorrectionChannel gamma bm j malign = unIOBitmap $ unsafePerformIO $ 
  IO.powerlawGammaCorrectionChannel gamma (IOBitmap bm) j malign
  
--------------------------------------------------------------------------------

{-
-- | Note that the data is /shared/; and also that the resulting ByteString
-- is encoded using the host machine's endianness.
bitmapToByteString :: PixelComponent t => Bitmap t -> ByteString
bitmapToByteString bm = bs where
  bs = B.fromForeignPtr (castForeignPtr $ bitmapPtr bm) 0 n
  n = bitmapSizeInBytes bm 
-}

{-  
-- | As its name says, this pretty much unsafe!
reallyUnsafeBitmapFromByteString :: ByteString -> Size -> NChn -> Padding -> Bitmap t
reallyUnsafeBitmapFromByteString bs siz nchn pad = 
  if n > len || ofs /= 0 
    then error "reallyUnsafeBitmapFromByteString: better than segfault :)"
    else bm   
  where
    bm = Bitmap 
      { bitmapSize = siz
      , bitmapNChannels = nchn
      , bitmapPtr = fptr 
      , bitmapRowPadding = pad
      , bitmapRowAlignment = 1
      } :: Bitmap t
    n = bitmapSizeInBytes bm
    (fptr,ofs,len) = B.toForeignPtr bm
-}
  
--------------------------------------------------------------------------------

