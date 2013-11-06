
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.Pure.Pixels
-- Version     : 0.0.2
-- License     : BSD3
-- Copyright   : (c) 2009-2010 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI and CPP
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

-- | Access to individual pixels. It isn't very efficient to handle bitmaps this way. 

{-# LANGUAGE CPP #-}
module Data.Bitmap.Pure.Pixels
  ( 
    Bitmap1, Bitmap2, Bitmap3, Bitmap4
  , bitmap1, bitmap2, bitmap3, bitmap4 
    --
  , unsafeReadComponent
  , unsafeReadComponents
    --
  , unsafeReadPixel
  , unsafeReadPixel1
  , unsafeReadPixel2
  , unsafeReadPixel3
  , unsafeReadPixel4  
  ) 
  where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Applicative

import Data.Word
import Foreign hiding (unsafePerformIO)

import Data.Bitmap.Base
import Data.Bitmap.Internal

import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | Note that the resulting pointer is valid only within a line (because of the padding)
withComponentPtr 
  :: PixelComponent t 
  => Bitmap t       -- ^ the bitmap
  -> Offset           -- ^ position (x,y)
  -> Int              -- ^ channel index {0,1,...,nchannels-1}
  -> (Ptr t -> IO a)  -- ^ user action
  -> IO a
withComponentPtr bm (x,y) ofs action = 
  withForeignPtr (bitmapPtr bm) $ \p -> do
    let nchn = bitmapNChannels bm
        rowsize = bitmapPaddedRowSizeInBytes bm
        q = p `myPlusPtr` ( ( nchn*x + ofs ) * sizeOf (bitmapUndefined bm) + y * rowsize ) 
    action q

--------------------------------------------------------------------------------
        
-- | Newtypes for bitmaps with a fixed number of channels (components per pixel) 
newtype Bitmap1 t = Bitmap1 { fromBitmap1 :: Bitmap t } 
newtype Bitmap2 t = Bitmap2 { fromBitmap2 :: Bitmap t }
newtype Bitmap3 t = Bitmap3 { fromBitmap3 :: Bitmap t }
newtype Bitmap4 t = Bitmap4 { fromBitmap4 :: Bitmap t } 

bitmap1 :: Bitmap t -> Bitmap1 t
bitmap2 :: Bitmap t -> Bitmap2 t
bitmap3 :: Bitmap t -> Bitmap3 t
bitmap4 :: Bitmap t -> Bitmap4 t

bitmap1 bm = if bitmapNChannels bm == 1 then Bitmap1 bm else error "bitmap/bitmap1: number of channels is not 1"
bitmap2 bm = if bitmapNChannels bm == 2 then Bitmap2 bm else error "bitmap/bitmap2: number of channels is not 2"
bitmap3 bm = if bitmapNChannels bm == 3 then Bitmap3 bm else error "bitmap/bitmap3: number of channels is not 3"
bitmap4 bm = if bitmapNChannels bm == 4 then Bitmap4 bm else error "bitmap/bitmap4: number of channels is not 4"


instance BitmapClass Bitmap1 where
  underlyingBitmap = fromBitmap1

instance BitmapClass Bitmap2 where
  underlyingBitmap = fromBitmap2

instance BitmapClass Bitmap3 where
  underlyingBitmap = fromBitmap3

instance BitmapClass Bitmap4 where
  underlyingBitmap = fromBitmap4

--------------------------------------------------------------------------------

-- | It is not very efficient to read\/write lots of pixels this way.
unsafeReadComponent 
  :: PixelComponent t 
  => Bitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> t
unsafeReadComponent bm xy ofs = unsafePerformIO $ withComponentPtr bm xy ofs $ peek

-- | Please note that the component array to read shouldn't cross 
-- the boundary between lines.
unsafeReadComponents
  :: PixelComponent t 
  => Bitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> Int           -- ^ the number of components to read
  -> [t]
unsafeReadComponents bm xy ofs k = unsafePerformIO $ withComponentPtr bm xy ofs $ \p -> peekArray k p
    

unsafeReadPixel 
  :: PixelComponent t 
  => Bitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> [t]
unsafeReadPixel bm xy = unsafeReadComponents bm xy 0 (bitmapNChannels bm)
   
       
--------------------------------------------------------------------------------

unsafeReadPixel1 :: PixelComponent t => Bitmap1 t -> Offset -> t
unsafeReadPixel2 :: PixelComponent t => Bitmap2 t -> Offset -> (t,t)
unsafeReadPixel3 :: PixelComponent t => Bitmap3 t -> Offset -> (t,t,t)
unsafeReadPixel4 :: PixelComponent t => Bitmap4 t -> Offset -> (t,t,t,t)

unsafeReadPixel1 bm xy = unsafePerformIO $ 
  withComponentPtr (fromBitmap1 bm) xy 0 $ \p -> liftM (\[x]       ->  x       ) $ peekArray 1 p
  
unsafeReadPixel2 bm xy = unsafePerformIO $ 
  withComponentPtr (fromBitmap2 bm) xy 0 $ \p -> liftM (\[x,y]     -> (x,y)    ) $ peekArray 2 p

unsafeReadPixel3 bm xy = unsafePerformIO $ 
  withComponentPtr (fromBitmap3 bm) xy 0 $ \p -> liftM (\[x,y,z]   -> (x,y,z)  ) $ peekArray 3 p

unsafeReadPixel4 bm xy = unsafePerformIO $ 
  withComponentPtr (fromBitmap4 bm) xy 0 $ \p -> liftM (\[x,y,z,w] -> (x,y,z,w)) $ peekArray 4 p
  
--------------------------------------------------------------------------------

-- restricted type
{-# SPECIALIZE myPlusPtr :: Ptr Word8 -> Int -> Ptr Word8 #-}
{-# SPECIALIZE myPlusPtr :: Ptr Float -> Int -> Ptr Float #-}
myPlusPtr :: Ptr a -> Int -> Ptr a
myPlusPtr = plusPtr

--------------------------------------------------------------------------------

