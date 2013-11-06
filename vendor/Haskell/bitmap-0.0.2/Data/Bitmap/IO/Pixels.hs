
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.IO.Pixels
-- Version     : 0.0.2
-- License     : BSD3
-- Copyright   : (c) 2009-2010 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI and CPP
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

-- | Accessing individual pixels.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Data.Bitmap.IO.Pixels
  ( 
    -- * Types
    IOBitmap1 -- (..)
  , IOBitmap2 -- (..)
  , IOBitmap3 -- (..)
  , IOBitmap4 -- (..)
    --
  , ioBitmap1
  , ioBitmap2
  , ioBitmap3
  , ioBitmap4
    --
  , fromIOBitmap1
  , fromIOBitmap2
  , fromIOBitmap3
  , fromIOBitmap4
    -- * Reading and writing pixels
  , withComponentPtr
   --
  , unsafeReadComponent
  , unsafeWriteComponent
  , unsafeReadComponents
  , unsafeWriteComponents
    --
  , unsafeReadPixel
  , unsafeReadPixel1
  , unsafeReadPixel2
  , unsafeReadPixel3
  , unsafeReadPixel4
  , unsafeWritePixel1
  , unsafeWritePixel2
  , unsafeWritePixel3
  , unsafeWritePixel4
  ) 
  where
  
--------------------------------------------------------------------------------

import Control.Monad
import Control.Applicative

import Data.Word

--import Foreign    -- GHC 7 complains?
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal

--import Data.ByteString (ByteString)
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Internal as B

import Data.Bitmap.Internal
import Data.Bitmap.Base
import Data.Bitmap.IO

--------------------------------------------------------------------------------

-- | Note that the resulting pointer is valid only within a line (because of the padding)
withComponentPtr 
  :: PixelComponent t 
  => IOBitmap t       -- ^ the bitmap
  -> Offset           -- ^ position (x,y)
  -> Int              -- ^ channel index {0,1,...,nchannels-1}
  -> (Ptr t -> IO a)  -- ^ user action
  -> IO a
withComponentPtr (IOBitmap bm) (x,y) ofs action = 
  withForeignPtr (bitmapPtr bm) $ \p -> do
    let nchn = bitmapNChannels bm
        rowsize = bitmapPaddedRowSizeInBytes bm
        q = p `myPlusPtr` ( ( nchn*x + ofs ) * sizeOf (bitmapUndefined bm) + y * rowsize ) 
    action q
    
-- | It is not very efficient to read\/write lots of pixels this way.
unsafeReadComponent 
  :: PixelComponent t 
  => IOBitmap t    -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> IO t
unsafeReadComponent bm xy ofs = withComponentPtr bm xy ofs $ peek
    
unsafeWriteComponent
  :: PixelComponent t 
  => IOBitmap t    -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> t             -- ^ the value to write
  -> IO ()
unsafeWriteComponent bm xy ofs value = withComponentPtr bm xy ofs $ \q -> poke q value

-- | Please note that the component array to read shouldn't cross 
-- the boundary between lines.
unsafeReadComponents
  :: PixelComponent t 
  => IOBitmap t    -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> Int           -- ^ the number of components to read
  -> IO [t]
unsafeReadComponents bm xy ofs k = withComponentPtr bm xy ofs $ \p -> peekArray k p

-- | Please note that the component array to write shouldn't cross 
-- the boundary between lines.
unsafeWriteComponents
  :: PixelComponent t 
  => IOBitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> [t]           -- ^ the components to write
  -> IO ()
unsafeWriteComponents bm xy ofs values = withComponentPtr bm xy ofs $ \q -> pokeArray q values

unsafeReadPixel 
  :: PixelComponent t 
  => IOBitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> IO [t]
unsafeReadPixel bm xy = unsafeReadComponents bm xy 0 (bitmapNChannels bm)
   
--------------------------------------------------------------------------------

instance BitmapClass IOBitmap1 where
  underlyingBitmap = unIOBitmap . fromIOBitmap1

instance BitmapClass IOBitmap2 where
  underlyingBitmap = unIOBitmap . fromIOBitmap2

instance BitmapClass IOBitmap3 where
  underlyingBitmap = unIOBitmap . fromIOBitmap3

instance BitmapClass IOBitmap4 where
  underlyingBitmap = unIOBitmap . fromIOBitmap4

--------------------------------------------------------------------------------
  
-- | Newtypes for mutable bitmaps with a fixed number of channels (components per pixel) 
newtype IOBitmap1 t = IOBitmap1 { fromIOBitmap1 :: IOBitmap t } 
newtype IOBitmap2 t = IOBitmap2 { fromIOBitmap2 :: IOBitmap t }
newtype IOBitmap3 t = IOBitmap3 { fromIOBitmap3 :: IOBitmap t }
newtype IOBitmap4 t = IOBitmap4 { fromIOBitmap4 :: IOBitmap t } 

ioBitmap1 :: IOBitmap t -> IOBitmap1 t
ioBitmap2 :: IOBitmap t -> IOBitmap2 t
ioBitmap3 :: IOBitmap t -> IOBitmap3 t
ioBitmap4 :: IOBitmap t -> IOBitmap4 t

ioBitmap1 bm = if bitmapNChannels bm == 1 then IOBitmap1 bm else error "bitmap/ioBitmap1: number of channels is not 1"
ioBitmap2 bm = if bitmapNChannels bm == 2 then IOBitmap2 bm else error "bitmap/ioBitmap2: number of channels is not 2"
ioBitmap3 bm = if bitmapNChannels bm == 3 then IOBitmap3 bm else error "bitmap/ioBitmap3: number of channels is not 3"
ioBitmap4 bm = if bitmapNChannels bm == 4 then IOBitmap4 bm else error "bitmap/ioBitmap4: number of channels is not 4"

--------------------------------------------------------------------------------

unsafeReadPixel1 :: PixelComponent t => IOBitmap1 t -> Offset -> IO t
unsafeReadPixel2 :: PixelComponent t => IOBitmap2 t -> Offset -> IO (t,t)
unsafeReadPixel3 :: PixelComponent t => IOBitmap3 t -> Offset -> IO (t,t,t)
unsafeReadPixel4 :: PixelComponent t => IOBitmap4 t -> Offset -> IO (t,t,t,t)

unsafeWritePixel1 :: PixelComponent t => IOBitmap1 t -> Offset -> t -> IO ()
unsafeWritePixel2 :: PixelComponent t => IOBitmap2 t -> Offset -> (t,t) -> IO ()
unsafeWritePixel3 :: PixelComponent t => IOBitmap3 t -> Offset -> (t,t,t) -> IO ()
unsafeWritePixel4 :: PixelComponent t => IOBitmap4 t -> Offset -> (t,t,t,t) -> IO ()

unsafeReadPixel1 bm xy = withComponentPtr (fromIOBitmap1 bm) xy 0 $ \p -> liftM (\[x]       ->  x       ) $ peekArray 1 p
unsafeReadPixel2 bm xy = withComponentPtr (fromIOBitmap2 bm) xy 0 $ \p -> liftM (\[x,y]     -> (x,y)    ) $ peekArray 2 p
unsafeReadPixel3 bm xy = withComponentPtr (fromIOBitmap3 bm) xy 0 $ \p -> liftM (\[x,y,z]   -> (x,y,z)  ) $ peekArray 3 p
unsafeReadPixel4 bm xy = withComponentPtr (fromIOBitmap4 bm) xy 0 $ \p -> liftM (\[x,y,z,w] -> (x,y,z,w)) $ peekArray 4 p

unsafeWritePixel1 bm xy  x        = withComponentPtr (fromIOBitmap1 bm) xy 0 $ \q -> pokeArray q [x]
unsafeWritePixel2 bm xy (x,y)     = withComponentPtr (fromIOBitmap2 bm) xy 0 $ \q -> pokeArray q [x,y]
unsafeWritePixel3 bm xy (x,y,z)   = withComponentPtr (fromIOBitmap3 bm) xy 0 $ \q -> pokeArray q [x,y,z]
unsafeWritePixel4 bm xy (x,y,z,w) = withComponentPtr (fromIOBitmap4 bm) xy 0 $ \q -> pokeArray q [x,y,z,w]

--------------------------------------------------------------------------------

-- restricted type
{-# SPECIALIZE myPlusPtr :: Ptr Word8 -> Int -> Ptr Word8 #-}
{-# SPECIALIZE myPlusPtr :: Ptr Float -> Int -> Ptr Float #-}
myPlusPtr :: Ptr a -> Int -> Ptr a
myPlusPtr = plusPtr

--------------------------------------------------------------------------------
