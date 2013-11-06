
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.IO
-- Version     : 0.0.2
-- License     : BSD3
-- Copyright   : (c) 2009-2010 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI and CPP
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

-- | The full, mutable API in the IO monad.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# CFILES cbits/bm.c #-}  -- for Hugs 
module Data.Bitmap.IO
  ( 
    module Data.Bitmap.Base
    -- * Mutable bitmap type
  , IOBitmap
  , IOBitmapChannel
  , unsafeFreezeBitmap
  , unsafeThawBitmap
    -- * Creating and accessing bitmaps
  , emptyBitmap
  , cloneBitmap
  , emptyCloneBitmap
  , createSingleChannelBitmap
  , newIOBitmap
  , newIOBitmapUninitialized
  , copyBitmapFromPtr
  -- , bitmapFromForeignPtrUnsafe
  , ioBitmapFromForeignPtrUnsafe
    -- * Using bitmaps
  , withIOBitmap
    -- * Mapping over bitmaps
  , componentMap
  , componentMap'
  , componentMapInPlace
    -- * Cropping and extending
  , copySubImage
  , copySubImage'  
  , copySubImageInto
    -- * Flipping and mirroring
  , flipBitmap
  , flipBitmapInPlace
  , mirrorBitmap
  , mirrorBitmapInPlace
    -- * Cast
  , castBitmap
  -- , castChannel
  -- , castChannelInto
    -- * Manipulating channels
  , combineChannels 
  , extractChannels 
  , extractSingleChannel 
  , extractChannelInto
    -- * Bilinear resampling
  , bilinearResample
  , bilinearResampleChannel
  , bilinearResampleChannelInto
    -- * Blending
  , blendBitmaps
  , blendChannels
  , blendChannelsInto
    -- * Gamma correction
  , powerlawGammaCorrection
  , powerlawGammaCorrectionChannel  
  , powerlawGammaCorrectionChannelInto  
{-  
    -- * Conversion to\/from ByteString
  , copyBitmapToByteString
  , copyBitmapFromByteString
-}  
{-
    -- * Reading and writing pixels
  , withComponentPtr
  , IOBitmap1 (..)
  , IOBitmap2 (..)
  , IOBitmap3 (..)
  , IOBitmap4 (..)
  , unsafeReadComponent
  , unsafeWriteComponent
  , unsafeReadComponents
  , unsafeWriteComponents
  , unsafeReadPixel
  , unsafeReadPixel1
  , unsafeReadPixel2
  , unsafeReadPixel3
  , unsafeReadPixel4
  , unsafeWritePixel1
  , unsafeWritePixel2
  , unsafeWritePixel3
  , unsafeWritePixel4
-}
  ) 
  where
  
--------------------------------------------------------------------------------

import Control.Monad
import Control.Applicative

--import Data.Array.IArray

import Data.Word
import Data.List (nub)

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Data.Bitmap.Internal
import Data.Bitmap.Base

--------------------------------------------------------------------------------
  
unsafeFreezeBitmap :: IOBitmap t -> Bitmap t  
unsafeFreezeBitmap = unIOBitmap

unsafeThawBitmap :: Bitmap t -> IOBitmap t
unsafeThawBitmap = IOBitmap

--------------------------------------------------------------------------------

defaultAlignment :: Int
defaultAlignment = 4

validateMaybeAlignment :: Maybe Alignment -> Alignment
validateMaybeAlignment = maybe defaultAlignment validateAlignment

validateAlignment :: Alignment -> Alignment
validateAlignment k = 
  if isValidAlignment k 
    then k 
    else error "invalid row alignment (allowed values: 1, 2, 4, and 8)" -- and 16)"
  
--------------------------------------------------------------------------------

-- GHC's type inference is acting up, that's why we need this here
allocBitmap :: PixelComponent t => Bitmap t -> IO (Bitmap t)
allocBitmap bm0 = do
  fptr <- mallocForeignPtrBytes (bitmapSizeInBytes bm0) -- :: IO (ForeignPtr t)
  return $ bm0 { _bitmapPtr = fptr }

allocIOBitmap :: PixelComponent t => IOBitmap t -> IO (IOBitmap t)
allocIOBitmap bm = IOBitmap <$> (allocBitmap $ unIOBitmap bm)

-- we do not initialize the new bitmap!
newBitmapRaw :: PixelComponent t => Size -> NChn -> Padding -> Alignment -> IO (IOBitmap t)   
newBitmapRaw siz nchn pad align = do
  let bm0 = Bitmap 
        { _bitmapSize = siz
        , _bitmapNChannels = nchn
        , _bitmapPtr = undefined 
        , _bitmapRowPadding = pad
        , _bitmapRowAlignment = align
        } -- :: Bitmap t
{-        
  let len = bitmapSizeInBytes bm0      
  fptr <- mallocForeignPtrBytes len -- :: IO (ForeignPtr t)
  return $ bm0 { bitmapPtr = fptr }
-}
  IOBitmap <$> allocBitmap bm0
   
-- | Note: we /cannot/ guarantee the alignment
-- of the memory block (but typically it is aligned at least to machine word boundary),
-- but what we /can/ guarantee is that the rows are properly padded.
--
-- At the moment, the default alignment is 4, valid alignments are 1, 2, 4, 8 and 16,
-- and the padding method is compatible with the OpenGL one (that is, the padding is the
-- smallest multiple of a component size such that the next row is aligned).
-- 
-- The resulting new bitmap is filled with zeros.
newIOBitmap 
  :: PixelComponent t 
  => Size             -- ^ (width,height)
  -> NChn             -- ^ number of channels (components\/pixel)
  -> Maybe Alignment  -- ^ the row alignment of the new image
  -> IO (IOBitmap t)
newIOBitmap siz nchn malign = do  
  bm <- newIOBitmapUninitialized siz nchn malign -- :: IO (Bitmap t)
  let fptr = bitmapPtr bm
      len  = bitmapSizeInBytes bm
  withForeignPtr fptr $ \p -> c_memset (castPtr p) len 0
  return bm

allocBitmapWithRecommendedPadding :: PixelComponent t => Bitmap t -> IO (Bitmap t)
allocBitmapWithRecommendedPadding bm0 = 
  allocBitmap $ 
    bm0 { _bitmapRowPadding = recommendedPadding bm0 } 
  
newIOBitmapUninitialized :: PixelComponent t => Size -> NChn -> Maybe Alignment -> IO (IOBitmap t)
newIOBitmapUninitialized siz nchn malign = do
  let align = validateMaybeAlignment malign
      bm0 = Bitmap 
        { _bitmapSize = siz
        , _bitmapNChannels = nchn
        , _bitmapPtr = undefined 
        , _bitmapRowPadding = undefined -- pad
        , _bitmapRowAlignment = align
        } -- :: Bitmap t
{-      
  let pad = recommendedPadding bm0
  newBitmapRaw siz nchn pad align
-}
  bm <- allocBitmapWithRecommendedPadding bm0
  return (IOBitmap bm)

-- | Creates a new single-channel bitmap, using the given function to compute
-- the pixel values.
-- Warning, this is probably slow!  
createSingleChannelBitmap 
  :: PixelComponent t 
  => Size               -- ^ (width,height)
  -> Maybe Alignment    -- ^ the row alignment of the new image
  -> (Int -> Int -> t)  -- ^ the function we will use to fill the bitmap
  -> IO (IOBitmap t)
createSingleChannelBitmap siz malign fun = do  
  bm <- newIOBitmapUninitialized siz 1 malign 
  let fptr = bitmapPtr bm
      len  = bitmapSizeInBytes bm
      -- f :: Int -> Int -> t -> t
      f x y _ = fun x y 
  genericComponentMapWithPos f bm bm
  return bm

{-    
createBitmap    
  :: PixelComponent t 
  => Size               -- ^ (width,height)
  -> Maybe Alignment    -- ^ the row alignment of the new image
  -> [Int -> Int -> t]  -- ^ the functions we will use to fill the bitmap
  -> IO (Bitmap t)
createBitmap siz malign funs = do 
  let nchn = length funs 
  bm <- newIOBitmapUninitialized siz nchn malign 
  let fptr = bitmapPtr bm
      len  = bitmapSizeInBytes bm
      f :: Int -> Int -> t -> t
      f x y _ = fun x y 
  genericComponentMapWithPos f bm bm
  return bm
-}
    
copyBitmapFromPtr 
  :: PixelComponent t 
  => Size       -- ^ (width,height) of the source
  -> NChn       -- ^ number of channels in the source 
  -> Padding    -- ^ source padding
  -> Ptr t      -- ^ the source
  -> Maybe Alignment  -- ^ target alignment
  -> IO (IOBitmap t)
copyBitmapFromPtr siz@(w,h) nchn srcpad srcptr tgtmalign = do
  bm <- newIOBitmapUninitialized siz nchn tgtmalign
  withIOBitmap bm $ \_ _ _ tgtptr -> do
    let pure_line = bitmapUnpaddedRowSizeInBytes bm
        src_line  = pure_line + srcpad
        tgt_line  = bitmapPaddedRowSizeInBytes bm
    forM_ [0..h-1] $ \y -> do
      let p = srcptr `myPlusPtr` (y*src_line)
          q = tgtptr `myPlusPtr` (y*tgt_line)
      c_memcpy (castPtr p) (castPtr q) pure_line 
  return bm

ioBitmapFromForeignPtrUnsafe 
  :: PixelComponent t 
  => Size -> NChn -> Alignment -> Padding -> ForeignPtr t -> IOBitmap t
ioBitmapFromForeignPtrUnsafe siz nchn align pad fptr = IOBitmap $ 
  bitmapFromForeignPtrUnsafe siz nchn align pad fptr
        
-- | @withIOBitmap bitmap $ \\(w,h) nchn padding ptr -> ...@
withIOBitmap :: PixelComponent t => IOBitmap t -> (Size -> NChn -> Padding -> Ptr t -> IO a) -> IO a
withIOBitmap (IOBitmap bm) action = 
  withForeignPtr (bitmapPtr bm) $ \p -> 
    action (bitmapSize bm) (bitmapNChannels bm) (bitmapRowPadding bm) p

--------------------------------------------------------------------------------

{-

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

-}

--------------------------------------------------------------------------------

{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word8  -> Ptr Word8  -> IO ()) -> IOBitmap Word8  -> IOBitmap Word8  -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word16 -> Ptr Word16 -> IO ()) -> IOBitmap Word16 -> IOBitmap Word16 -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word32 -> Ptr Word32 -> IO ()) -> IOBitmap Word32 -> IOBitmap Word32 -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Float  -> Ptr Float  -> IO ()) -> IOBitmap Float  -> IOBitmap Float  -> IO () #-}

{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word8  -> Ptr Float  -> IO ()) -> IOBitmap Word8  -> IOBitmap Float  -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Float  -> Ptr Word8  -> IO ()) -> IOBitmap Float  -> IOBitmap Word8  -> IO () #-}

{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word16 -> Ptr Float  -> IO ()) -> IOBitmap Word16 -> IOBitmap Float  -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Float  -> Ptr Word16 -> IO ()) -> IOBitmap Float  -> IOBitmap Word16 -> IO () #-}

-- the first Int is the y position
-- the second Int is the number of pixel components (nchn*width)
genericComponentRowMap 
  :: (PixelComponent s, PixelComponent t) 
  => (Int -> Int -> Ptr s -> Ptr t -> IO ())        -- ^ ypos totalNumberOfComps src tgt
  -> IOBitmap s -> IOBitmap t -> IO ()
genericComponentRowMap rowAction bm1 bm2 = do

  let (w1,h1) = bitmapSize       bm1
      pad1    = bitmapRowPadding bm1
      nchn1   = bitmapNChannels  bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1
      
  let (w2,h2) = bitmapSize       bm2
      pad2    = bitmapRowPadding bm2
      nchn2   = bitmapNChannels  bm2
      fptr2   = bitmapPtr        bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2
    
  let minw = min w1 w2  
      npc = nchn1 * minw
      
  when (nchn1 /= nchn2) $ 
    error "bitmap/genericRowMap: number of channels disagree" 
    
  withForeignPtr fptr1 $ \ptr1 -> withForeignPtr fptr2 $ \ptr2 -> 
    forM_ (zip3 [0..h1-1]
               (map (*xlen1) [0..h1-1]) 
               (map (*xlen2) [0..h2-1])) $ \(ypos,vo1,vo2) -> do
      let p1 = ptr1 `myPlusPtr` vo1     
          p2 = ptr2 `myPlusPtr` vo2     
      rowAction ypos npc p1 p2 

-------

-- userAction ypos width ptr1 nchn1 ptr2 nchn2 
genericPixelRowMap 
  :: (PixelComponent s, PixelComponent t) 
  => (Int -> Int -> Ptr s -> NChn -> Ptr t -> NChn -> IO ())    -- ^ ypos width ptr1 nchn1 ptr2 nchn2 
  -> IOBitmap s -> IOBitmap t -> IO ()
genericPixelRowMap rowAction bm1 bm2 = do

  let (w1,h1) = bitmapSize       bm1
      pad1    = bitmapRowPadding bm1
      nchn1   = bitmapNChannels  bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1
      
  let (w2,h2) = bitmapSize       bm2
      pad2    = bitmapRowPadding bm2
      nchn2   = bitmapNChannels  bm2
      fptr2   = bitmapPtr        bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2
    
  let minw = min w1 w2 

  withForeignPtr fptr1 $ \ptr1 -> withForeignPtr fptr2 $ \ptr2 -> 
    forM_ (zip3 [0..h1-1] 
                (map (*xlen1) [0..h1-1]) 
                (map (*xlen2) [0..h2-1])) $ \(ypos,o1,o2) -> do
      let p1 = ptr1 `myPlusPtr` o1     
          p2 = ptr2 `myPlusPtr` o2     
      rowAction ypos minw p1 nchn1 p2 nchn2

--------------------------------------------------------------------------------
      
{-# SPECIALIZE genericComponentMap :: (Word8  -> Word8 ) -> IOBitmap Word8  -> IOBitmap Word8  -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Word16 -> Word16) -> IOBitmap Word16 -> IOBitmap Word16 -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Word32 -> Word32) -> IOBitmap Word32 -> IOBitmap Word32 -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Float  -> Float ) -> IOBitmap Float  -> IOBitmap Float  -> IO () #-}      

{-# SPECIALIZE genericComponentMap :: (Word8  -> Float ) -> IOBitmap Word8  -> IOBitmap Float  -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Float  -> Word8 ) -> IOBitmap Float  -> IOBitmap Word8  -> IO () #-}
      
{-# SPECIALIZE genericComponentMap :: (Word16 -> Float ) -> IOBitmap Word16 -> IOBitmap Float  -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Float  -> Word16) -> IOBitmap Float  -> IOBitmap Word16 -> IO () #-}      
      
genericComponentMap 
--  :: forall s t . (PixelComponent s, PixelComponent t) 
  :: (PixelComponent s, PixelComponent t) 
  => (s -> t) -> IOBitmap s -> IOBitmap t -> IO ()  
genericComponentMap f bm1 bm2 = genericComponentRowMap g bm1 bm2 where
  --h :: (Ptr s, Ptr t) -> Int -> IO (Ptr s, Ptr t)
  h (q1,q2) _ = do
    x <- peek q1
    poke q2 (f x)
    return (advancePtr1 q1, advancePtr1 q2)
  --g :: Int -> Int -> Ptr s -> Ptr t -> IO ()
  g ypos n p1 p2 = do
    foldM_ h (p1,p2) [0..n-1]

{-# SPECIALIZE genericComponentMapWithPos :: (Int -> Int -> Word8  -> Word8 ) -> IOBitmap Word8  -> IOBitmap Word8  -> IO () #-}      
{-# SPECIALIZE genericComponentMapWithPos :: (Int -> Int -> Word16 -> Word16) -> IOBitmap Word16 -> IOBitmap Word16 -> IO () #-}      
{-# SPECIALIZE genericComponentMapWithPos :: (Int -> Int -> Word32 -> Word32) -> IOBitmap Word32 -> IOBitmap Word32 -> IO () #-}      
{-# SPECIALIZE genericComponentMapWithPos :: (Int -> Int -> Float  -> Float ) -> IOBitmap Float  -> IOBitmap Float  -> IO () #-}      

genericComponentMapWithPos 
--  :: forall s t . (PixelComponent s, PixelComponent t) 
  :: (PixelComponent s, PixelComponent t) 
  => (Int -> Int -> s -> t) -> IOBitmap s -> IOBitmap t -> IO ()  
genericComponentMapWithPos f bm1 bm2 = genericComponentRowMap g bm1 bm2 where
  --h :: Int -> (Ptr s, Ptr t) -> Int -> IO (Ptr s, Ptr t)
  h ypos (q1,q2) xpos = do
    x <- peek q1
    poke q2 (f xpos ypos x)
    return (advancePtr1 q1, advancePtr1 q2)
  --g :: Int -> Int -> Ptr s -> Ptr t -> IO ()
  g ypos n p1 p2 = do
    foldM_ (h ypos) (p1,p2) [0..n-1]

--------------------------------------------------------------------------------

-- | Maps a function over each component of each pixel. Warning: this is probably slow!
-- Use a specialized function if there is one for your task.
{- 
-- Note: We don't do the more general (s->t) here, because then we would have no idea 
-- about the padding in the new bitmap. See `componentMap'` for that.
-}
componentMap :: PixelComponent s => (s -> s) -> IOBitmap s -> IO (IOBitmap s)
componentMap f bm1 = do
  let siz   = bitmapSize bm1
      nchn  = bitmapNChannels bm1
      align = bitmapRowAlignment bm1
  bm2 <- newIOBitmapUninitialized siz nchn (Just align) 
  genericComponentMap f bm1 bm2 
  return bm2

componentMapInPlace :: PixelComponent s => (s -> s) -> IOBitmap s -> IO ()
componentMapInPlace f bm = do
  genericComponentMap f bm bm
    
-- See the comments at 'componentMap'.
componentMap' 
  :: (PixelComponent s, PixelComponent t) 
  => (s -> t) 
  -> IOBitmap s           -- ^ source bitmap
  -> Maybe Alignment    -- ^ row alignment of the resulting bitmap
  -> IO (IOBitmap t)
componentMap' f bm1 malign = do
  let siz  = bitmapSize bm1
      nchn = bitmapNChannels bm1
      x = bitmapPaddedRowSizeInBytes bm1 
  bm2 <- newIOBitmapUninitialized siz nchn malign 
  genericComponentMap f bm1 bm2 
  return bm2
  
--------------------------------------------------------------------------------

-- | Clones a bitmap.
cloneBitmap 
  :: PixelComponent t 
  => IOBitmap t         -- ^ source image
  -> Maybe Alignment    -- ^ target alignment
  -> IO (IOBitmap t)
cloneBitmap bm1 malign = do
  let siz1@(w1,h1) = bitmapSize bm1
      pad1    = bitmapRowPadding bm1
      nchn1   = bitmapNChannels  bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1

  bm2 <- newIOBitmapUninitialized siz1 nchn1 malign

  let fptr2   = bitmapPtr                  bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2
      
  let len1 = bitmapUnpaddedRowSizeInBytes bm1
      len2 = bitmapUnpaddedRowSizeInBytes bm2

  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      forM_ [0..h1-1] $ \i -> do
        let p = plusPtr ptr1 (i*xlen1)
            q = plusPtr ptr2 (i*xlen2)
        c_memcpy p q len1
        
  return bm2

-- | Creates an empty bitmap with the same properties as the source.
emptyCloneBitmap
  :: PixelComponent t 
  => IOBitmap t         -- ^ source (only dimensions and such is used)
  -> Maybe Alignment    -- ^ target alignment
  -> IO (IOBitmap t)    -- ^ new empty bitmap
emptyCloneBitmap bm1 malign = do

  let siz1  = bitmapSize bm1
      nchn1 = bitmapNChannels  bm1

  bm2 <- newIOBitmapUninitialized siz1 nchn1 malign

  let fptr2 = bitmapPtr         bm2
      n     = bitmapSizeInBytes bm2
  withForeignPtr fptr2 $ \ptr2 -> do
    c_memset (castPtr ptr2 :: Ptr Word8) n 0
    
  return bm2  

-- | Synonym for 'newIOBitmap'
emptyBitmap 
  :: PixelComponent t 
  => Size             -- ^ (width,height)
  -> NChn             -- ^ number of channels (components\/pixel)
  -> Maybe Alignment  -- ^ the row alignment of the new image
  -> IO (IOBitmap t)
emptyBitmap = newIOBitmap
    
--------------------------------------------------------------------------------


-- | Copies a subrectangle of the source image into a new image.  
copySubImage
  :: PixelComponent t 
  => IOBitmap t       -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> IO (IOBitmap t)
copySubImage bm ofs1 siz1 = copySubImage' bm ofs1 siz1 siz1 (0,0)   

-- | Copy into a new \"black\" bitmap; common generalization of crop and extend.
copySubImage'
  :: PixelComponent t 
  => IOBitmap t       -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Size             -- ^ target image size
  -> Offset           -- ^ target rectangle offset
  -> IO (IOBitmap t)
copySubImage' bm1 ofs1 rsiz tsiz ofs2 = do
  let align   = bitmapRowAlignment  bm1
      nchn    = bitmapNChannels  bm1
  bm2 <- newIOBitmap tsiz nchn (Just align)
  copySubImageInto bm1 ofs1 rsiz bm2 ofs2
  return bm2

-- | The source rectangle may be arbitrary, may or may not intersect the
-- source image in any way. We only copy the intersection of the rectangle
-- with the image.  
copySubImageInto 
  :: PixelComponent t 
  => IOBitmap t       -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> IOBitmap t       -- ^ target image
  -> Offset           -- ^ target rectangle offset
  -> IO ()
  
copySubImageInto bm1 ofs1@(o1x0,o1y0) siz1@(sx0,sy0) bm2 ofs2@(o2x0,o2y0) = do

  let (bm1xs,bm1ys) = bitmapSize bm1
      pad1    = bitmapRowPadding bm1
      align1  = bitmapRowAlignment  bm1
      nchn1   = bitmapNChannels  bm1
      pixsiz1 = bitmapPixelSizeInBytes bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1

  let (bm2xs,bm2ys) = bitmapSize bm2
      pad2    = bitmapRowPadding bm2
      align2  = bitmapRowAlignment  bm2
      nchn2   = bitmapNChannels  bm2
      pixsiz2 = bitmapPixelSizeInBytes bm2
      fptr2   = bitmapPtr        bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2

  when (nchn1/=nchn2) $ error "bitmap/copySubImageInto: number of channels disagree" 

  -- handle negative offsets
  let (o1x1,sx1,o2x1) = if o1x0 >= 0 then (o1x0, sx0, o2x0) else (0, sx0+o1x0, o2x0-o1x0) 
      (o1y1,sy1,o2y1) = if o1y0 >= 0 then (o1y0, sy0, o2y0) else (0, sy0+o1y0, o2y0-o1y0) 

      (o1x ,sx ,o2x ) = if o2x1 >= 0 then (o1x1, sx1, o2x1) else (o1x1-o2x1, sx1+o2x1, 0) 
      (o1y ,sy ,o2y ) = if o2y1 >= 0 then (o1y1, sy1, o2y1) else (o1y1-o2y1, sy1+o2y1, 0) 
  
  -- size of the rectangle we actually copy
  let xs = minimum [ sx , (bm1xs - o1x) , (bm2xs - o2x) ] 
      ys = minimum [ sy , (bm1ys - o1y) , (bm2ys - o2y) ] 
      pixsiz = pixsiz1

  when (xs>0 && ys>0) $ do
    withForeignPtr fptr1 $ \ptr1' -> withForeignPtr fptr2 $ \ptr2' -> do
      let ptr1 = ptr1' `myPlusPtr` (pixsiz*o1x)
          ptr2 = ptr2' `myPlusPtr` (pixsiz*o2x)
          nbytes = pixsiz*xs
      forM_ (zip (map (*xlen1) [o1y..o1y+ys-1]) 
                 (map (*xlen2) [o2y..o2y+ys-1])) $ \(vo1,vo2) -> do
        let p1 = ptr1 `plusPtr` vo1     
            p2 = ptr2 `plusPtr` vo2     
        c_memcpy p1 p2 nbytes

--------------------------------------------------------------------------------

-- | Convert a bitmap to one with a different component type.
castBitmap 
  :: (PixelComponent s, PixelComponent t)
  => IOBitmap s             -- ^ source image
  -> Maybe Alignment        -- ^ target image row alignment
  -> IO (IOBitmap t) 
castBitmap bm1 malign = do
  
  let nchn1 = bitmapNChannels bm1
      siz1@(w,h) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  bm2 <- newIOBitmapUninitialized siz1 nchn1 malign

  let pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_cast_bitmap 
        (bitmapCType bm1)  (bitmapCType bm2)
        (ci w) (ci h)
        ptr1 (ci nchn1) (ci pad1) 0 -- (ci ofs1)
        ptr2 (ci nchn1) (ci pad2) 0 -- (ci ofs2)

  return bm2
          
--------------------------------------------------------------------------------

_flipBitmapInto 
  :: PixelComponent t 
  => IOBitmap t             -- ^ source image
  -> IOBitmap t             -- ^ target image 
  -> IO () 
_flipBitmapInto bm1 bm2 = do

  let siz1@(w1,h1) = bitmapSize bm1
      pad1    = bitmapRowPadding bm1
      nchn1   = bitmapNChannels  bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1

  let siz2@(w2,h2) = bitmapSize bm2
      pad2    = bitmapRowPadding bm2
      nchn2   = bitmapNChannels  bm2
      fptr2   = bitmapPtr        bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2
      
  let len1 = bitmapUnpaddedRowSizeInBytes bm1
      len2 = bitmapUnpaddedRowSizeInBytes bm2

  when ( siz1 /= siz2 || nchn1 /= nchn2 || len1 /= len2 ) $ error "_flipBitmapInto" 

  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
    
      if ptr1 == ptr2
        then do 
          allocaBytes len1 $ \tmp -> do
            forM_ [0..(div h1 2)-1] $ \i -> do
              let j = h1-1-i
                  p1 = plusPtr ptr1 (i*xlen1)
                  q1 = plusPtr ptr1 (j*xlen1)                
                  p2 = plusPtr ptr2 (i*xlen2)
                  q2 = plusPtr ptr2 (j*xlen2)
              -- we have to be careful, since the two bitmaps coincide.
              -- that's why the extra copy
              c_memcpy p1  tmp len1
              c_memcpy q1  p2  len1
              c_memcpy tmp q2  len1    
        else do
          forM_ [0..h1-1] $ \i -> do
            let j = h1-1-i
                p = plusPtr ptr1 (i*xlen1)
                q = plusPtr ptr2 (j*xlen2)
            c_memcpy p q len1
              
-- | Flips the bitmap vertically.
flipBitmap 
  :: PixelComponent t 
  => IOBitmap t             -- ^ source image
  -> Maybe Alignment        -- ^ target image row alignment
  -> IO (IOBitmap t) 
flipBitmap bm1 malign = do
  let nchn = bitmapNChannels bm1
      siz@(w,h) = bitmapSize bm1
  bm2 <- newIOBitmapUninitialized siz nchn malign 
  _flipBitmapInto bm1 bm2
  return bm2
  
flipBitmapInPlace
  :: PixelComponent t 
  => IOBitmap t             -- ^ source image
  -> IO () 
flipBitmapInPlace bm = do
  _flipBitmapInto bm bm

--------------------------------------------------------------------------------

_mirrorBitmapInto 
  :: PixelComponent t 
  => IOBitmap t             -- ^ source image
  -> IOBitmap t             -- ^ target image 
  -> IO () 
_mirrorBitmapInto bm1 bm2 = do

  let siz1@(w1,h1) = bitmapSize bm1
      pad1    = bitmapRowPadding bm1
      nchn1   = bitmapNChannels  bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1

  let siz2@(w2,h2) = bitmapSize bm2
      pad2    = bitmapRowPadding bm2
      nchn2   = bitmapNChannels  bm2
      fptr2   = bitmapPtr        bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2
      
  let len1 = bitmapUnpaddedRowSizeInBytes bm1
      len2 = bitmapUnpaddedRowSizeInBytes bm2
      bpp1 = bitmapPixelSizeInBytes bm1
      bpp2 = bitmapPixelSizeInBytes bm2

  when ( siz1 /= siz2 || nchn1 /= nchn2 || len1 /= len2 || bpp1 /= bpp2 ) $ error "_mirrorBitmapInto" 

  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      forM_ [0..h1-1] $ \i -> do
        let p = plusPtr ptr1 (i*xlen1)
            q = plusPtr ptr2 (i*xlen2)
        c_mirror_line (ci w1) (ci bpp1) p q
              
-- | Flips the bitmap horizontally.
mirrorBitmap 
  :: PixelComponent t 
  => IOBitmap t             -- ^ source image
  -> Maybe Alignment        -- ^ target image row alignment
  -> IO (IOBitmap t) 
mirrorBitmap bm1 malign = do
  let nchn = bitmapNChannels bm1
      siz@(w,h) = bitmapSize bm1
  bm2 <- newIOBitmapUninitialized siz nchn malign 
  _mirrorBitmapInto bm1 bm2
  return bm2
  
mirrorBitmapInPlace
  :: PixelComponent t 
  => IOBitmap t             -- ^ source image
  -> IO () 
mirrorBitmapInPlace bm = do
  _mirrorBitmapInto bm bm
  
--------------------------------------------------------------------------------

extractSingleChannel 
  :: PixelComponent t 
  => IOBitmap t             -- ^ source image
  -> Maybe Alignment        -- ^ target image row alignment
  -> Int                    -- ^ source channel index
  -> IO (IOBitmap t) 
extractSingleChannel bm1 malign j = do
  let nchn = bitmapNChannels bm1
      siz@(w,h) = bitmapSize bm1
  when (j<0 || j>=nchn) $ error "bitmap/extractSingleChannel: invalid channel index"
  bm2 <- newIOBitmapUninitialized siz 1 malign
  extractChannelInto bm1 j bm2 0
  return bm2
  
extractChannels :: PixelComponent t => IOBitmap t -> Maybe Alignment -> IO [IOBitmap t]
extractChannels bm malign = 
  mapM (extractSingleChannel bm malign) [0..nchn-1] 
    where nchn = bitmapNChannels bm


combineChannels :: PixelComponent t => [IOBitmap t] -> Maybe Alignment -> IO (IOBitmap t)
combineChannels [] _ = error "bitmap/combineChannels: no channel data"
combineChannels bms malign = do
  let sizes = map bitmapSize bms
      nchns = map bitmapNChannels bms
      pixsizs = map bitmapPixelSizeInBytes bms 
      sumchn = sum nchns 
      siz@(w,h) = head sizes
      
  when (length (nub sizes) /= 1) $ error "bitmap/combineChannels: incompatible sizes"

  bm2 <- newIOBitmapUninitialized siz sumchn malign
  let pad2 = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  let loop = concatMap (\bm -> zip (repeat bm) [0..bitmapNChannels bm - 1]) bms

  withForeignPtr fptr2 $ \ptr2 -> do
    forM_ (zip [0..] loop) $ \(i,(bm1,j)) -> do
      let pad1  = bitmapRowPadding bm1
          fptr1 = bitmapPtr bm1    
          nchn1 = bitmapNChannels bm1    
      withForeignPtr fptr1 $ \ptr1 -> 
        c_extract_channel 
          (bitmapCType (head bms))
          (ci w) (ci h)
          ptr1 (ci nchn1)  (ci pad1) (ci j)
          ptr2 (ci sumchn) (ci pad2) (ci i)
          
  return bm2

extractChannelInto 
  :: PixelComponent t 
  => IOBitmap t   -- ^ source image
  -> Int          -- ^ source channel index 
  -> IOBitmap t   -- ^ target image
  -> Int          -- ^ target channel index
  -> IO ()
extractChannelInto bm1 ofs1 bm2 ofs2 = do

  let nchn1 = bitmapNChannels bm1
      siz1@(w,h) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  let nchn2 = bitmapNChannels bm2
      siz2  = bitmapSize bm2
      pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  when (siz1 /= siz2)          $ error "bitmap/extractChannelInto: incompatible dimensions"
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/extractChannelInto: invalid source channel index"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/extractChannelInto: invalid target channel index"
  
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_extract_channel 
        (bitmapCType bm1)
        (ci w) (ci h)
        ptr1 (ci nchn1) (ci pad1) (ci ofs1)
        ptr2 (ci nchn2) (ci pad2) (ci ofs2)

--------------------------------------------------------------------------------

bilinearResample
  :: PixelComponent t 
  => IOBitmap t      -- ^ source image
  -> Size            -- ^ target image size
  -> Maybe Alignment -- ^ target image alignment
  -> IO (IOBitmap t)   
bilinearResample bm1 siz2@(w2,h2) malign = do
  let nchn1 = bitmapNChannels bm1
  bm2 <- newIOBitmapUninitialized siz2 nchn1 malign
  forM_ [0..nchn1-1] $ \ofs ->
    bilinearResampleChannelInto bm1 ofs bm2 ofs
  return bm2


bilinearResampleChannel
  :: PixelComponent t 
  => IOBitmap t      -- ^ source image
  -> Int             -- ^ source channel index 
  -> Size            -- ^ target image size
  -> Maybe Alignment -- ^ target image alignment
  -> IO (IOBitmap t)   
bilinearResampleChannel bm1 ofs1 siz2@(w2,h2) malign = do
  let nchn1 = bitmapNChannels bm1
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/bilinearResampleChannel: invalid channel index"
  bm2 <- newIOBitmapUninitialized siz2 1 malign
  bilinearResampleChannelInto bm1 ofs1 bm2 0
  return bm2


bilinearResampleChannelInto 
  :: PixelComponent t 
  => IOBitmap t   -- ^ source image
  -> Int          -- ^ source channel index 
  -> IOBitmap t   -- ^ target image
  -> Int          -- ^ target channel index
  -> IO ()
bilinearResampleChannelInto bm1 ofs1 bm2 ofs2 = do

  let nchn1 = bitmapNChannels bm1
      siz1@(w1,h1) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  let nchn2 = bitmapNChannels bm2
      siz2@(w2,h2) = bitmapSize bm2
      pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/bilinearResampleChannelInto: invalid source channel index"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/bilinearResampleChannelInto: invalid target channel index"
  
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_bilinear_resample_channel 
        (c_type (bitmapUndefined bm1))
        (ci w1) (ci h1) ptr1 (ci nchn1) (ci pad1) (ci ofs1)
        (ci w2) (ci h2) ptr2 (ci nchn2) (ci pad2) (ci ofs2)

--------------------------------------------------------------------------------

-- | This is equivalent to @componentMap (\c -> c^gamma)@, except that
-- @(^)@ is defined only for integral exponents; but should be faster anyway.
powerlawGammaCorrection 
  :: PixelComponent t 
  => Float             -- ^ gamma
  -> IOBitmap t        -- ^ source bitmap
  -> Maybe Alignment   -- ^ target alignment
  -> IO (IOBitmap t)
powerlawGammaCorrection gamma bm1 malign = do
  let nchn1 = bitmapNChannels bm1
      siz1@(w1,h1) = bitmapSize bm1
      pad1 = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1
  bm2 <- newIOBitmapUninitialized siz1 nchn1 malign
  let pad2 = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_gamma_correct_all_channels 
        (c_type (bitmapUndefined bm1))
        (realToFrac gamma) 
        (ci w1) (ci h1) (ci nchn1) 
        ptr1 (ci pad1) 
        ptr2 (ci pad2) 
  return bm2
  

powerlawGammaCorrectionChannel
  :: PixelComponent t 
  => Float           -- ^ gamma
  -> IOBitmap t      -- ^ source image
  -> Int             -- ^ source channel index 
  -> Maybe Alignment -- ^ target image alignment
  -> IO (IOBitmap t)   
powerlawGammaCorrectionChannel gamma bm1 ofs1 malign = do
  let nchn1 = bitmapNChannels bm1
      siz1  = bitmapSize bm1
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/powerlawGammaCorrectionChannel: invalid channel index"
  bm2 <- newIOBitmapUninitialized siz1 1 malign
  powerlawGammaCorrectionChannelInto gamma bm1 ofs1 bm2 0
  return bm2
  
  
powerlawGammaCorrectionChannelInto
  :: PixelComponent t 
  => Float        -- ^ gamma
  -> IOBitmap t   -- ^ source image
  -> Int          -- ^ source channel index 
  -> IOBitmap t   -- ^ target image
  -> Int          -- ^ target channel index
  -> IO ()
powerlawGammaCorrectionChannelInto gamma bm1 ofs1 bm2 ofs2 = do

  let nchn1 = bitmapNChannels bm1
      siz1@(w1,h1) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  let nchn2 = bitmapNChannels bm2
      siz2@(w2,h2) = bitmapSize bm2
      pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  when (siz1 /= siz2)          $ error "bitmap/powerlawGammaCorrectionChannelInto: incompatible dimensions"
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/powerlawGammaCorrectionChannelInto: invalid source channel index"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/powerlawGammaCorrectionChannelInto: invalid target channel index"
  
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_gamma_correct_channel 
        (c_type (bitmapUndefined bm1))
        (realToFrac gamma) 
        (ci w1) (ci h1) 
        ptr1 (ci nchn1) (ci pad1) (ci ofs1)
        ptr2 (ci nchn2) (ci pad2) (ci ofs2)

--------------------------------------------------------------------------------

-- | Blends two bitmaps with the given weights; that is, the result is
-- the specified linear combination. If the values are outside the allowed
-- range (this can happen with the Word8, Word16, Word32 types and weights
-- whose sum is bigger than 1, or with a negative weight), then they are
-- clipped. The clipping /does not/ happen with the Float component type.
blendBitmaps
  :: PixelComponent t 
  => Float           -- ^ weight1
  -> Float           -- ^ weight2
  -> IOBitmap t      -- ^ source1 image 
  -> IOBitmap t      -- ^ source2 image 
  -> Maybe Alignment -- ^ target alignment
  -> IO (IOBitmap t)
-- this could be implemented more effectively by a specialized c routine
blendBitmaps weight1 weight2 bm1 bm2 malign = do 
  let nchn1 = bitmapNChannels bm1
      siz1  = bitmapSize bm1
  let nchn2 = bitmapNChannels bm2
      siz2  = bitmapSize bm2
  when (siz1  /= siz2 ) $ error "bitmap/blend: incompatible dimensions"    
  when (nchn1 /= nchn2) $ error "bitmap/blend: incompatible number of channels"
  bm3 <- newIOBitmapUninitialized siz1 nchn1 malign
  forM [0..nchn1-1] $ \ofs -> 
    blendChannelsInto weight1 weight2 bm1 ofs bm2 ofs bm3 ofs
  return bm3  
  
blendChannels
  :: PixelComponent t 
  => Float        -- ^ weight1
  -> Float        -- ^ weight2
  -> IOBitmap t   -- ^ source1 image 
  -> Int          -- ^ source1 channel index  
  -> IOBitmap t   -- ^ source2 image 
  -> Int          -- ^ source2 channel index  
  -> Maybe Alignment -- ^ target alignment
  -> IO (IOBitmap t)
blendChannels weight1 weight2 bm1 ofs1 bm2 ofs2 malign = do
  let nchn1 = bitmapNChannels bm1
      siz1  = bitmapSize bm1
  let nchn2 = bitmapNChannels bm2
      siz2  = bitmapSize bm2
  when (siz1 /= siz2)          $ error "bitmap/blendChannels: incompatible dimensions"    
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/blendChannels: invalid channel index"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/blendChannels: invalid channel index"
  bm3 <- newIOBitmapUninitialized siz1 1 malign
  blendChannelsInto weight1 weight2 bm1 ofs1 bm2 ofs2 bm3 0
  return bm3  
  
blendChannelsInto
  :: PixelComponent t 
  => Float        -- ^ weight1
  -> Float        -- ^ weight2
  -> IOBitmap t   -- ^ source1 image 
  -> Int          -- ^ source1 channel index  
  -> IOBitmap t   -- ^ source2 image 
  -> Int          -- ^ source2 channel index  
  -> IOBitmap t   -- ^ target image
  -> Int          -- ^ target channel index
  -> IO ()
blendChannelsInto weight1 weight2 bm1 ofs1 bm2 ofs2 bm3 ofs3 = do

  let nchn1 = bitmapNChannels bm1
      siz1@(w1,h1) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  let nchn2 = bitmapNChannels bm2
      siz2@(w2,h2) = bitmapSize bm2
      pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  let nchn3 = bitmapNChannels bm3
      siz3@(w3,h3) = bitmapSize bm3
      pad3  = bitmapRowPadding bm3
      fptr3 = bitmapPtr bm3

  when (siz1 /= siz2)          $ error "bitmap/blendChannelInto: incompatible dimensions"
  when (siz2 /= siz3)          $ error "bitmap/blendChannelInto: incompatible dimensions"
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/blendChannelInto: invalid source channel index 1"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/blendChannelInto: invalid source channel index 2"
  when (ofs3<0 || ofs3>=nchn3) $ error "bitmap/blendChannelInto: invalid target channel index"
  
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      withForeignPtr fptr3 $ \ptr3 ->     
        c_linear_combine_channels 
          (bitmapCType bm1)
          (realToFrac weight1) (realToFrac weight2) 
          (ci w1) (ci h1) 
          ptr1 (ci nchn1) (ci pad1) (ci ofs1)
          ptr2 (ci nchn2) (ci pad2) (ci ofs2)
          ptr3 (ci nchn3) (ci pad3) (ci ofs3)
        
--------------------------------------------------------------------------------

{- 

-- | The data is copied, not shared. Note that the resulting ByteString is
-- encoded using the host machine's endianness, so it may be not compatible
-- across different architectures!
copyBitmapToByteString :: PixelComponent t => Bitmap t -> IO ByteString
copyBitmapToByteString bm = do
  let n = bitmapSizeInBytes bm
  newfp <- B.mallocByteString n
  withBitmap bm $ \_ _ _ src -> 
    withForeignPtr newfp $ \tgt -> do
      c_memcpy (castPtr src) tgt n
  return $ B.fromForeignPtr (castForeignPtr newfp) 0 n

-- | The data is copied, not shared.
-- Note that we expect the ByteString to be encoded
-- encoded using the host machine's endianness.
copyBitmapFromByteString :: PixelComponent t => ByteString -> Size -> NChn -> Padding -> IO (Bitmap t)
copyBitmapFromByteString bs siz nchn pad = do
  let (bsfptr0,ofs,len) = B.toForeignPtr bs
      bm = Bitmap 
        { bitmapSize = siz
        , bitmapNChannels = nchn
        , bitmapPtr = undefined 
        , bitmapRowPadding = pad
        , bitmapRowAlignment = 1
        } -- :: Bitmap t
      n = bitmapSizeInBytes bm
  if n > len-ofs
    then error "copyBitmapFromByteString: ByteString is too short"
    else do
      newfptr <- mallocForeignPtrBytes n
      withForeignPtr bsfptr0 $ \src0 -> do
        let src = src0 `myPlusPtr` ofs
        withForeignPtr newfptr $ \tgt ->
          c_memcpy src tgt n
      return $ bm { bitmapPtr = castForeignPtr newfptr } 

-}
  
--------------------------------------------------------------------------------

ptrUndefined :: Ptr a -> a
ptrUndefined _ = undefined

-- no multiplication
{-# SPECIALIZE advancePtr1 :: Ptr Word8 -> Ptr Word8 #-}
{-# SPECIALIZE advancePtr1 :: Ptr Float -> Ptr Float #-}
--advancePtr1 :: forall a. Storable a => Ptr a -> Ptr a
--advancePtr1 p = p `plusPtr` (sizeOf (undefined::a))
advancePtr1 :: Storable a => Ptr a -> Ptr a
advancePtr1 p = p `plusPtr` (sizeOf (ptrUndefined p))

-- restricted type
{-# SPECIALIZE myPlusPtr :: Ptr Word8 -> Int -> Ptr Word8 #-}
{-# SPECIALIZE myPlusPtr :: Ptr Float -> Int -> Ptr Float #-}
myPlusPtr :: Ptr a -> Int -> Ptr a
myPlusPtr = plusPtr

ci :: Int -> CInt
ci = fromIntegral

-- @c_memset target count fill@.
-- Note that we use /nonstandard/ argument order!
foreign import ccall unsafe "bm.h c_memset" 
  c_memset :: Ptr Word8 -> Int -> Word8 -> IO ()

-- @c_memcpy from to cnt@.
-- Note that we use /nonstandard/ argument order!
foreign import ccall unsafe "bm.h c_memcpy" 
  c_memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

--------------------

{-
// tgt and src can be potentally the same
void c_mirror_line(int width, int bytesperpixel, void *src, void *tgt);
-}

foreign import ccall unsafe "bm.h c_mirror_line"
  c_mirror_line 
    :: CInt        -- ^ width
    -> CInt        -- ^ bytesperpixel
    -> Ptr a       -- ^ src 
    -> Ptr a       -- ^ tgt
    -> IO ()

--------------------

{-
void c_cast_bitmap
  ( int k_type1, int k_type2
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  );
-}

-- offset is measured in components, not bytes!
-- also, nchn1 should be equal to nchn2
-- offset should be zero
foreign import ccall unsafe "bm.h c_cast_bitmap"
  c_cast_bitmap 
    :: CInt -> CInt      -- ^ component types
    -> CInt -> CInt      -- ^ width, height
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source, nchn, pad, offset
    -> Ptr b -> CInt -> CInt -> CInt  -- ^ target, nchn, pad, offset
    -> IO ()

--------------------

{-
void c_extract_channel(
  ( int k_type
  , int width, int height 
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  );
-}

-- offset is measured in components, not bytes!
foreign import ccall unsafe "bm.h c_extract_channel"
  c_extract_channel 
    :: CInt              -- ^ component type
    -> CInt -> CInt      -- ^ width, height
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source, nchn, pad, offset
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ target, nchn, pad, offset
    -> IO ()

--------------------
 
{-
void c_bilinear_resample_channel
  ( int k_type
  , int width1, int height1, void *p1, int nchn1, int pad1, int ofs1 
  , int width2, int height2, void *p2, int nchn2, int pad2, int ofs2 
  );       
-}

-- offset is measured in components, not bytes!
foreign import ccall unsafe "bm.h c_bilinear_resample_channel"
  c_bilinear_resample_channel 
    :: CInt                                           -- ^ component type
    -> CInt -> CInt -> Ptr a -> CInt -> CInt -> CInt  -- ^ width, height, source, nchn, pad, offset
    -> CInt -> CInt -> Ptr a -> CInt -> CInt -> CInt  -- ^ width, height, target, nchn, pad, offset
    -> IO ()

--------------------

{-
void c_gamma_correct_channel
  ( int k_type
  , float gamma
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  );
  
void c_gamma_correct_all_channels
  ( int k_type
  , float gamma
  , int width, int height, int nchn
  , void *p1, int pad1 
  , void *p2, int pad2 
  );
-}
 
-- offset is measured in components, not bytes!
foreign import ccall unsafe "bm.h c_gamma_correct_channel"
  c_gamma_correct_channel 
    :: CInt              -- ^ component type
    -> CFloat            -- ^ gamma
    -> CInt -> CInt      -- ^ width, height
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source, nchn, pad, offset
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ target, nchn, pad, offset
    -> IO ()

foreign import ccall unsafe "bm.h c_gamma_correct_all_channels"
  c_gamma_correct_all_channels 
    :: CInt                 -- ^ component type
    -> CFloat               -- ^ gamma
    -> CInt -> CInt -> CInt -- ^ width, height, nchn
    -> Ptr a -> CInt        -- ^ source, pad
    -> Ptr a -> CInt        -- ^ target, pad
    -> IO ()

--------------------
    
{-
void c_linear_combine_channels
  ( int k_type
  , float weight1, float weight2
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  , void *p3, int nchn3, int pad3, int ofs3 
  );    
-}

-- offset is measured in components, not bytes!
foreign import ccall unsafe "bm.h c_linear_combine_channels"
  c_linear_combine_channels 
    :: CInt              -- ^ component type
    -> CFloat -> CFloat  -- ^ weight1, weight2 
    -> CInt -> CInt      -- ^ width, height
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source1, nchn, pad, offset
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source2, nchn, pad, offset
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ target,  nchn, pad, offset
    -> IO ()

--------------------------------------------------------------------------------
