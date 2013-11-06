
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.Internal
-- Version     : 0.0.2
-- License     : BSD3
-- Copyright   : (c) 2009-2010 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI and CPP
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
module Data.Bitmap.Internal where

--------------------------------------------------------------------------------

import Control.Monad

--import Data.Array.IArray
import Data.Word

import Foreign
import Foreign.C

--------------------------------------------------------------------------------

data PixelComponentType 
  = PctWord8 
  | PctWord16
  | PctWord32 
  | PctFloat
  deriving (Show,Eq)
  
pixelComponentSize :: PixelComponentType -> Int
pixelComponentSize pct = case pct of
  PctWord8  -> 1 
  PctWord16 -> 2 
  PctWord32 -> 4 
  PctFloat  -> 4 

prettyPrintPixelComponentType :: PixelComponentType -> String
prettyPrintPixelComponentType t = case t of
  PctWord8  -> "Word8"
  PctWord16 -> "Word16"
  PctWord32 -> "Word32"
  PctFloat  -> "Float"

--------------------------------------------------------------------------------

class (Num t, Storable t) => PixelComponent t where
  c_type :: t -> CInt
--  nbytes :: t -> Int
--  nbytes x = sizeOf x
  toFloat   :: t -> Float
  fromFloat :: Float -> t
  
pixelComponentType :: PixelComponent t => t -> PixelComponentType
pixelComponentType t = decodeCType (c_type t)
  
decodeCType :: CInt -> PixelComponentType 
decodeCType k = case k of
  1 -> PctWord8
  2 -> PctWord16
  3 -> PctWord32
  4 -> PctFloat

-- hmm hmm let's hope ghc will inline this into an 
-- inlined function if i explicitely ask for it... 
{-# INLINE clamp #-}
clamp :: Float -> Float
clamp = min 1 . max 0
 
instance PixelComponent Word8 where 
  {-# SPECIALIZE instance PixelComponent Word8  #-}
  c_type _ = 1
  fromFloat = floor . (+0.5) . (*255) . min 1 . max 0
  toFloat = (*3.92156862745098e-3) . fromIntegral     -- 1/255

instance PixelComponent Word16 where 
  {-# SPECIALIZE instance PixelComponent Word16 #-}
  c_type _ = 2
  fromFloat = floor . (+0.5) . (*65535) . min 1 . max 0
  toFloat = (*1.5259021896696422e-5) . fromIntegral   -- 1/65535

instance PixelComponent Word32 where 
  {-# SPECIALIZE instance PixelComponent Word32 #-}
  c_type _ = 3
  fromFloat = floor . (+0.5) . (*4294967295) . min 1 . max 0
  toFloat = (*2.3283064370807974e-10) . fromIntegral  -- 1/(2^32-1)
  
instance PixelComponent Float where 
  {-# SPECIALIZE instance PixelComponent Float  #-}
  c_type _ = 4
  fromFloat = id
  toFloat = id

--------------------------------------------------------------------------------

-- this is for portability, to avoid ScopedTypeVariables

bitmapUndefined :: BitmapClass bitmap => bitmap t -> t
bitmapUndefined _ = undefined  

bitmapCType :: (BitmapClass bitmap, PixelComponent t) => bitmap t -> CInt
bitmapCType = c_type . bitmapUndefined
  
--------------------------------------------------------------------------------

{-
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
-}

--------------------------------------------------------------------------------

-- | A class so that using newtypes is convenient.
class BitmapClass b where
  underlyingBitmap :: b t -> Bitmap t    -- ?? better name ??

instance BitmapClass Bitmap where
  underlyingBitmap = id

{-
instance BitmapClass Bitmap1 where
  underlyingBitmap = fromBitmap1

instance BitmapClass Bitmap2 where
  underlyingBitmap = fromBitmap2

instance BitmapClass Bitmap3 where
  underlyingBitmap = fromBitmap3

instance BitmapClass Bitmap4 where
  underlyingBitmap = fromBitmap4
-}

--------------------------------------------------------------------------------

-- A fixed channel of a bitmap
data BitmapChannel t = BmChn (Bitmap t) Int

data IOBitmapChannel t = IOBmChn (IOBitmap t) Int
data STBitmapChannel t = STBmChn (STBitmap t) Int

--------------------------------------------------------------------------------

-- to provide better documentation
type Size   = (Int,Int)
type Offset = (Int,Int)
type NChn      = Int
type Padding   = Int
type Alignment = Int

-- | A bitmap.
data Bitmap t = Bitmap
  { _bitmapSize      :: Size          -- ^ (width,height)
  , _bitmapNChannels :: NChn          -- ^ number of channels (eg. 3 for RGB)
  , _bitmapPtr  :: ForeignPtr t       -- ^ pointer to the data
  , _bitmapRowPadding   :: Padding    -- ^ the padding of the rows, measured in /bytes/
  , _bitmapRowAlignment :: Alignment  -- ^ the alignment of the rows (in bytes)
  }
--  deriving Show

--------------------------------------------------------------------------------

-- | A mutable Bitmap in the IO Monad. Only the content is mutable, the shape isn't.
newtype IOBitmap t = IOBitmap { unIOBitmap :: Bitmap t }
-- | A mutable Bitmap in the ST Monad. Only the content is mutable, the shape isn't.
newtype STBitmap t = STBitmap { unSTBitmap :: Bitmap t }

instance BitmapClass IOBitmap where underlyingBitmap = unIOBitmap
instance BitmapClass STBitmap where underlyingBitmap = unSTBitmap

--------------------------------------------------------------------------------

bitmapSize :: BitmapClass bitmap => bitmap t -> Size
bitmapSize = _bitmapSize . underlyingBitmap  

bitmapNChannels :: BitmapClass bitmap => bitmap t -> NChn
bitmapNChannels = _bitmapNChannels . underlyingBitmap  

bitmapPtr :: BitmapClass bitmap => bitmap t -> ForeignPtr t
bitmapPtr = _bitmapPtr . underlyingBitmap  

bitmapRowPadding :: BitmapClass bitmap => bitmap t -> Padding
bitmapRowPadding = _bitmapRowPadding . underlyingBitmap  

bitmapRowAlignment :: BitmapClass bitmap => bitmap t -> Alignment
bitmapRowAlignment = _bitmapRowAlignment . underlyingBitmap  

--------------------------------------------------------------------------------

bitmapComponentType :: (BitmapClass bitmap, PixelComponent t) => bitmap t -> PixelComponentType
bitmapComponentType bm = pixelComponentType (bitmapUndefined bm)

bitmapComponentSizeInBytes :: (BitmapClass bitmap, PixelComponent t) => bitmap t -> Int
bitmapComponentSizeInBytes bm = sizeOf (bitmapUndefined bm) 

bitmapPixelSizeInBytes :: (BitmapClass bitmap, PixelComponent t) => bitmap t -> Int
bitmapPixelSizeInBytes bm = bitmapNChannels bm * bitmapComponentSizeInBytes bm
  
bitmapUnpaddedRowSizeInBytes :: (BitmapClass bitmap, PixelComponent t) => bitmap t -> Int  
bitmapUnpaddedRowSizeInBytes bm = w * sizeOf (bitmapUndefined bm) * nchn where
  (w,h) = bitmapSize bm
  nchn  = bitmapNChannels bm   
  
bitmapPaddedRowSizeInBytes :: (BitmapClass bitmap, PixelComponent t) => bitmap t -> Int  
bitmapPaddedRowSizeInBytes bm = bitmapUnpaddedRowSizeInBytes bm + bitmapRowPadding bm 
  
bitmapSizeInBytes :: (BitmapClass bitmap, PixelComponent t) => bitmap t -> Int 
bitmapSizeInBytes bm = h*x where
  x = bitmapPaddedRowSizeInBytes bm
  (_,h) = bitmapSize bm  
 
-- | The width divided by the height.
bitmapAspect :: (Fractional a, BitmapClass bitmap) => bitmap t -> a
bitmapAspect bm = (fromIntegral x / fromIntegral y) where
  (x,y) = bitmapSize bm

--------------------------------------------------------------------------------

prettyPrintBitmap :: (BitmapClass bitmap, PixelComponent t) => String -> bitmap t -> String
prettyPrintBitmap prefix bm = text where
  text = "<" ++ prefix ++ " " ++ typ ++ ", " ++ show xres ++ "x" ++ show yres ++ ", " ++ show nchn ++ " channels>" where
  (xres,yres) = bitmapSize bm
  typ = prettyPrintPixelComponentType (bitmapComponentType bm)
  nchn = bitmapNChannels bm

instance PixelComponent t => Show (Bitmap t) where
  show = prettyPrintBitmap "Bitmap"

--------------------------------------------------------------------------------

-- | @withBitmap bitmap $ \\(w,h) nchn padding ptr -> ...@
withBitmap :: PixelComponent t => Bitmap t -> (Size -> NChn -> Padding -> Ptr t -> IO a) -> IO a
withBitmap bm action = 
  withForeignPtr (bitmapPtr bm) $ \p -> 
    action (bitmapSize bm) (bitmapNChannels bm) (bitmapRowPadding bm) p

bitmapFromForeignPtrUnsafe 
  :: PixelComponent t 
  => Size -> NChn -> Alignment -> Padding -> ForeignPtr t -> Bitmap t
bitmapFromForeignPtrUnsafe siz nchn align pad fptr = Bitmap
  { _bitmapSize      = siz 
  , _bitmapNChannels = nchn 
  , _bitmapPtr       = fptr 
  , _bitmapRowPadding   = pad 
  , _bitmapRowAlignment = align
  }
    
--------------------------------------------------------------------------------
  
{-# SPECIALIZE isValidAlignment :: Int -> Bool #-}
isValidAlignment :: Integral a => a -> Bool
isValidAlignment a = elem a [1,2,4,8,16]

--------------------------------------------------------------------------------

-- we mimic the OpenGL padding at the moment
recommendedPadding :: (BitmapClass bitmap, PixelComponent t) => bitmap t -> Int
recommendedPadding bm = pad where
  (w,_) = bitmapSize bm
  n = bitmapNChannels bm
  b = bitmapRowAlignment bm
  s = sizeOf (bitmapUndefined bm)
  a = if b<s then s else b
  k = case divMod a s of (q,r) -> if r==0 then q else error "recommendedPadding: should not happen"
  pad = s * ( k * div (n*w + k-1) k - n*w )    
  
{-
-- OpenGL padding algorithm test
recommendedPadding' 
  :: Int  -- ^ number of channels
  -> Int  -- ^ size of a component
  -> Int  -- ^ width of the picture
  -> Int  -- ^ target alignment
  -> (Int,Int)
recommendedPadding' n s w b = (pad,pad2) where  
  a = if b<s then s else b
  k = case divMod a s of (q,r) -> if r==0 then q else error "recommendedPadding': should not happen"
  pad = s * ( k * div (n*w + k-1) k - n*w )    

  kk = a * (div (s*n*w + a-1) a)
  pad2 = kk - w*s*nn
-}
  
--------------------------------------------------------------------------------
