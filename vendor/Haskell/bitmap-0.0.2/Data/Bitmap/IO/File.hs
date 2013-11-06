
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.IO.File
-- Version     : 0.0.2
-- License     : BSD3
-- Copyright   : (c) 2009-2010 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI and CPP
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

-- | Saving and loading uncompressed bitmaps.
-- For loading from compressed formats, see the @stb-image@ library:
-- <http://hackage.haskell.org/package/stb-image>.
--
-- The goal of this module is to provide the simplest possible interface 
-- for loading and saving bitmaps; so you can start experimenting
-- without much hassle.
-- 
-- Note: Endianness is the endianness of the host, so the resulting file is 
-- not portable across platforms with different endiannesses.
-- 
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Data.Bitmap.IO.File
  ( readBitmap
  , writeBitmap
    --
  , readRawData
  , writeRawData
    --
  , hPutHeader
  , hPutRawData
  , hGetHeader
  , hGetRawData
  ) 
  where

--------------------------------------------------------------------------------

import Control.Monad
import System.IO

--import Foreign     -- GHC 7 complains?
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal

import Data.Int

import Data.Bitmap.Base
import Data.Bitmap.Internal
import Data.Bitmap.IO

--------------------------------------------------------------------------------

readBitmap :: PixelComponent t => FilePath -> IO (IOBitmap t)
readBitmap fpath = do
  h <- openFile fpath ReadMode
  header <- hGetHeader h
  bitmap <- hGetRawData h header
  hClose h
  return bitmap
  
readRawData :: PixelComponent t => FilePath -> (Size,NChn,PixelComponentType) -> IO (IOBitmap t) 
readRawData fpath header = do 
  h <- openFile fpath ReadMode
  bitmap <- hGetRawData h header
  hClose h
  return bitmap

--------------------------------------------------------------------------------

writeBitmap :: PixelComponent t => FilePath -> IOBitmap t -> IO ()
writeBitmap fpath bm = do
  h <- openFile fpath WriteMode
  hPutHeader  h bm
  hPutRawData h bm
  hClose h

-- | Saves only the raw pixel data, no resolution etc.
writeRawData :: PixelComponent t => FilePath -> IOBitmap t -> IO ()
writeRawData fpath bm = do
  h <- openFile fpath WriteMode
  hPutRawData h bm
  hClose h

--------------------------------------------------------------------------------

-- | Writes a 16 byte header in the following format:
-- 
-- > dword xsize
-- > dword ysize
-- > dword nchn
-- > dword pixelcomponent_type
--
-- Pixel component encoding is the following:
-- 
-- * 1 = Word8
--
-- * 2 = Word16
--
-- * 3 = Word32
--
-- * 4 = Float
--
-- Endianness is the endianness of the host, so the resulting file is 
-- not portable across platform with different endiannesses.
hPutHeader :: PixelComponent t => Handle -> IOBitmap t -> IO ()
hPutHeader h bm = do
  let (xsize,ysize) = bitmapSize bm
      nchn = bitmapNChannels bm
      typ  = bitmapCType bm
  with (fromIntegral xsize :: Int32) $ \p -> hPutBuf h p 4 
  with (fromIntegral ysize :: Int32) $ \p -> hPutBuf h p 4 
  with (fromIntegral nchn  :: Int32) $ \p -> hPutBuf h p 4 
  with (fromIntegral typ   :: Int32) $ \p -> hPutBuf h p 4 
  
-- | Saves only the raw pixel data, no resolution etc.
hPutRawData :: PixelComponent t => Handle -> IOBitmap t -> IO ()
hPutRawData h bm = 
  withIOBitmap bm $ \(xres,yres) nchn padding ptr -> do
    forM_ [0..yres-1] $ \k -> do
      let q = ptr `plusPtr` (k*long)
      hPutBuf h q short    
  where
    long  = bitmapPaddedRowSizeInBytes   bm
    short = bitmapUnpaddedRowSizeInBytes bm
    
--------------------------------------------------------------------------------
    
hGetRawData :: PixelComponent t => Handle -> (Size,NChn,PixelComponentType) -> IO (IOBitmap t)
hGetRawData h (siz,nchn,pct) = do
  bm <- newIOBitmapUninitialized siz nchn (Just 1)
  if bitmapComponentType bm /= pct
    then error "bitmap/getRawData: bitmap component type does not match"
    else do
      withIOBitmap bm $ \(_,ysiz) _ _ ptr -> do
        let n = ysiz * bitmapUnpaddedRowSizeInBytes bm
        k <- hGetBuf h ptr n
        when (k/=n) $ error "bitmap/getRawData: not enough data"
        return bm 
    
hGetHeader :: Handle -> IO (Size,NChn,PixelComponentType)
hGetHeader h = do
  xsize <- loadInt32
  ysize <- loadInt32
  nchn  <- loadInt32
  ctyp  <- loadInt32
  return 
    ( (fromIntegral xsize, fromIntegral ysize)
    , fromIntegral nchn
    , decodeCType (fromIntegral ctyp)
    )
  where
    loadInt32 :: IO Int32
    loadInt32 = alloca $ \p -> do
      hGetBuf h p 4
      peek p
    
--------------------------------------------------------------------------------

    