
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.Pure.File
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
-- See the module "Data.Bitmap.IO.File" for the file format.
--

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Data.Bitmap.Pure.File
  ( readBitmap
  , writeBitmap
  , readRawData
  , writeRawData
  ) 
  where

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad
import Foreign
import System.IO

import Data.Bitmap.Base
import Data.Bitmap.Internal
import Data.Bitmap.IO
import qualified Data.Bitmap.IO.File as IO

--------------------------------------------------------------------------------

readBitmap :: PixelComponent t => FilePath -> IO (Bitmap t)
readBitmap fpath = unIOBitmap <$> IO.readBitmap fpath
  
readRawData :: PixelComponent t => FilePath -> (Size,NChn,PixelComponentType) -> IO (Bitmap t) 
readRawData fpath header = unIOBitmap <$> IO.readRawData fpath header

--------------------------------------------------------------------------------

writeBitmap :: PixelComponent t => FilePath -> Bitmap t -> IO ()
writeBitmap fpath bm = IO.writeBitmap fpath (IOBitmap bm)

-- | Saves only the raw pixel data, no resolution etc.
writeRawData :: PixelComponent t => FilePath -> Bitmap t -> IO ()
writeRawData fpath bm = IO.writeRawData fpath (IOBitmap bm)

--------------------------------------------------------------------------------

