
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap
-- Version     : 0.0.2
-- License     : BSD3
-- Copyright   : (c) 2009-2010 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI and CPP
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

-- | A library to handle bitmaps (uncompressed pixel rectangles).
-- The smallest storage unit is 1 byte (thus /bit/maps, in the literal sense
-- of the word, are not supported).
--
-- For loading JPEG/PNG images into 'Bitmap's, see the @stb-image@ 
-- library (version 0.2 or newer):
-- <http://hackage.haskell.org/package/stb-image>.
-- 
-- Terminology: 
-- Pixels are made out of one or more \"components\". These components 
-- are also referred as \"channels\"; for example a color image could be made out 
-- of three channels, the red, green and blue one. The components can be unsigned
-- bytes, words, dwords, or floats. The pixels are stored in horizontal order,
-- and the channels are interleaved: That is, the structure of an RGB image is 
-- @R0 G0 B0 R1 G1 B1 ...@. 
-- Most of the library is indifferent to the meaning of different channels.
--
-- \"Padding\" refers to unused bytes at the end of each row. This is sometimes
-- necessary because other software components want the rows aligned to machine
-- word boundary, for example. 
--
-- The library should be relatively fast (except where noted), but 
-- performance is not the primary goal (thus there is no inline assembly,
-- no SSE, etc.; but the critical functions are coded in C).
--
-- There are both pure and mutable IO versions of the API (ST is also planned).
-- This module re-exports the simplified pure interface.
--

module Data.Bitmap 
  ( module Data.Bitmap.Simple
  ) 
  where

--------------------------------------------------------------------------------

import Data.Bitmap.Simple

--------------------------------------------------------------------------------

