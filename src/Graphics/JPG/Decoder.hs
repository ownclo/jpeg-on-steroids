{-# LANGUAGE BangPatterns #-}
module Graphics.JPG.Decoder
    ( decodeJPG
    ) where

import Graphics.JPG.Env

import Data.Monoid
import qualified Data.ByteString.Char8 as B8

type BS = B8.ByteString
type Image = [[Int]] -- XXX: Temporary

decodeJPG :: Env -> BS -> Either String Env
decodeJPG !e _ = return e

-- NOTE: BS.cons is O(n), but the rest is fast.
-- Can we convert to lazy byteStrings and then back?
skipPadded :: BS -> BS
skipPadded s = sub <> rest where
    (sub, next) = B8.break (=='\xFF') s
    next' = B8.drop 1 next
    rest = case B8.uncons next' of
                Just ('\x00', xs) -> '\xFF' `B8.cons` skipPadded xs
                Just (_, xs) -> skipPadded xs -- skipping the restart marker
                Nothing  -> B8.empty
