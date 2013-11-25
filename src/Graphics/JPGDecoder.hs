module Graphics.JPGDecoder where

import Data.Monoid
import qualified Data.ByteString.Char8 as BS


-- NOTE: BS.cons is O(n), but the rest is fast.
skipPadded :: BS.ByteString -> BS.ByteString
skipPadded s = sub <> rest where
    (sub, next) = BS.break (=='\xFF') s
    rest = case BS.uncons next of
                Just ('\x00', xs) -> '\xFF' `BS.cons` skipPadded xs
                Just (_, xs) -> skipPadded xs -- skipping the restart marker
                Nothing  -> BS.empty
