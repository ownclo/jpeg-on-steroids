-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.JPG where

import Control.Applicative

import Data.Word
import Data.Char
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS

-- Here will be a JPEG parser.
-- It will be used on the decoder side.

type Frame = BS.ByteString
data Marker = SOI | EOI
    deriving Show

byte :: Char -> Parser Word8
byte = char8

marker :: Parser Marker
marker = do
      byte '\xFF'
      choice [
          byte '\xD8' >> return SOI,
          byte '\xD9' >> return EOI ]

main :: IO ()
main = do
    contents <- BS.readFile "img/sample.jpg"
    print $ parseOnly marker contents
