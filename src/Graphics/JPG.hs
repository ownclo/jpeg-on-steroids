-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.JPG where

import Debug.Trace
import Numeric

import Control.Applicative
import Control.Monad
import Prelude hiding(take)

import Data.Word
import Data.Char
import Data.Attoparsec.Char8
import Data.Attoparsec.Number
import qualified Data.ByteString.Char8 as BS

type Frame = BS.ByteString

-- supported markers
data Marker = SOI -- start of input
            | EOI -- end of input
            | SOF -- start of frame
    deriving Show

markerCode :: Marker -> Char
markerCode SOI = '\xD8'
markerCode EOI = '\xD9'
markerCode SOF = '\xC0'

byte = char
anyByte = anyChar

marker :: Marker -> Parser Char
marker m = byte '\xFF' >> byte (markerCode m)

getMarker :: Parser Char
getMarker = byte '\xFF' >> anyByte

word :: Parser Int
word = do
    a <- anyByte
    b <- anyByte
    return $ ord(a)*256 + ord(b)

frame :: Parser Frame
frame = marker SOF >> return "LOL"

segment :: Parser BS.ByteString
segment = do
   m <- getMarker
   l <- word
   trace ("Marker: " ++ showHex (ord m) " " ++
          "Length: " ++ show l) $
       take (l-2)

jpegImage :: Parser [Frame]
jpegImage = marker SOI >> many segment

main :: IO ()
main = do
    contents <- BS.readFile "img/sample.jpg"
    print $ parseOnly jpegImage contents
