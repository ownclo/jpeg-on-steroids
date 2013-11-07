-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.JPG where

import Debug.Trace (trace)
import Numeric (showHex)

import Control.Applicative
import Control.Monad
import Prelude hiding(take)

import Data.Word ()
import Data.Char
import Data.Attoparsec.Char8
import Data.Attoparsec.Number ()
import qualified Data.ByteString.Char8 as BS

----------------------------
-------- TYPES -------------
----------------------------
type Frame = [Segment]
type Segment = BS.ByteString

type Table a = [a] --- XXX: Temporary!
type BC = Int
type Run = Int
data HuffTree a = Node (HuffTree a) (HuffTree a) | Leaf a

type DCHuffTable = Table (HuffTree BC)
type ACHuffTable = Table (HuffTree (Run, BC))
type QTable = [[Int]] --- XXX: Temporary!

-- Environment will be updated by headers.
data Env =
     Env {
         huffTables :: (DCHuffTable, ACHuffTable),
         quanTables :: Table QTable,
         frameTable :: Table BS.ByteString
     }

data SegmentType = HuffSpec
                 | QuaSpec
                 | FrameHeader
                 | ScanHeader
                 | Unsupported

-- supported markers
data Marker = SOI -- start of input
            | EOI -- end of input
            | SOF -- start of frame
    deriving Show

markerCode :: Marker -> Char
markerCode SOI = '\xD8'
markerCode EOI = '\xD9'
markerCode SOF = '\xC0'


---------------------------
--------- PARSERS --------- 
---------------------------

byte :: Char -> Parser ()
byte = void . char

anyByte :: Parser Int
anyByte = ord <$> anyChar

marker :: Marker -> Parser ()
marker m = void $ do
    byte '\xFF'
    byte $ markerCode m

getMarker :: Parser Int
getMarker = byte '\xFF' >> anyByte

word :: Parser Int
word = do
    a <- anyByte
    b <- anyByte
    return $ a * 256 + b

segment :: Parser Segment
segment = do
   m <- getMarker
   l <- word
   trace ("Marker: " ++ showHex m " " ++
          "Length: " ++ show l) $
       take (l-2)

-- frame :: Parser Env
frame :: Parser Frame
frame = many segment

jpegImage :: Parser Frame
jpegImage = marker SOI >> frame

main :: IO ()
main = do
    contents <- BS.readFile "img/sample.jpg"
    print $ parseOnly jpegImage contents
