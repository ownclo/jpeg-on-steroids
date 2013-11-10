-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.JPG where

import Debug.Trace (trace)
import Numeric (showHex)

import Control.Applicative
import Control.Monad.State
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
data HuffTree a = Node (HuffTree a) (HuffTree a)
                | Leaf a
                deriving Show

type DCHuffTable = Table (HuffTree BC)
type ACHuffTable = Table (HuffTree (Run, BC))
type QTable = [[Int]] --- XXX: Temporary!

-- Environment will be updated by headers.
data Env =
     Env {
         huffTables :: (DCHuffTable, ACHuffTable),
         quanTables :: Table QTable,
         frameTable :: Table BS.ByteString
     } deriving Show

--- ENVIRONMENT MANAGEMENT ---
type EnvParser = StateT Env Parser

parseEnv :: EnvParser a -> BS.ByteString -> Either String (a, Env)
parseEnv f = parseOnly $ runStateT f initialEnv
    where initialEnv = Env {
               huffTables = (emptyTable, emptyTable)
              ,quanTables = emptyTable
              ,frameTable = emptyTable}
          emptyTable = []

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

frame :: Parser Frame
frame = many segment

jpegImage :: Parser Frame
jpegImage = marker SOI >> frame

parseQuanTable :: Parser QTable
parseQuanTable = guard False >> return []

startOfFrame :: Parser BS.ByteString
startOfFrame = do
        marker SOF
        l <- word
        take (l-2)

addQuanTable :: EnvParser ()
addQuanTable = do
        table <- lift parseQuanTable
        modify $ addTable table
            where addTable t env = env {
                        quanTables = t:quanTables env
            }

addFrameDesc :: EnvParser ()
addFrameDesc = do
    s <- lift startOfFrame
    modify $ addString s
        where addString s env = env {
                        frameTable = s:frameTable env
        }

skipSegment :: EnvParser ()
skipSegment = void $ lift segment

jpegHeader :: EnvParser ()
jpegHeader = void $ do
    lift $ marker SOI
    many $ addQuanTable <|> addFrameDesc <|> skipSegment

-- TODO: - use lenses for accessing fields of Env record.
--       - create `choice` function for EnvParser.
main :: IO ()
main = do
    contents <- BS.readFile "img/sample.jpg"
    print $ parseEnv jpegHeader contents
--     print $ parseOnly jpegImage contents
