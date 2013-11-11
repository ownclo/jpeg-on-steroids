{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.JPG where

import Debug.Trace (trace)
import Numeric (showHex)

import Control.Applicative
import Control.Monad.State
import Prelude hiding(take, id)

import Control.Lens(makeLenses, (.=), (%=))
import Data.Word (Word8, Word16)
import Data.Char
import Data.Attoparsec.Char8
import Data.Attoparsec.Number ()
import qualified Data.ByteString.Char8 as BS

----------------------------
-------- TYPES -------------
----------------------------
type Byte = Word8
type Word = Word16

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

data Dim a = Dim {
           _y :: !a, _x :: !a
    } deriving Show

toDim :: (a, a) -> Dim a
toDim (!y, !x) = Dim y x

data CompSpec = CompSpec {
              _id :: {-# UNPACK #-} !Byte, -- component identifier
              _sf :: Dim Byte,             -- sampling factors
              _tq :: {-# UNPACK #-} !Byte  -- quantization table index
    } deriving Show

data FrameHeader = FrameHeader {
                 _size :: Dim Word,
                 _fcs  :: [CompSpec]
    } deriving Show

-- Environment will be updated by headers.
data Env = Env {
         _huffTables  :: (DCHuffTable, ACHuffTable),
         _quanTables  :: Table QTable,
         _frameHeader :: FrameHeader
     } deriving Show

makeLenses ''Env

--- ENVIRONMENT MANAGEMENT ---
type EnvParser = StateT Env Parser

parseEnv :: EnvParser a -> BS.ByteString -> Either String (a, Env)
parseEnv f = parseOnly $ runStateT f initialEnv
    where initialEnv = Env {
               _huffTables  = (emptyTable, emptyTable)
              ,_quanTables  = emptyTable
              ,_frameHeader = error "No frame header found"}
          emptyTable = []

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
theByte :: Char -> Parser ()
theByte = void . char

byteI :: Parser Int
byteI = ord <$> anyChar

byte :: Parser Byte
byte = fromIntegral <$> byteI

word :: Parser Word
word = do
        a <- byte
        b <- byte
        return $ to16 a * 256 + to16 b
        where to16 a = fromIntegral a :: Word16

nibbles :: Parser (Byte, Byte)
nibbles = liftM byte2nibs byte
    where byte2nibs = (`divMod` 16)

marker :: Marker -> Parser ()
marker m = void $ do
        theByte '\xFF'
        theByte $ markerCode m

getMarker :: Parser Word8
getMarker = theByte '\xFF' >> byte

unknownSegment :: Parser ()
unknownSegment = do
        m <- getMarker
        l <- word
        trace ("Marker: " ++ showHex m " " ++ "Length: " ++ show l) $
            void $ take (fromIntegral l - 2)

parseQuanTable :: Parser QTable
parseQuanTable = guard False >> return []

frameCompSpec :: Parser CompSpec
frameCompSpec = do
        id <- byte
        sf <- nibbles
        tq <- byte
        return $ CompSpec id (toDim sf) tq

startOfFrame :: Parser FrameHeader
startOfFrame = do
        marker SOF
        _ <- word  -- Length. Assuming that length does match.
        _ <- byte  -- Sample precision. Unsupported.
        y <- word
        x <- word
        n <- byteI -- Number of color components.
        fcs <- n `count` frameCompSpec
        return $ FrameHeader (toDim (y, x)) fcs

quanTable :: EnvParser ()
quanTable = do
        table <- lift parseQuanTable
        quanTables %= (table:)

frameDesc :: EnvParser ()
frameDesc = do
        hdr <- lift startOfFrame
        frameHeader .= hdr

knownSegments :: [EnvParser ()]
knownSegments = [ quanTable
                , frameDesc
                ]

eitherOf :: (Alternative f) => [f a] -> f a
eitherOf = foldl1 (<|>)

jpegHeader :: EnvParser ()
jpegHeader = void $ do
    lift $ marker SOI
    many $ eitherOf knownSegments <|> lift unknownSegment

-- XXX: This is used for debugging purposes only!
jpegImage :: Parser [()]
jpegImage = marker SOI >> many unknownSegment

-- TODO: - Implement all primitive parsers.
--       - Encode structural constraints upon segment precedence order
--         (e.g. SOS cannot preceed SOF)
--       - Replace 'Table' definition from '[]' to 'Map'
main :: IO ()
main = do
        contents <- BS.readFile "img/sample.jpg"
        print $ parseEnv jpegHeader contents
        print $ parseOnly jpegImage contents
