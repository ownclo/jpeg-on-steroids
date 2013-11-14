{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.JPG where

import Debug.Trace (trace, traceShow)
import Numeric (showHex)

import Control.Applicative
import Control.Monad.State
import Prelude hiding(take, id)

import Control.Lens(makeLenses, (.=), (%=))
import qualified Data.Map as M
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

-- supported markers
data Marker = SOI -- start of input
            | EOI -- end of input
            | SOF -- start of frame
            | DQT -- Define quantization table
    deriving Show

markerCode :: Marker -> Char
markerCode SOI = '\xD8'
markerCode EOI = '\xD9'
markerCode SOF = '\xC0'
markerCode DQT = '\xDB'


type Segment = BS.ByteString

type Table a = M.Map Int a
type BC = Int
type Run = Int
data HuffTree a = Node (HuffTree a) (HuffTree a)
                | Leaf a
                deriving Show

type DCHuffTable = Table (HuffTree BC)
type ACHuffTable = Table (HuffTree (Run, BC))
type QTable = [Int] --- XXX: Temporary!

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
         _hufTables   :: (DCHuffTable, ACHuffTable),
         _qTables     :: Table QTable,
         _frameHeader :: FrameHeader
    } deriving Show

makeLenses ''Env


--- ENVIRONMENT MANAGEMENT ---
type EnvParser = StateT Env Parser

parseEnv :: EnvParser a -> BS.ByteString -> Either String (a, Env)
parseEnv f = parseOnly $ runStateT f initialEnv
    where initialEnv = Env {
               _hufTables   = (emptyTable, emptyTable)
              ,_qTables     = emptyTable
              ,_frameHeader = error "No frame header found"}
          emptyTable = M.empty


---------------------------
--------- PARSERS --------- 
---------------------------
skip :: Int -> Parser ()
skip = void . take

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

wordI :: Parser Int
wordI = do
        a <- byteI
        b <- byteI
        return $ a * 256 + b

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
        mark <- getMarker
        len  <- wordI
        trace ("Marker: " ++ showHex mark " "
            ++ "Length: " ++ show len) $ return ()
        skip $ len - 2 -- the length of 'len' (Word16) itself is included.

parseQuanTable :: Parser (Int, QTable)
parseQuanTable = do
        (p, id) <- nibbles
        qTable  <- 64 `count` (if p==0 then byteI else wordI)
        return (fromIntegral id, qTable)

-- NOTE: QTable consists of 64 quantization parameters.
-- A QTable can have 1- or 2-byte precision. Yet another byte
-- precedes q-values. That byte indicates the type of precision
-- and the index of QTable (see parseQuanTable). So, the size
-- of each table is 1 + (1|2)*64. How can we deduce the number
-- of QTables given their accumulative length? As it can be
-- seen, the number of extra bytes indicates this properly.
--      len = k * (1 + (1|2)*64)
--      len mod 64 = k mod 64 + k*(1|2)*64 mod 64
--      len mod 64 = k mod 64
--      len mod 64 = k // see below.
-- The last equation holds iff k < 64. We assume that this is
-- the case for any image (not necessarily, but more than likely),
-- because the index space of QTables is of 4 bit size.
manyQuanTables :: Parser [(Int, QTable)]
manyQuanTables = do
        marker DQT
        len <- wordI
        let n = (len - 2) `rem` 64 -- WHY? See the comment above.
        traceShow (len, n) $ return ()
        n `count` parseQuanTable

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
        _ <- byte  -- Sample precision. Unsupported (always byte)
        y <- word  -- Vertical size of a picture (in pixels)
        x <- word  -- Horizonal size of a picture (in pixels)
        n <- byteI -- Number of color components.
        fcs <- n `count` frameCompSpec
        return $ FrameHeader (toDim (y, x)) fcs

-- NOTE: The order is relevant!
-- 'new `union` old' OVERRIDES duplicated keys from old to new,
-- whereas 'old `union` new' doesn't
quanTables :: EnvParser ()
quanTables = do
        tables <- M.fromList <$> lift manyQuanTables
        qTables %= M.union tables

frameDesc :: EnvParser ()
frameDesc = do
        hdr <- lift startOfFrame
        frameHeader .= hdr

knownSegments :: [EnvParser ()]
knownSegments =  [quanTables
                 ,frameDesc
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
main :: IO ()
main = do
        contents <- BS.readFile "img/sample.jpg"
        print $ parseEnv jpegHeader contents
        print $ parseOnly jpegImage contents
