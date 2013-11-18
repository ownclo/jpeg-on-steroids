{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.JPG where

import Debug.Trace (trace)
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
            | DQT -- define quantization table
            | DHT -- define huffman table
    deriving Show

markerCode :: Marker -> Char
markerCode SOI = '\xD8'
markerCode EOI = '\xD9'
markerCode SOF = '\xC0'
markerCode DQT = '\xDB'
markerCode DHT = '\xC4'

type Table a = M.Map Int a
type BC = Int
type Run = Int
data HTree a = Node (HTree a) (HTree a)
             | Leaf a
             | Nil -- for trees with odd number of leaves
             deriving (Show, Functor)

type DCHuffTable = Table (HTree BC)
type ACHuffTable = Table (HTree (Run, BC))
data HClass  = ACHuff | DCHuff deriving Show

type QTable = [Int] --- XXX: Temporary!

data Dim a = Dim {
           _y :: !a, _x :: !a
    } deriving Show

toDim :: (a, a) -> Dim a
toDim (!y, !x) = Dim y x

data CompSpec = CompSpec {
            _compId :: {-# UNPACK #-} !Byte, -- component identifier
            _sf     :: Dim Byte,             -- sampling factors
            _tq     :: {-# UNPACK #-} !Byte  -- quantization table index
    } deriving Show

data FrameHeader = FrameHeader {
            _size :: Dim Word,  -- size of an image
            _fcs  :: [CompSpec] -- frame component specification
    } deriving Show

data HuffmanSegment = HFS {
            _type    :: !HClass,
            _tableId :: {-# UNPACK #-} !Byte,
            _tree    :: HTree Int
    } deriving Show

data HuffTables = HuffTables {
            _dcTable :: DCHuffTable,
            _acTable :: ACHuffTable
    } deriving Show

-- Environment will be updated by headers.
data Env = Env {
            _huffTables  :: HuffTables,
            _qTables     :: Table QTable,
            _frameHeader :: FrameHeader
    } deriving Show
makeLenses ''Env
makeLenses ''HuffTables


--- ENVIRONMENT MANAGEMENT ---
type EnvParser = StateT Env Parser

parseEnv :: EnvParser a -> BS.ByteString -> Either String (a, Env)
parseEnv f = parseOnly $ runStateT f initialEnv
    where initialEnv = Env {
               _huffTables  = HuffTables emptyTable emptyTable
              ,_qTables     = emptyTable
              ,_frameHeader = error "No frame header found"}
          emptyTable = M.empty


---------------------------
---- MISC FUNCTIONS -------
---------------------------
byte2nibs :: (Integral a) => a -> (a, a)
byte2nibs = (`divMod` 16)

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

hClassFromIndex :: Byte -> HClass
hClassFromIndex 0 = DCHuff
hClassFromIndex _ = ACHuff


--------------------------
---- HUFFMAN TREES -------
--------------------------

-- WARN! If the tree is malformed, the code will blow up
-- with runtime exception:
-- evalState (build 0) $ [(0,1), (1,3), (2,3)]
build :: Int -> State [(a,Int)] (HTree a)
build n = do
        isEmpty <- liftM null get
        if isEmpty then return Nil -- odd number of leaves
        else do
            (v,s):xs <- get
            if n==s -- leaf
            then put xs >> return (Leaf v)
            else do x <- build (n+1)
                    y <- build (n+1)
                    return $ Node x y

buildHuffmanTree :: [[a]] -> HTree a
buildHuffmanTree =  evalState (build 0) . concat . zipWith f [1..16]
         where  f s = map (\v->(v,s))


---------------------------
---- PRIMITIVE PARSERS ----
---------------------------
skip :: Int -> Parser ()
skip = void . take

theByte :: Char -> Parser ()
theByte = void . char

byteI :: Parser Int
byteI = ord <$> anyChar

byte :: Parser Byte
byte = fI <$> byteI

word :: Parser Word
word = do
        a <- byte
        b <- byte
        return $ to16 a * 256 + to16 b
        where to16 a = fI a :: Word16

wordI :: Parser Int
wordI = do
        a <- byteI
        b <- byteI
        return $ a * 256 + b

nibbles :: Parser (Byte, Byte)
nibbles = liftM byte2nibs byte

marker :: Marker -> Parser ()
marker m = void $ do
        theByte '\xFF'
        theByte $ markerCode m

getMarker :: Parser Word8
getMarker = theByte '\xFF' >> byte


---------------------------
----- SEGMENT PARSERS -----
---------------------------
unknownSegment :: Parser ()
unknownSegment = do
        mark <- getMarker
        len  <- wordI
        trace ("Marker: " ++ showHex mark " "
            ++ "Length: " ++ show len) $ return ()
        skip $ len - 2 -- the length of 'len' (Word16) itself is included.

quanTable :: Parser (Int, QTable)
quanTable = do
        (p, id) <- nibbles
        qTable  <- 64 `count` (if p==0 then byteI else wordI)
        return (fI id, qTable)

-- NOTE: QTable consists of 64 quantization parameters.
-- A QTable can be of 1- or 2-byte precision. Yet another byte
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
quanTablesSegment :: Parser [(Int, QTable)]
quanTablesSegment = do
        marker DQT
        len <- wordI
        let n = (len - 2) `rem` 64 -- WHY? See the comment above.
        n `count` quanTable

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

huffTableSegment :: Parser HuffmanSegment
huffTableSegment = do
        marker DHT
        _ <- word              -- length, unused
        (tc, th) <- nibbles    -- table class (ac | dc), table header (aka id)
        ls <- 16 `count` byteI -- number of huffman codes with given length
        values <- mapM (`count` byteI) ls

        let hClass = hClassFromIndex tc
            id = fI th -- that was a bad name
            tree = buildHuffmanTree values

        return $ HFS hClass id tree


---------------------------
--- ENVIRONMENT PARSERS ---
---------------------------

-- NOTE: The order is relevant!
-- 'new `union` old' OVERRIDES duplicated keys from old to new,
-- whereas 'old `union` new' doesn't. We need the first.
quanTables :: EnvParser ()
quanTables = do
        tables <- M.fromList <$> lift quanTablesSegment
        qTables %= M.union tables

huffTable :: EnvParser ()
huffTable = do
        HFS hClass id' tree <- lift huffTableSegment
        let id = fI id'
        case hClass of
             DCHuff -> huffTables.dcTable %= M.insert id tree
             ACHuff -> huffTables.acTable %= M.insert id (fmap byte2nibs tree)

frameDesc :: EnvParser ()
frameDesc = do
        hdr <- lift startOfFrame
        frameHeader .= hdr

knownSegments :: [EnvParser ()]
knownSegments =  [quanTables
                 ,huffTable
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
