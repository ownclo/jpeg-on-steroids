{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.JPG.Env where

import Data.Word (Word8, Word16)
import qualified Data.Map as M
import Control.Lens(makeLenses)

type Byte = Word8
type Word = Word16

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

data ScanCompSpec = ScanCompSpec {
            _scId :: {-# UNPACK #-} !Byte, -- component identifier (within a scan)
            _dcId :: {-# UNPACK #-} !Byte, -- DC table to be used within component
            _acId :: {-# UNPACK #-} !Byte  -- AC table to be used within component
    } deriving Show

type ScanHeader = [ScanCompSpec]

data HuffmanSegment = HFS {
            _type    :: !HClass,              -- AC or DC table
            _tableId :: {-# UNPACK #-} !Byte, -- id of a table (see _dcId)
            _tree    :: HTree Int             -- Huffman tree
    } deriving Show

data HuffTables = HuffTables { -- retrieves a huffman tree given its type and id.
            _dcTable :: DCHuffTable,
            _acTable :: ACHuffTable
    } deriving Show

-- Environment will be updated by headers.
data Env = Env {
            _huffTables  :: HuffTables,   -- added by DHT (HuffmanSegment)
            _qTables     :: Table QTable, -- added by DQT
            _frameHeader :: FrameHeader,  -- added by SOF
            _scanHeader  :: ScanHeader    -- added by SOS
    } deriving Show
makeLenses ''Env
makeLenses ''HuffTables
