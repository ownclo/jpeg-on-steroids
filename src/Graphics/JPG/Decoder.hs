{-# LANGUAGE BangPatterns #-}
module Graphics.JPG.Decoder
    ( decodeJPG
    ) where

import Graphics.JPG.Env

import Control.Lens((^.))
import Data.Monoid
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M

type BS = B8.ByteString
type Image = [FullCompSpec] -- XXX: Temporary

type MCUSpec = [CompMCUSpec]

data FullCompSpec = FullCompSpec {
            _fcs :: !FrameCompSpec,
            _scs :: !ScanCompSpec
    } deriving Show

data CompMCUSpec = CompMCUSpec {
            _numDataUnits :: Dim Int, -- number of DUs in an MCU (h and v)
            _scaleFactors :: Dim Int, -- miltiplicators of _one_ DU
            _duSpec       :: !DataUnitSpec
    } deriving Show

-- Everything that is needed in order to
-- decode particular Data Unit.
data DataUnitSpec = DataUnitSpec {
            _qTable :: !QTable,
            _dcTree :: !DCHuffTree,
            _acTree :: !ACHuffTree
    } deriving Show

decodeJPG :: Env -> BS -> Either String Image
decodeJPG !env !s = Right fullCompSpecs where
    Dim y x = env^.frameHeader.size
    frameCS = env^.frameHeader.fcs
    hufIds  = env^.scanHeader

    fullCompSpecs = zipById frameCS hufIds

zipById :: Table FrameCompSpec -> [ScanCompSpec] -> [FullCompSpec]
zipById fcs scs = map addFcs scs where
    addFcs sc = case M.lookup (fromIntegral $ _scId sc) fcs of
                    Nothing -> error "Frame header corrupted. Aborting."
                    Just fc -> FullCompSpec fc sc

-- NOTE: BS.cons is O(n) because of strict strings, but the rest is fast.
-- Will it be faster to convert to lazy ByteStrings and then back?
-- NOTE: That code probably belongs to parser.
skipPadded :: BS -> BS
skipPadded s = sub <> rest where
    (sub, next) = B8.break (=='\xFF') s
    next' = B8.drop 1 next
    rest = case B8.uncons next' of
                Just ('\x00', xs) -> '\xFF' `B8.cons` skipPadded xs
                Just (_, xs) -> skipPadded xs -- skipping the restart marker
                Nothing  -> B8.empty
