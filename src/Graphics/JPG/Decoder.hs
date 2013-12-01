module Graphics.JPG.Decoder
    ( decodeJPG
    ) where

import Graphics.JPG.Env

import Control.Applicative
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Data.Maybe(fromJust)
import Data.Monoid((<>))

type BS = B8.ByteString

data FullCompSpec = FullCompSpec {
            _fs :: !FrameCompSpec,
            _ss :: !ScanCompSpec
    } deriving Show

data CompMCUSpec = CompMCUSpec {
            _numDataUnits :: Dim Int, -- number of DUs in an MCU (h and v)
            _scaleFactors :: Dim Int, -- miltiplicators of _one_ DU
            _duSpec       :: !DataUnitSpec
    } deriving Show

type MCUSpec = [CompMCUSpec]
type Image = MCUSpec -- XXX: Temporary

-- Everything that is needed in order to
-- decode particular Data Unit.
data DataUnitSpec = DataUnitSpec {
            _qTable :: !QTable,
            _dcTree :: !DCHuffTree,
            _acTree :: !ACHuffTree
    } deriving Show

decodeJPG :: Env -> BS -> Either String Image
decodeJPG env _ = Right $ getMCUSpec env

getMCUSpec :: Env -> MCUSpec
getMCUSpec (Env (HuffTables dcT acT)
               quanT
               (FrameHeader (Dim x y) frameCS)
               scanH) = mcus where

    getSF (FullCompSpec (FrameCompSpec _ sf _) _) = sf
    maxYsf, maxXsf :: Int -- depends on fullCompSpecs
    maxYsf = fI . maximum $! map (getY . getSF) fullCompSpecs
    maxXsf = fI . maximum $! map (getX . getSF) fullCompSpecs

    _numMCUs = Dim (fI y `ceilDiv` 8*maxYsf)
                   (fI x `ceilDiv` 8*maxXsf)

    upSampFactor (Dim h w) = Dim (maxYsf `div` h)
                                 (maxXsf `div` w)

    fullCompSpecs = zipById frameCS scanH

    mcus = map buildCompMCU fullCompSpecs

    -- tables came from external scope
    buildCompMCU (FullCompSpec
                    (FrameCompSpec _ samplings qI)
                    (ScanCompSpec _ dcI acI))
                = CompMCUSpec nd ups duSpec where
        nd = fI <$> samplings
        ups = upSampFactor nd
        duSpec = DataUnitSpec qtable dctree actree

        qtable = fromJust $ M.lookup (fI qI) quanT
        dctree = fromJust $ M.lookup (fI dcI) dcT
        actree = fromJust $ M.lookup (fI acI) acT


zipById :: Table FrameCompSpec -> [ScanCompSpec] -> [FullCompSpec]
zipById tfcs = map addFcs where
    addFcs sc = case M.lookup (fromIntegral $ _scId sc) tfcs of
                    Nothing -> error "Frame header corrupted. Aborting."
                    Just fc -> FullCompSpec fc sc

-- NOTE: BS.cons is O(n) because of strict strings, but the rest is fast.
-- Will it be faster to convert to lazy ByteStrings and then back?
-- NOTE: That code probably belongs to parser.
_skipPadded :: BS -> BS
_skipPadded s = sub <> rest where
    (sub, next) = B8.break (=='\xFF') s
    next' = B8.drop 1 next
    rest = case B8.uncons next' of
                Just ('\x00', xs) -> '\xFF' `B8.cons` _skipPadded xs
                Just (_, xs) -> _skipPadded xs -- skipping the restart marker
                Nothing  -> B8.empty

-- helpers --
ceilDiv :: Int -> Int -> Int
ceilDiv n d = (n+d-1)`div`d

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral
