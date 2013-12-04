module Graphics.JPG.Decoder
    ( decodeJPG
    ) where

import Graphics.JPG.Env
import Graphics.JPG.Common
import Graphics.JPG.Image

import Control.Applicative hiding ((<|>))
-- import Control.Monad(replicateM)
import Control.Monad.State

import qualified Data.ByteString.Char8 as B8
import Data.List(foldl1')
import qualified Data.Map as M
import Data.Matrix hiding (matrix)
import Data.Maybe(fromJust)
import Data.Monoid((<>))

type BS = B8.ByteString

data FullCompSpec = FullCompSpec {
            _fs :: !FrameCompSpec,
            _ss :: !ScanCompSpec
    } deriving Show

data CompMCUSpec = CompMCUSpec {
            _numDataUnits :: !(Dim Int), -- number of DUs in an MCU (h and v)
            _duSpec       :: !DataUnitSpec
    } deriving Show

type MCUSpec = [CompMCUSpec]

-- Everything that is needed in order to
-- decode particular Data Unit.
data DataUnitSpec = DataUnitSpec {
            _scaleFactors :: !(Dim Int), -- miltiplicators of _one_ DU
            _qTable :: !QTable,
            _dcTree :: !DCHuffTree,
            _acTree :: !ACHuffTree
    } deriving Show

decodeJPG :: Env -> BS -> Either String Image
decodeJPG env s = Right . fst $ runState (image numMCUs mcuSpec) s'
    where (numMCUs, mcuSpec) = getMCUSpec env
          s' = skipPadded s

image :: Dim Int -- number of MCUs
      -> MCUSpec
      -> State BS Image
image numMCUs mcuSpec = fmap concatMCUs $ matrix numMCUs (mcu mcuSpec)
    where concatMCUs :: [[Image]] -> Image
          concatMCUs = joinVert . map joinHor
          joinVert = foldl1' concatImagesVert
          joinHor  = foldl1' concatImagesHor

          concatImagesVert :: Image -> Image -> Image
          concatImagesVert = zipWith (<->)

          concatImagesHor :: Image -> Image -> Image
          concatImagesHor = zipWith (<|>)

mcu :: MCUSpec -> State BS Image
mcu = mapM compMCU

compMCU :: CompMCUSpec -> State BS DataUnit
compMCU (CompMCUSpec numDUs duSpec) =
    concatDUs <$> matrix numDUs (dataUnit duSpec)

concatDUs :: [[DataUnit]] -> DataUnit
concatDUs = joinVert . map joinHor where
    joinVert = foldl1' (<->)
    joinHor  = foldl1' (<|>)

dataUnit :: DataUnitSpec -> State BS DataUnit
dataUnit (DataUnitSpec ups qT dcTree acTree)
            = undefined

getMCUSpec :: Env -> (Dim Int, MCUSpec)
getMCUSpec (Env (HuffTables dcT acT)
                quanT
                (FrameHeader (Dim x y) frameCS)
                scanH) = (numMCUs, mcuSpec) where

    fullCompSpecs = zipById frameCS scanH
    (maxYsf, maxXsf) = getMaxSampFactors fullCompSpecs
    mcuSpec = map buildCompMCU fullCompSpecs

    numMCUs = Dim (fI y `ceilDiv` 8*maxYsf)
                  (fI x `ceilDiv` 8*maxXsf)

    upSampFactor (Dim h w) = Dim (maxYsf `div` h)
                                 (maxXsf `div` w)

    -- it needs all tables + maxY and maxX from outer closure.
    buildCompMCU (FullCompSpec
                    (FrameCompSpec _ samplings qI)
                    (ScanCompSpec _ dcI acI))
                = CompMCUSpec nd duSpec where
        nd = fI <$> samplings
        ups = upSampFactor nd
        duSpec = DataUnitSpec ups qtable dctree actree

        qtable = fromJust $ M.lookup (fI qI) quanT
        dctree = fromJust $ M.lookup (fI dcI) dcT
        actree = fromJust $ M.lookup (fI acI) acT

getMaxSampFactors :: [FullCompSpec] -> (Int, Int)
getMaxSampFactors fullCompSpecs = (maxYsf, maxXsf) where
    maxYsf = fI . maximum $! map (getY . getSF) fullCompSpecs
    maxXsf = fI . maximum $! map (getX . getSF) fullCompSpecs
    getSF (FullCompSpec (FrameCompSpec _ sf _) _) = sf

zipById :: Table FrameCompSpec -> [ScanCompSpec] -> [FullCompSpec]
zipById tfcs = map addFcs where
    addFcs sc = case M.lookup (fromIntegral $ _scId sc) tfcs of
                    Nothing -> error "Frame header corrupted. Aborting."
                    Just fc -> FullCompSpec fc sc

matrix :: Monad m => Dim Int -> m a -> m [[a]]
matrix (Dim y x) = replicateM y . replicateM x

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
