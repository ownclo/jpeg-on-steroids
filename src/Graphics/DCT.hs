{-# LANGUAGE BangPatterns #-}
module Graphics.DCT
        ( dct
        , idct
        ) where

import Data.Packed.Matrix
import Numeric.Container

-- Normalizing coefficent generator.
spt :: Int -> Double
spt 0 = sqrt 0.125
spt _ = sqrt 0.25

dctCoeff :: (Int, Int) -> Double
dctCoeff (i, j) = spt i * cos((j' + 0.5)*i'*pi*0.125)
    where i' = fromIntegral i
          j' = fromIntegral j

dctMat :: Matrix Double
dctMat = buildMatrix 8 8 dctCoeff

idctMat :: Matrix Double
idctMat = trans dctMat

transformWith :: (Product a) => Matrix a -> Matrix a -> Matrix a
transformWith !t !m = t `multiply` m `multiply` trans t

-- Perform DCT on a 8x8 matrix.
dct :: Matrix Double -> Matrix Double
dct = transformWith dctMat

-- Perform IDCT on a 8x8 matrix.
idct :: Matrix Double -> Matrix Double
idct = transformWith idctMat

-- TEST FUNCTIONS. See testSuite instead.
-- energy :: Vector Double -> Double
-- energy = foldl acc 0 . toList
--     where acc res a = res + a*a
-- 
-- testInversable :: IO ()
-- testInversable = do
--     let testMat = buildMatrix 8 8 $ \(i,j) -> fromIntegral(i*20 + j*4000 + j*i + j*i*i*i)
--         testMat' = idct . dct $ testMat
--         eng = energy $ zipVectorWith (-) (flatten testMat) (flatten testMat')
--     print $ eng
