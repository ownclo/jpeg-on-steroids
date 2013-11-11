{-# LANGUAGE BangPatterns #-}
import Data.Packed.Matrix
import Numeric.Container

-- Normalizing coefficent generator.
spt :: Int -> Double
spt 0 = sqrt 0.125
spt x = sqrt 0.25

dctCoeff (i, j) = spt i * cos((j' + 0.5)*i'*pi*0.125)
    where i' = fromIntegral i
          j' = fromIntegral j

dctMat :: Matrix Double
dctMat = buildMatrix 8 8 dctCoeff

idctMat :: Matrix Double
idctMat = trans dctMat

transformWith :: (Product a) => Matrix a -> Matrix a -> Matrix a
transformWith !m !t = t `multiply` m `multiply` trans t

-- Perform DCT on a 8x8 matrix.
dct :: Matrix Double -> Matrix Double
dct datM = datM `transformWith` dctMat

-- Perform IDCT on a 8x8 matrix.
idct :: Matrix Double -> Matrix Double
idct datM = datM `transformWith` idctMat

main :: IO()
main = do 
    let testMat = buildMatrix 8 8 $ \(i,j) -> fromIntegral(i + j)
    print $ idct (dct testMat)
