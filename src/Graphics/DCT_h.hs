{-# LANGUAGE BangPatterns #-}
import Data.Packed.Matrix
import Numeric.Container

-- Normalizing coefficent generator.
spt :: Int -> Double
spt x = if (x == 0)
	then sqrt(0.125)
	else sqrt(0.25)

-- Perform DCT on a 8x8 matrix.
dct :: Matrix Double -> Matrix Double
dct datM = do
	let dctMat = buildMatrix 8 8 (\(i,j) -> ((spt i) * cos((fromIntegral(j)+0.5)*fromIntegral(i)*pi*0.125)) )
	let !result = multiply (multiply dctMat datM) (trans dctMat)
	result

-- Perform IDCT on a 8x8 matrix.
idct :: Matrix Double -> Matrix Double
idct datM = do
	let dctMat = buildMatrix 8 8 (\(i,j) -> ((spt j) * cos((fromIntegral(i)+0.5)*fromIntegral(j)*pi*0.125)) )
	let !result = multiply (multiply dctMat datM) (trans dctMat)
	result

--prepTest :: Int -> [Mddatrix Double]
--prepTest ret = do
--	let testM = []

-- Main.
main :: IO()
main = do 

	let testMat = buildMatrix 8 8 ( \(i,j) -> 2 ^ (fromIntegral(i*j)) )
	putStrLn . show $ idct (dct testMat)
