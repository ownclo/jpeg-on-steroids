{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Graphics.DCT
import Data.Packed.Matrix
import Numeric.Container
import Control.Monad

import Prelude hiding ((||),(&&)) 
import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
-- import Test.Framework.Providers.HUnit
-- import Test.HUnit hiding (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ((==>))

main :: IO () 
main = defaultMain tests

spec a f _ = f a

instance Arbitrary (Matrix Double) where
        arbitrary = do
            list <- replicateM 64 (arbitrary :: Gen Double)
            let m = buildMatrix 8 8 $ \(i, j) -> list !! (i*8 + j)
            return m

prop_inversable :: Matrix Double -> Bool
prop_inversable m = energy diffMatrix < 0.0000005 where
                diffMatrix = mapVector abs $ zipVectorWith (-) (flatten m) (flatten m')
                m' = idct . dct $ m
                energy = foldVector acc 0
                acc res a = res + a*a

--[ testGroup "cases" $ zipWith (testCase . show) [1 :: Int ..] $ [] 
tests :: [Test]
tests = 
    [ testGroup "properties" $ zipWith (testProperty . show) [1 :: Int ..] $ 
        [ property $ \ a -> spec a (*2) a == ((*2) a :: Int)  -- unevaluated
        , property $ \ !a -> spec a (*2) a == ((*2) $! a :: Int) -- evaluated
        ] 
    , testGroup "DCT" $ zipWith (testProperty . show) [1 :: Int ..] $
        [ prop_inversable :: Matrix Double -> Bool
        ]
    ]
