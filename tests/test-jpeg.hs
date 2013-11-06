{-# LANGUAGE BangPatterns #-}
module Main where

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

--[ testGroup "cases" $ zipWith (testCase . show) [1 :: Int ..] $ [] 
tests :: [Test]
tests = 
    [ testGroup "properties" $ zipWith (testProperty . show) [1 :: Int ..] $ 
        [ property $ \ a -> spec a (*2) a == ((*2) a :: Int)  -- unevaluated
        , property $ \ !a -> spec a (*2) a == ((*2) $! a :: Int) -- evaluated
        ] 
    ]
