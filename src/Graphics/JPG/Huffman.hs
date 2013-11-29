{-# LANGUAGE DoAndIfThenElse #-}

module Graphics.JPG.Huffman
    ( buildHuffmanTree
    ) where

import Graphics.JPG.Env(HTree(..))

import Control.Monad.State

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
