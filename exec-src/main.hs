module Main where

import Graphics.JPG

import qualified Data.ByteString.Char8 as B8
import System.Environment(getArgs)

main :: IO ()
main = do
        [filename] <- getArgs
        contents <- B8.readFile filename
        print $ parseHeader contents >>= uncurry decodeJPG
