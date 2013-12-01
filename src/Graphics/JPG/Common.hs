module Graphics.JPG.Common where

-- helpers --
ceilDiv :: Int -> Int -> Int
ceilDiv n d = (n+d-1)`div`d

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral
