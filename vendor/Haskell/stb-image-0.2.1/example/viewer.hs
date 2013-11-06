
-- | Very simple image viewer using OpenGL\/GLUT, provided 
-- as an example\/test for 'Codec.Image.STB'.

module Main where

--------------------------------------------------------------------------------

import Data.Bits

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT 

import Data.Bitmap.Pure
import Data.Bitmap.OpenGL

import Codec.Image.STB

import System.Environment
import System.Exit
import System.IO.Unsafe

import Control.Monad
--import Foreign

--------------------------------------------------------------------------------

main = do
  initialDisplayMode $= [ RGBAMode, DoubleBuffered ]

  args <- getArgs
  nonGLUTArgs <- initialize "viewer" args
  
  fname <- case nonGLUTArgs of
    [fn] -> return fn
    _    -> putStrLn "usage: viewer <imagefile.ext>" >> exitFailure
         
  img <- loadImage fname >>= \result -> case result of
    Left err  -> putStrLn err >> exitFailure
    Right img -> return img
    
  let (x,y) = bitmapSize img
      c = bitmapNChannels img

  putStrLn $ "\"" ++ fname ++ "\" loaded"
  putStrLn $ "resolution = " ++ show x ++ " x " ++ show y ++ ", " ++ show c ++ " bytes per pixel"  

  createWindow ("viewer - " ++ show fname)

  let eimg = extendImage img 
  tex <- makeSimpleBitmapTexture eimg

  displayCallback       $= display img tex
  reshapeCallback       $= Just reshape
  keyboardMouseCallback $= Just keyboard

  drawBuffer $= BackBuffers
  postRedisplay Nothing

  mainLoop
  
-- reshape callback
reshape _ = postRedisplay Nothing
  
-- keyboard callback
keyboard key keyState mod pos = 
  case key of 
    Char '\ESC' -> exitWith ExitSuccess
    _ -> return ()    

--------------------------------------------------------------------------------  
  
-- glVertex2d / glTexCoord2d, convenient because the OpenGL vertex / texCoord functions are too polymorphic

vt :: Double -> Double -> IO ()
vt x y = vertex (Vertex2 (realToFrac x :: GLdouble) (realToFrac y :: GLdouble))

tc :: Double -> Double -> IO ()
tc x y = texCoord (TexCoord2 (realToFrac x :: GLdouble) (realToFrac y :: GLdouble))

--------------------------------------------------------------------------------  

-- display callback  
display img tex = do
  let (xsize,ysize) = bitmapSize img
  clear [ColorBuffer] 

  size@(Size xres yres) <- get windowSize
  viewport $= ( Position 0 0 , size )
  let winaspect = fromIntegral xres  / fromIntegral yres  :: Double
      picaspect = fromIntegral xsize / fromIntegral ysize :: Double
  matrixMode $= Projection >> loadIdentity
  ortho (-1) 1 (-1) 1 (-1) 1
          
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just tex
  let (x,y) = if picaspect > winaspect then (1,winaspect/picaspect) else (picaspect/winaspect,1)
      r = fromIntegral xsize / fromIntegral (nextPowerOfTwo xsize) 
      b = fromIntegral ysize / fromIntegral (nextPowerOfTwo ysize) 
  renderPrimitive Quads $ do 
    tc 0 0 ; vt (-x) ( y)
    tc r 0 ; vt ( x) ( y)
    tc r b ; vt ( x) (-y)
    tc 0 b ; vt (-x) (-y)
         
  swapBuffers
  
--------------------------------------------------------------------------------  
  
log2 :: Int -> Int
log2 n = case n of
  0 -> -1
  _ -> 1 + log2 (shiftR n 1) 
 
nextPowerOfTwo :: Int -> Int      
nextPowerOfTwo n = 2 ^ ( 1 + log2 (n-1) )
  
-- extend the image to have power-of-two sizes, for old videocards
extendImage :: Image -> Image  
extendImage bm = copySubImage' bm (0,0) (oldx,oldy) (newx,newy) (0,0) where
  (oldx,oldy) = bitmapSize bm
  (newx,newy) = (nextPowerOfTwo oldx, nextPowerOfTwo newx)
  
--------------------------------------------------------------------------------  
    