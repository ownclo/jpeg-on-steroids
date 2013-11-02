import Graphics.JPEG
import System.Environment(getArgs)

main = do
    [source, dest] <- getArgs
    jpgFile2ppmFile source dest
