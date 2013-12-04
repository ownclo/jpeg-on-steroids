module Graphics.JPG.Image where

import Data.Matrix
import Data.Int(Int8)

type DataUnit = Matrix Int8
type Component = Matrix Int8

type Image = [Component]
