module Utils.Colors.Color where
import Utils.Colors.Palettes
import Numeric.Tools.Interpolation  (at, cubicSpline, tabulate)
import Numeric.Tools.Mesh           (uniformMesh)
import Graphics.Rendering.OpenGL.GL (Color4 (..), GLfloat)

color :: Double -> (Double, Double, Double)
color t = 
  (tbl_r `at` t, tbl_g `at` t, tbl_b `at` t)
  where
    umesh = uniformMesh (0,1) 256
    r = magma_r
    g = magma_g
    b = magma_b
    tab_r = tabulate umesh r
    tab_g = tabulate umesh g
    tab_b = tabulate umesh b
    tbl_r = cubicSpline tab_r
    tbl_g = cubicSpline tab_g
    tbl_b = cubicSpline tab_b

rgbToColor4 :: (Double,Double,Double) -> Color4 GLfloat
rgbToColor4 (r,g,b) =
  Color4 (realToFrac r) (realToFrac g) (realToFrac b) 1

color' :: Double -> Color4 GLfloat
color' = rgbToColor4 . color