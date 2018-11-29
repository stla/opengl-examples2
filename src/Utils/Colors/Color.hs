module Utils.Colors.Color where
import Data.Map.Strict              ((!))
import Graphics.Rendering.OpenGL.GL (Color4 (..), GLfloat)
import Numeric.Tools.Interpolation  (at, cubicSpline, tabulate)
import Numeric.Tools.Mesh           (uniformMesh)
import Utils.Colors.Palettes

color :: String -> Double -> (Double, Double, Double)
color palette t = 
  (tbl_r `at` t, tbl_g `at` t, tbl_b `at` t)
  where
    (r, g, b) = palettes ! palette
    umesh = uniformMesh (0,1) 256
    tab_r = tabulate umesh r
    tab_g = tabulate umesh g
    tab_b = tabulate umesh b
    tbl_r = cubicSpline tab_r
    tbl_g = cubicSpline tab_g
    tbl_b = cubicSpline tab_b

rgbToColor4 :: (Double,Double,Double) -> Color4 GLfloat
rgbToColor4 (r,g,b) =
  Color4 (realToFrac r) (realToFrac g) (realToFrac b) 1

color' :: String -> Double -> Color4 GLfloat
color' palette = rgbToColor4 . (color palette)