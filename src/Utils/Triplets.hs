module Utils.Triplets where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..),
                                               Vector3 (..))
import           Linear                       (V3 (..))

norm :: Floating a => (a,a,a) -> a
norm (x,y,z) = sqrt(x*x + y*y + z*z)

normalize :: Floating a => (a,a,a) -> (a,a,a)
normalize (x,y,z) = (-x/n,-y/n,-z/n)
  where
    n = norm (x,y,z)

crossProd :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a)
crossProd (v1,v2,v3) (w1,w2,w3) =
  (
  v2*w3 - v3*w2,
  v3*w1 - v1*w3,
  v1*w2 - v2*w1
  )

tripletToVertex3 :: Floating a => (a,a,a) -> Vertex3 a
tripletToVertex3 (x,y,z) = Vertex3 x y z

tripletToVector3 :: Floating a => (a,a,a) -> Vector3 a
tripletToVector3 (x,y,z) = Vector3 x y z

tripletToNormal3 :: Floating a => (a,a,a) -> Normal3 a
tripletToNormal3 (x,y,z) = Normal3 x y z

tripletTonegNormal3 :: Floating a => (a,a,a) -> Normal3 a
tripletTonegNormal3 (x,y,z) = Normal3 (-x) (-y) (-z)

tripletToV3 :: Floating a => (a,a,a) -> V3 a
tripletToV3 (x,y,z) = V3 x y z

triangleNormal :: Floating a => ((a,a,a),(a,a,a),(a,a,a)) -> Normal3 a
triangleNormal ((x1,x2,x3), (y1,y2,y3), (z1,z2,z3)) =
  tripletToNormal3 (normalize abc)
  where
    abc = crossProd (y1-x1, y2-x2, y3-x3) (z1-x1, z2-x2, z3-x3)

triangleNegNormal :: Floating a => ((a,a,a),(a,a,a),(a,a,a)) -> Normal3 a
triangleNegNormal ((x1,x2,x3), (y1,y2,y3), (z1,z2,z3)) =
  tripletTonegNormal3 (normalize abc)
  where
    abc = crossProd (y1-x1, y2-x2, y3-x3) (z1-x1, z2-x2, z3-x3)
