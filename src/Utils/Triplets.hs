module Utils.Triplets where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))

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

tripletToNormal3 :: Floating a => (a,a,a) -> Normal3 a
tripletToNormal3 (x,y,z) = Normal3 x y z
