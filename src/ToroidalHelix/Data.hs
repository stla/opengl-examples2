module ToroidalHelix.Data where
import           Data.Array                   (Array, (!), array, elems)
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..),
                                               GLfloat, Color4 (..))
import           Utils.Colors.Color           (color')
import           Utils.Triplets

helix :: Floating a => a -> a -> a -> a -> (a,a,a)
helix bigR smallR w t = (x, y, z)
  where
    x = (bigR + smallR * cos t) * cos(t/w)
    y = (bigR + smallR * cos t) * sin(t/w)
    z = smallR * sin t

dhelix :: Floating a => a -> a -> a -> a -> (a,a,a)
dhelix bigR smallR w t = (x, y, z)
  where
    x = -smallR * sin t * cos(t/w) - (bigR + smallR * cos t)/w * sin(t/w)
    y = -smallR * sin t * sin(t/w) - (bigR + smallR * cos t)/w * cos(t/w)
    z = smallR * cos t

ddhelix :: Floating a => a -> a -> a -> a -> (a,a,a)
ddhelix bigR smallR w t = normalize (x, y, z)
  where
    x = -smallR * cos t * cos(t/w) + smallR * sin t / w * sin(t/w) +
        smallR * sin t / w * sin(t/w) - (bigR + smallR * cos t) / w / w * cos(t/w)
    y = -smallR * cos t * sin(t/w) + smallR * sin t / w * cos(t/w) +
        smallR * sin t / w * cos(t/w) - (bigR + smallR * cos t) / w / w * sin(t/w)
    z = -smallR * sin t

binormal :: Floating a => a -> a -> a -> a -> (a,a,a)
binormal bigR smallR w t =
  normalize $ crossProd (dhelix bigR smallR w t) (ddhelix bigR smallR w t)

shift :: Floating a => a -> a -> a -> a -> a -> (a,a,a)
shift bigR smallR w u v =
  add (scale (cos v) (ddhelix bigR smallR w u)) (scale (sin v) (binormal bigR smallR w u))

toroidalHelix :: Floating a => a -> a -> a -> a -> a -> a -> (a,a,a)
toroidalHelix bigR smallR w h u v =
  add (helix bigR smallR w u) (scale h (shift bigR smallR w u v))

allVertices :: Floating a => (a -> a -> (a,a,a)) -> [a] -> [a]
            -> Array (Int,Int) (a,a,a)
allVertices f u_ v_ = array ((0,0), (n_u-1,n_v-1)) associations
  where
  n_u = length u_
  n_v = length v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), f (u_ !! i) (v_ !! j))
  associations = map g indices

allNormals :: Floating a => a -> a -> a -> [a] -> [a]
           -> Array (Int,Int) (a,a,a)
allNormals bigR smallR w u_ v_ =
  array ((0,0), (n_u-1,n_v-1)) associations
  where
  n_u = length u_
  n_v = length v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), normalize $ shift bigR smallR w (u_ !! i) (v_ !! j))
  associations = map g indices

triangles_ij :: (Real a, Floating a) => Array (Int,Int) (a,a,a)
             -> Array (Int,Int) (a,a,a) -> (a,a) -> (Int,Int) -> (Int, Int)
             -> (((Vertex3 a, Normal3 a, Color4 GLfloat),
                  (Vertex3 a, Normal3 a, Color4 GLfloat),
                  (Vertex3 a, Normal3 a, Color4 GLfloat)),
                 ((Vertex3 a, Normal3 a, Color4 GLfloat),
                  (Vertex3 a, Normal3 a, Color4 GLfloat),
                  (Vertex3 a, Normal3 a, Color4 GLfloat)))
triangles_ij vertices normals (minNorm,maxNorm) (n_u,n_v) (i,j) =
  (((a,na,cola), (b,nb,colb), (c,nc,colc)),
   ((c,nc,colc), (b,nb,colb), (d,nd,cold)))
  where
  range = maxNorm - minNorm
  ip1 = if i==n_u-1 then 0 else i+1
  jp1 = if j==n_v-1 then 0 else j+1
  a = tripletToVertex3 $ vertices ! (i,j)
  na = tripletToNormal3 $ normals ! (i,j)
  cola = color' "viridis"
                ((norm (vertices ! (i,j)) - minNorm) / range)
  c = tripletToVertex3 $ vertices ! (i,jp1)
  nc = tripletToNormal3 $ normals ! (i,jp1)
  colc = color' "viridis"
                ((norm (vertices ! (i,jp1)) - minNorm) / range)
  d = tripletToVertex3 $ vertices ! (ip1,jp1)
  nd = tripletToNormal3 $ normals ! (ip1,jp1)
  cold = color' "viridis"
                ((norm (vertices ! (ip1,jp1)) - minNorm) / range)
  b = tripletToVertex3 $ vertices ! (ip1,j)
  nb = tripletToNormal3 $ normals ! (ip1,j)
  colb = color' "viridis"
                ((norm (vertices ! (ip1,j)) - minNorm) / range)

allTriangles :: (Ord a, Real a, Floating a) => a -> a -> a -> a -> (Int,Int)
             -> [(((Vertex3 a, Normal3 a, Color4 GLfloat),
                   (Vertex3 a, Normal3 a, Color4 GLfloat),
                   (Vertex3 a, Normal3 a, Color4 GLfloat)),
                  ((Vertex3 a, Normal3 a, Color4 GLfloat),
                   (Vertex3 a, Normal3 a, Color4 GLfloat),
                   (Vertex3 a, Normal3 a, Color4 GLfloat)))]
allTriangles bigR smallR w h (n_u,n_v) =
  map (triangles_ij vertices normals (minNorm,maxNorm) (n_u,n_v)) indices
  where
  frac :: Fractional b => Int -> Int -> b
  frac p q = fromIntegral p / fromIntegral q
  u_ = [w * 2*pi * frac i n_u | i <- [0 .. n_u-1]]
  v_ = [2*pi * frac i n_v | i <- [0 .. n_v-1]]
  vertices = allVertices (toroidalHelix bigR smallR w h) u_ v_
  normals = allNormals bigR smallR w u_ v_
  norms = map norm (elems vertices)
  minNorm = minimum norms
  maxNorm = maximum norms
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]

allTriangles' :: (Ord a, Real a, Floating a) => a -> a -> a -> a -> (Int,Int)
              -> [(((Vertex3 a, Normal3 a, Color4 GLfloat),
                    (Vertex3 a, Normal3 a, Color4 GLfloat),
                    (Vertex3 a, Normal3 a, Color4 GLfloat)),
                   ((Vertex3 a, Normal3 a, Color4 GLfloat),
                    (Vertex3 a, Normal3 a, Color4 GLfloat),
                    (Vertex3 a, Normal3 a, Color4 GLfloat)))]
allTriangles' bigR smallR w h nu_nv =
  (allTriangles bigR smallR w h nu_nv) ++ (allTriangles bigR smallR w h nu_nv)
