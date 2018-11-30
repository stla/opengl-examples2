module HorseSaddle.Data where
import           Data.Array                   (Array, (!), array, elems)
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..),
                                               GLfloat, Color4 (..))
import           Utils.Colors.Color           (color')
import           Utils.Triplets

func :: Floating a => a -> a -> (a,a,a)
func u v = (u, v, u*u-v*v)

gradient :: Floating a => a -> a -> (a,a,a)
gradient u v = normalize (crossProd df_du df_dv)
  where
    dfx_du = 1
    dfy_du = 0
    dfz_du = 2*u
    df_du = (dfx_du, dfy_du, dfz_du)
    dfx_dv = 0
    dfy_dv = 1
    dfz_dv = -2*v
    df_dv = (dfx_dv, dfy_dv, dfz_dv)

allVertices :: Floating a => (a -> a -> (a,a,a)) -> [a] -> [a]
            -> Array (Int,Int) (a,a,a)
allVertices f u_ v_ = array ((0,0), (n_u-1,n_v-1)) associations
  where
  n_u = length u_
  n_v = length v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), f (u_ !! i) (v_ !! j))
  associations = map g indices

allNormals :: Floating a => [a] -> [a] -> Array (Int,Int) (a,a,a)
allNormals u_ v_ =
  array ((0,0), (n_u-1,n_v-1)) associations
  where
  n_u = length u_
  n_v = length v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), gradient (u_ !! i) (v_ !! j))
  associations = map g indices

triangles_ij :: (Real a, Floating a) => Array (Int,Int) (a,a,a) 
             -> Array (Int,Int) (a,a,a) -> (a,a) -> (Int, Int)
             -> (((Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat), 
                  (Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat), 
                  (Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat)), 
                 ((Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat), 
                  (Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat), 
                  (Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat)))
triangles_ij vertices normals (minNorm,maxNorm) (i,j) =
  (((a,na,cola,cola'), (b,nb,colb,colb'), (c,nc,colc,colc')), 
   ((c,nc,colc,colc'), (b,nb,colb,colb'), (d,nd,cold,cold')))
  where
  range = maxNorm - minNorm
  ip1 = i+1
  jp1 = j+1 
  a = tripletToVertex3 $ vertices ! (i,j)
  na = tripletToNormal3 $ normals ! (i,j)
  da = (norm (vertices ! (i,j)) - minNorm) / range
  cola = color' "viridis" da
  cola' = color' "magma" da
  c = tripletToVertex3 $ vertices ! (i,jp1)
  nc = tripletToNormal3 $ normals ! (i,jp1)
  dc = (norm (vertices ! (i,jp1)) - minNorm) / range
  colc = color' "viridis" dc
  colc' = color' "magma" dc
  d = tripletToVertex3 $ vertices ! (ip1,jp1)
  nd = tripletToNormal3 $ normals ! (ip1,jp1)
  dd = (norm (vertices ! (ip1,jp1)) - minNorm) / range
  cold = color' "viridis" dd
  cold' = color' "magma" dd
  b = tripletToVertex3 $ vertices ! (ip1,j)
  nb = tripletToNormal3 $ normals ! (ip1,j)
  db = (norm (vertices ! (ip1,j)) - minNorm) / range
  colb = color' "viridis" db
  colb' = color' "magma" db

allTriangles :: (Ord a, Real a, Floating a) => (Int,Int)
             -> [(((Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat)), 
                  ((Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat, Color4 GLfloat)))]
allTriangles (n_u,n_v) =
  map (triangles_ij vertices normals (minNorm,maxNorm)) indices
  where
  frac :: Fractional b => Int -> Int -> b
  frac p q = fromIntegral p / fromIntegral q        
  u_ = [2 * frac i n_u -1 | i <- [0 .. n_u]]
  v_ = [2 * frac i n_v -1 | i <- [0 .. n_v]]
  vertices = allVertices func u_ v_
  normals = allNormals u_ v_
  norms = map norm (elems vertices)
  minNorm = minimum norms
  maxNorm = maximum norms
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]

