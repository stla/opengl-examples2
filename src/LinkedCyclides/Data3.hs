module LinkedCyclides.Data3 where
import           Data.Array                   (Array, (!), array, elems)
import qualified Data.Array                   as A
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..),
                                               Color4 (..), GLfloat)
import           Utils.Colors.Color (color')
import           Utils.Functions4D
import           Utils.Triplets 

frac :: Floating a => Int -> Int -> a
frac p q = realToFrac p / realToFrac q

hopfinverse :: Floating a => (a,a,a) -> a -> (a,a,a,a)
hopfinverse (q0,q1,q2) t =
  ((cos t * q0 + sin t * q1)/d,
   sin t * (1+q2) / d,
   cos t * (1+q2) / d,
   (sin t * q0 - cos t * q1)/d)
   where
   d = sqrt (2 * (1+q2))

func :: Floating a => a -> a -> a -> a -> (a,a,a)
func beta phi u v = stereog 1 (rightIsoclinic (pi/2) beta 1 h)
  where
  theta = 2*pi*u
  t = 2*pi*v
  h = hopfinverse (cos theta * cos phi, sin theta * cos phi, sin phi) t 

center :: Floating a => a -> a -> (a,a,a)
center beta phi = 
    middle (func beta phi 0.75 (-0.5)) (func beta phi 0.25 0)

allVertices :: Floating a => (a -> a -> (a,a,a)) -> (Int,Int)
            -> Array (Int,Int) (a,a,a)
allVertices f (n_u,n_v) = array ((0,0), (n_u-1,n_v-1)) associations
  where
  u_ = [frac i n_u | i <- [0 .. n_u-1]]
  v_ = [frac i n_v | i <- [0 .. n_v-1]]  
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), f (u_ !! i) (v_ !! j))
  associations = map g indices
 
averageNormals :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a) -> (a,a,a) 
               -> (a,a,a) -> (a,a,a) -> (a,a,a)
averageNormals (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) (x4,y4,z4) (x5,y5,z5) (x6,y6,z6) = 
  ((x1+x2+x3+x4+x5+x6)/6, (y1+y2+y3+y4+y5+y6)/6, (z1+z2+z3+z4+z5+z6)/6)

normalij :: Floating a => Array (Int,Int) (a,a,a) -> (Int, Int) -> (a,a,a)
normalij vertices (i,j) = averageNormals n1 n2 n3 n4 n5 n6
  where
  ((_,_), (n_u',n_v')) = A.bounds vertices
  im1 = if i==0 then n_u' else i-1
  ip1 = if i==n_u' then 0 else i+1
  jm1 = if j==0 then n_v' else j-1
  jp1 = if j==n_v' then 0 else j+1
  n1 = triangleNormal' 
       (vertices ! (i,j), vertices ! (i,jp1), vertices ! (ip1,j))
  n2 = triangleNormal' 
       (vertices ! (i,j), vertices ! (ip1,jm1), vertices ! (i,jm1))
  n3 = triangleNormal' 
       (vertices ! (i,j), vertices ! (im1,j), vertices ! (im1,jp1))
  n4 = triangleNormal' 
       (vertices ! (i,j), vertices ! (ip1,j), vertices ! (ip1,jm1))
  n5 = triangleNormal' 
       (vertices ! (i,j), vertices ! (i,jm1), vertices ! (im1,j))
  n6 = triangleNormal' 
       (vertices ! (i,j), vertices ! (im1,jp1), vertices ! (i,jp1))

allNormals :: Floating a => Array (Int,Int) (a,a,a) -> Array (Int,Int) (a,a,a)
allNormals vertices = array bounds associations
  where
  bounds = A.bounds vertices
  indices = A.indices vertices  
  g (i,j) = ((i,j), normalij vertices (i,j))
  associations = map g indices

trianglesij :: (Real a, Floating a) => Array (Int,Int) (a,a,a) -> Array (Int,Int) (a,a,a) 
            -> (a,a,a) -> (a,a) -> String
            -> (Int, Int) -> (Int, Int)
            -> (((Vertex3 a, Normal3 a, Color4 GLfloat), 
                 (Vertex3 a, Normal3 a, Color4 GLfloat), 
                 (Vertex3 a, Normal3 a, Color4 GLfloat)), 
                ((Vertex3 a, Normal3 a, Color4 GLfloat), 
                 (Vertex3 a, Normal3 a, Color4 GLfloat), 
                 (Vertex3 a, Normal3 a, Color4 GLfloat)))
trianglesij vertices normals o (minNorm,maxNorm) palette (nu,nv) (i,j) = 
  (((a,na,cola), (b,nb,colb), (c,nc,colc)), ((c,nc,colc), (b,nb,colb), (d,nd,cold)))
  where
  range = maxNorm - minNorm
  ip1 = if i==nu-1 then 0 else i+1
  jp1 = if j==nv-1 then 0 else j+1
  a = tripletToVertex3 $ vertices ! (i,j)
  na = tripletToNormal3 $ normals ! (i,j)
  cola = color' palette 
                ((norm' o (vertices ! (i,j)) - minNorm) / range)
  c = tripletToVertex3 $ vertices ! (i,jp1)
  nc = tripletToNormal3 $ normals ! (i,jp1)
  colc = color' palette
                ((norm' o (vertices ! (i,jp1)) - minNorm) / range)
  d = tripletToVertex3 $ vertices ! (ip1,jp1)
  nd = tripletToNormal3 $ normals ! (ip1,jp1)
  cold = color' palette
                ((norm' o (vertices ! (ip1,jp1)) - minNorm) / range)
  b = tripletToVertex3 $ vertices ! (ip1,j)
  nb = tripletToNormal3 $ normals ! (ip1,j)
  colb = color' palette
                ((norm' o (vertices ! (ip1,j)) - minNorm) / range)

allTriangles :: (Ord a, Real a, Floating a) => (Int,Int) -> String -> a -> a  
             -> [(((Vertex3 a, Normal3 a, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat)), 
                  ((Vertex3 a, Normal3 a, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat)))]
allTriangles nunv palette beta phi =
  map (trianglesij vertices normals o (minNorm,maxNorm) palette nunv) indices
  where
  vertices = allVertices (func beta phi) nunv
  normals = allNormals vertices
  o = center beta phi
  norms = map (norm' o) (elems vertices)
  minNorm = minimum norms
  maxNorm = maximum norms
  indices = A.indices vertices 