module LinkedCyclides.Data where
import           Data.Array                   (Array, (!), array)
import qualified Data.Array                   as A
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

frac :: Floating a => Int -> Int -> a
frac p q = realToFrac p / realToFrac q

tripletToVertex3 :: Floating a => (a,a,a) -> Vertex3 a
tripletToVertex3 (x,y,z) = Vertex3 x y z

tripletToNormal3 :: Floating a => (a,a,a) -> Normal3 a
tripletToNormal3 (x,y,z) = Normal3 x y z

stereog :: Floating a => a -> (a,a,a,a) -> (a,a,a)
stereog r (x1,x2,x3,x4) = (x1 / (r-x4), x2 / (r-x4), x3 / (r-x4))

rightIsoclinic :: Floating a => a -> a -> a -> (a,a,a,a) -> (a,a,a,a) 
rightIsoclinic theta phi alpha (x0,x1,x2,x3) =
  ( q0*x0 - q1*x1 - q2*x2 - q3*x3
  , q1*x0 + q0*x1 + q3*x2 - q2*x3
  , q2*x0 - q3*x1 + q0*x2 + q1*x3
  , q3*x0 + q2*x1 - q1*x2 + q0*x3 )
  where
    q0 = cos alpha
    q1 = sin theta * cos phi * sin alpha
    q2 = sin theta * sin phi * sin alpha
    q3 = cos theta * sin alpha

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

allVertices :: Floating a => (a -> a -> (a,a,a)) -> (Int,Int)
            -> Array (Int,Int) (a,a,a)
allVertices f (n_u,n_v) = array ((0,0), (n_u-1,n_v-1)) associations
  where
  u_ = [frac i n_u | i <- [0 .. n_u-1]]
  v_ = [frac i n_v | i <- [0 .. n_v-1]]  
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), f (u_ !! i) (v_ !! j))
  associations = map g indices

triangleNormal0 :: Floating a => ((a,a,a), (a,a,a), (a,a,a)) -> (a,a,a)
triangleNormal0 ((x1,x2,x3), (y1,y2,y3), (z1,z2,z3)) = normalize abc
  where
    abc = crossProd (z1-x1, z2-x2, z3-x3) (y1-x1, y2-x2, y3-x3) 
 
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
  n1 = triangleNormal0 
       (vertices ! (i,j), vertices ! (i,jp1), vertices ! (ip1,j))
  n2 = triangleNormal0 
       (vertices ! (i,j), vertices ! (ip1,jm1), vertices ! (i,jm1))
  n3 = triangleNormal0 
       (vertices ! (i,j), vertices ! (im1,j), vertices ! (im1,jp1))
  n4 = triangleNormal0 
       (vertices ! (i,j), vertices ! (ip1,j), vertices ! (ip1,jm1))
  n5 = triangleNormal0 
       (vertices ! (i,j), vertices ! (i,jm1), vertices ! (im1,j))
  n6 = triangleNormal0 
       (vertices ! (i,j), vertices ! (im1,jp1), vertices ! (i,jp1))

allNormals :: Floating a => Array (Int,Int) (a,a,a) -> Array (Int,Int) (a,a,a)
allNormals vertices = array bounds associations
  where
  bounds = A.bounds vertices
  indices = A.indices vertices  
  g (i,j) = ((i,j), normalij vertices (i,j))
  associations = map g indices

trianglesij :: Floating a => Array (Int,Int) (a,a,a) -> Array (Int,Int) (a,a,a) 
            -> (Int, Int) -> (Int, Int)
            -> (((Vertex3 a, Normal3 a), 
                 (Vertex3 a, Normal3 a), 
                 (Vertex3 a, Normal3 a)), 
                ((Vertex3 a, Normal3 a), 
                 (Vertex3 a, Normal3 a), 
                 (Vertex3 a, Normal3 a)))
trianglesij vertices normals (nu,nv) (i,j) = 
  (((a,na), (b,nb), (c,nc)), ((c,nc), (b,nb), (d,nd)))
  where
  ip1 = if i==nu-1 then 0 else i+1
  jp1 = if j==nv-1 then 0 else j+1
  a = tripletToVertex3 $ vertices ! (i,j)
  na = tripletToNormal3 $ normals ! (i,j)
  c = tripletToVertex3 $ vertices ! (i,jp1)
  nc = tripletToNormal3 $ normals ! (i,jp1)
  d = tripletToVertex3 $ vertices ! (ip1,jp1)
  nd = tripletToNormal3 $ normals ! (ip1,jp1)
  b = tripletToVertex3 $ vertices ! (ip1,j)
  nb = tripletToNormal3 $ normals ! (ip1,j)

allTriangles :: Floating a => (Int,Int) -> a -> a 
             -> [(((Vertex3 a, Normal3 a), 
                   (Vertex3 a, Normal3 a), 
                   (Vertex3 a, Normal3 a)), 
                  ((Vertex3 a, Normal3 a), 
                   (Vertex3 a, Normal3 a), 
                   (Vertex3 a, Normal3 a)))]
allTriangles nunv beta phi =
  map (trianglesij vertices normals nunv) indices
  where
  vertices = allVertices (func beta phi) nunv
  normals = allNormals vertices
  indices = A.indices vertices 