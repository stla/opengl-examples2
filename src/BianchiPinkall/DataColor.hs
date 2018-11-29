module BianchiPinkall.DataColor where
import           Data.Array                   (Array, (!), array, elems)
import qualified Data.Array                   as A
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..), 
                                               Color4 (..), GLfloat)
import Utils.Colors.Color

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)
type NPoint = (Vertex3 Double, Normal3 Double, Color4 GLfloat)
type NTriangle = (NPoint, NPoint, NPoint)

normalize :: Floating a => (a,a,a) -> (a,a,a)
normalize (x,y,z) = (x/n,y/n,z/n)
  where
    n = sqrt(x*x+y*y+z*z)

frac :: Int -> Int -> Double
frac p q = realToFrac p / realToFrac q

pointToVertex3 :: Point -> Vertex3 Double
pointToVertex3 (x,y,z) = Vertex3 x y z

vectorToNormal3 :: Vector -> Normal3 Double
vectorToNormal3 (x,y,z) = Normal3 x y z

norm :: Point -> Double
norm (x,y,z) = sqrt(x*x + y*y + z*z)

crossProd :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a)
crossProd (v1,v2,v3) (w1,w2,w3) =
  (
  v2*w3 - v3*w2,
  v3*w1 - v1*w3,
  v1*w2 - v2*w1
  )

-- modified stereographic projection
stereom :: (Double,Double,Double,Double) -> Point
stereom (p1,p2,p3,p4) = (r * p1, r * p2, r * p3)
  where
    r = acos(p4) / pi / sqrt (1 - p4*p4)

flatTorus :: Double -> Double -> Double -> (Double,Double,Double,Double)
flatTorus n u v = ( cos a * cos (u+v)
                  , cos a * sin (u+v)
                  , sin a * cos (u-v)
                  , sin a * sin (u-v) )
  where
  a = 0.5 + 0.5 * sin (n * 2 * v)

pinkallFun :: Double -> Double -> Double -> Point
pinkallFun n u v = stereom (flatTorus n u v)

allVertices :: (Double -> Double -> Point) -> (Int,Int)
            -> Array (Int,Int) Point
allVertices f (n_u,n_v) = array ((0,0), (n_u-1,n_v-1)) associations
  where
  u_ = [2*pi * frac i n_u | i <- [0 .. n_u-1]]
  v_ = [2*pi * frac i n_v | i <- [0 .. n_v-1]]  
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), f (u_ !! i) (v_ !! j))
  associations = map g indices

triangleNormal0 :: (Point, Point, Point) -> Vector
triangleNormal0 ((x1,x2,x3), (y1,y2,y3), (z1,z2,z3)) = normalize abc
  where
    abc = crossProd (z1-x1, z2-x2, z3-x3) (y1-x1, y2-x2, y3-x3) 

averageNormals :: Vector -> Vector -> Vector -> Vector -> Vector -> Vector 
               -> Vector
averageNormals (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) (x4,y4,z4) (x5,y5,z5) (x6,y6,z6) = 
  ((x1+x2+x3+x4+x5+x6)/6, (y1+y2+y3+y4+y5+y6)/6, (z1+z2+z3+z4+z5+z6)/6)

normalij :: Array (Int,Int) Point -> (Int, Int) -> Vector
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

allNormals :: Array (Int,Int) Point -> Array (Int,Int) Vector
allNormals vertices = array bounds associations
  where
  bounds = A.bounds vertices
  indices = A.indices vertices  
  g (i,j) = ((i,j), normalij vertices (i,j))
  associations = map g indices

trianglesij :: Array (Int,Int) Point -> Array (Int,Int) Vector 
            -> (Double, Double)
            -> (Int, Int) -> (Int, Int)
            -> (NTriangle, NTriangle)
trianglesij vertices normals (minNorm,maxNorm) (nu,nv) (i,j) = 
  (((a,na,cola), (b,nb,colb), (c,nc,colc)), ((c,nc,colc), (b,nb,colb), (d,nd,cold)))
  where
  ip1 = if i==nu-1 then 0 else i+1
  jp1 = if j==nv-1 then 0 else j+1
  a = pointToVertex3 $ vertices ! (i,j)
  na = vectorToNormal3 $ normals ! (i,j)
  cola = color' "magma" 
                ((norm (vertices ! (i,j)) - minNorm) / (maxNorm - minNorm))
  c = pointToVertex3 $ vertices ! (i,jp1)
  nc = vectorToNormal3 $ normals ! (i,jp1)
  colc = color' "magma"
                ((norm (vertices ! (i,jp1)) - minNorm) / (maxNorm - minNorm))
  d = pointToVertex3 $ vertices ! (ip1,jp1)
  nd = vectorToNormal3 $ normals ! (ip1,jp1)
  cold = color' "magma"
                ((norm (vertices ! (ip1,jp1)) - minNorm) / (maxNorm - minNorm))
  b = pointToVertex3 $ vertices ! (ip1,j)
  nb = vectorToNormal3 $ normals ! (ip1,j)
  colb = color' "magma"
                ((norm (vertices ! (ip1,j)) - minNorm) / (maxNorm - minNorm))

allTriangles :: (Int,Int) -> Double -> [(NTriangle,NTriangle)]
allTriangles nunv n =
  map (trianglesij vertices normals (minNorm,maxNorm) nunv) indices
  where
  vertices = allVertices (pinkallFun n) nunv
  normals = allNormals vertices
  norms = map norm (elems vertices)
  minNorm = minimum norms
  maxNorm = maximum norms
  indices = A.indices vertices 