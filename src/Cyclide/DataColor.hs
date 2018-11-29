module Cyclide.DataColor where
import           Data.Array                   (Array, (!), array, elems)
import qualified Data.Array                   as A
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..), 
                                               Color4 (..), GLfloat)
import Utils.Colors.Color

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)
type NPoint = (Vertex3 Double, Normal3 Double, Color4 GLfloat)
type NTriangle = (NPoint, NPoint, NPoint)

norm :: Floating a => (a,a,a) -> a
norm (x,y,z) = sqrt(x*x + y*y + z*z)

norm' :: Floating a => (a,a,a) -> (a,a,a) -> a
norm' (ox,oy,oz) (x,y,z) = norm (x-ox, y-oy, z-oz)

nnormalize :: Floating a => (a,a,a) -> (a,a,a)
nnormalize (x,y,z) = (-x/n,-y/n,-z/n)
  where
    n = norm (x,y,z)

crossProd :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a)
crossProd (v1,v2,v3) (w1,w2,w3) =
  (
  v2*w3 - v3*w2,
  v3*w1 - v1*w3,
  v1*w2 - v2*w1
  )

frac :: Int -> Int -> Double
frac p q = realToFrac p / realToFrac q

pointToVertex3 :: Point -> Vertex3 Double
pointToVertex3 (x,y,z) = Vertex3 x y z

vectorToNormal3 :: Vector -> Normal3 Double
vectorToNormal3 (x,y,z) = Normal3 x y z

func :: Double -> Double -> Double -> Double -> Double -> Point
func a c d u v = (x, y, z)
  where
  b = sqrt(a*a-c*c)
  den = a - c * cos u * cos v
  x = (d * (c - a * cos u * cos v) + b * b * cos u) / den
  y = (b * sin u * (a - d * cos v)) / den
  z = (b * sin v * (c * cos u - d)) / den

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
triangleNormal0 ((x1,x2,x3), (y1,y2,y3), (z1,z2,z3)) = nnormalize abc
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
            -> Point 
            -> (Double, Double)
            -> (Int, Int) -> (Int, Int)
            -> (NTriangle, NTriangle)
trianglesij vertices normals o (minNorm,maxNorm) (nu,nv) (i,j) = 
  (((a,na,cola), (b,nb,colb), (c,nc,colc)), 
   ((c,nc,colc), (b,nb,colb), (d,nd,cold)))
  where
  range = maxNorm - minNorm
  ip1 = if i==nu-1 then 0 else i+1
  jp1 = if j==nv-1 then 0 else j+1
  a = pointToVertex3 $ vertices ! (i,j)
  na = vectorToNormal3 $ normals ! (i,j)
  cola = color' "viridis" 
                ((norm' o (vertices ! (i,j)) - minNorm) / range)
  c = pointToVertex3 $ vertices ! (i,jp1)
  nc = vectorToNormal3 $ normals ! (i,jp1)
  colc = color' "viridis"
                ((norm' o (vertices ! (i,jp1)) - minNorm) / range)
  d = pointToVertex3 $ vertices ! (ip1,jp1)
  nd = vectorToNormal3 $ normals ! (ip1,jp1)
  cold = color' "viridis"
                ((norm' o (vertices ! (ip1,jp1)) - minNorm) / range)
  b = pointToVertex3 $ vertices ! (ip1,j)
  nb = vectorToNormal3 $ normals ! (ip1,j)
  colb = color' "viridis"
                ((norm' o (vertices ! (ip1,j)) - minNorm) / range)

allTriangles :: (Int,Int) -> Double -> Double -> Double 
             -> [(NTriangle,NTriangle)]
allTriangles nunv a c d =
  map (trianglesij vertices normals o (minNorm,maxNorm) nunv) indices
  where
  vertices = allVertices (func a c d) nunv
  normals = allNormals vertices
  o = (2*c,0,0)
  norms = map (norm' o) (elems vertices)
  minNorm = minimum norms
  maxNorm = maximum norms
  indices = A.indices vertices 