module ConicalSpiral.Data where
import           Data.Array                   (Array, (!), array, elems)
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..),
                                               GLfloat, Color4 (..))
import           Utils.Colors.Color           (color')
import           Utils.Triplets

sconical :: Floating a => a -> a -> a -> a -> Bool -> a -> a -> (a,a,a)
sconical alpha beta gamma n swap u v = (x, e*y, e*z)
  where
    x = alpha*(1-0.5*v/pi)*sin (n*v+0.5*pi)*(1-cos u) + gamma*sin(n*v+0.5*pi)
    y = beta*0.5*v/pi + alpha*(1-0.5*v/pi)*sin u
    z = cos(n*v+0.5*pi) * (gamma + alpha*(1-0.5*v/pi)*(1-cos u))
    e = realToFrac $ 1 - 2*fromEnum swap

gradient :: Floating a => a -> a -> a -> a -> Bool -> a -> a -> (a,a,a)
gradient alpha beta gamma n swap u v = normalize (crossProd df_du df_dv)
  where
    dfx_du = alpha*(1-0.5*v/pi)*sin(n*v+0.5*pi)*sin u
    dfy_du = alpha*(1-0.5*v/pi)*cos u
    dfz_du = alpha*(1-0.5*v/pi)*cos(n*v+0.5*pi)*sin u
    df_du = (dfx_du, e*dfy_du, e*dfz_du)
    dfx_dv = n*cos(n*v+0.5*pi)*(gamma + alpha*(1-0.5*v/pi)*(1-cos u)) +
             sin(n*v+0.5*pi)*alpha*0.5/pi*(cos u - 1)
    dfy_dv = beta/2/pi - alpha/2/pi*sin u
    dfz_dv = -n*sin(n*v+0.5*pi)*(gamma + alpha*(1-0.5*v/pi)*(1-cos u)) +
             cos(n*v+0.5*pi)*alpha*0.5/pi*(cos u - 1)
    df_dv = (dfx_dv, e*dfy_dv, e*dfz_dv)
    e = realToFrac $ 1 - 2*fromEnum swap

allVertices :: Floating a => (a -> a -> (a,a,a)) -> [a] -> [a]
            -> Array (Int,Int) (a,a,a)
allVertices f u_ v_ = array ((0,0), (n_u-1,n_v-1)) associations
  where
  n_u = length u_
  n_v = length v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), f (u_ !! i) (v_ !! j))
  associations = map g indices

allNormals :: Floating a => a -> a -> a -> a -> Bool -> [a] -> [a] 
           -> Array (Int,Int) (a,a,a)
allNormals alpha beta gamma n swap u_ v_ =
  array ((0,0), (n_u-1,n_v-1)) associations
  where
  n_u = length u_
  n_v = length v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), gradient alpha beta gamma n swap (u_ !! i) (v_ !! j))
  associations = map g indices

triangles_ij :: (Real a, Floating a) => Array (Int,Int) (a,a,a) 
             -> Array (Int,Int) (a,a,a) -> (a,a) -> Int -> (Int, Int)
             -> (((Vertex3 a, Normal3 a, Color4 GLfloat), 
                  (Vertex3 a, Normal3 a, Color4 GLfloat), 
                  (Vertex3 a, Normal3 a, Color4 GLfloat)), 
                 ((Vertex3 a, Normal3 a, Color4 GLfloat), 
                  (Vertex3 a, Normal3 a, Color4 GLfloat), 
                  (Vertex3 a, Normal3 a, Color4 GLfloat)))
triangles_ij vertices normals (minNorm,maxNorm) n_u (i,j) =
  (((a,na,cola), (b,nb,colb), (c,nc,colc)), 
   ((c,nc,colc), (b,nb,colb), (d,nd,cold)))
  where
  range = maxNorm - minNorm
  ip1 = if i==n_u-1 then 0 else i+1
  jp1 = j+1 
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

allTriangles :: (Ord a, Real a, Floating a) => a -> a -> a -> a 
             -> Bool -> (Int,Int)
             -> [(((Vertex3 a, Normal3 a, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat)), 
                  ((Vertex3 a, Normal3 a, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat), 
                   (Vertex3 a, Normal3 a, Color4 GLfloat)))]
allTriangles alpha beta gamma n swap (n_u,n_v) =
  map (triangles_ij vertices normals (minNorm,maxNorm) n_u) indices
  where
  frac :: Fractional b => Int -> Int -> b
  frac p q = fromIntegral p / fromIntegral q        
  u_ = [2*pi * frac i n_u | i <- [0 .. n_u-1]]
  v_ = [2*pi * frac i n_v | i <- [0 .. n_v]]
  vertices = allVertices (sconical alpha beta gamma n swap) u_ v_
  normals = allNormals alpha beta gamma n swap u_ v_
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
allTriangles' alpha beta gamma n (n_u,n_v) = 
  (allTriangles alpha beta gamma n False (n_u,n_v)) ++
    (allTriangles alpha beta gamma n True (n_u,n_v))
