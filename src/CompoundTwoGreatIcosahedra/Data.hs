module CompoundTwoGreatIcosahedra.Data
  where
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (Vector3 (..))
import           Linear (V3 (..))
import           Utils.Triplets

vertices0 :: [(Double,Double,Double)]
vertices0 = [ 
  (0, 0, a), 
  (b, 0, -b/2), 
  (-c, 1, -b/2),
  (d, -phi, -b/2), 
  (d, phi, -b/2),
  (-c, -1, -b/2),
  (-d, -phi, b/2),
  (-d, phi, b/2),
  (c, 1, b/2),
  (-b, 0, b/2),
  (c, -1, b/2),
  (0, 0, -a) 
  ]
  where
    phi = (1 + sqrt 5) / 2
    a = sqrt (phi+2)
    b = 1 / sin (pi/5)
    c = cos (pi/5) / sin (pi/5)
    d = (cos (pi/5) - 0.5) / sin (pi/5)

rvertices0 :: [(Double, Double, Double)]
rvertices0 = map (rotationY (pi/2)) vertices0

vertices :: [Vector3 Double]
vertices = map tripletToVector3 vertices0

rvertices :: [Vector3 Double]
rvertices = map tripletToVector3 rvertices0

vertices'' :: [V3 Double]
vertices'' = map tripletToV3 vertices0

rvertices'' :: [V3 Double]
rvertices'' = map tripletToV3 rvertices0

edges :: [(Int,Int)]
edges = [
    (0,1),
    (0,2),
    (0,3),
    (0,4),
    (0,5),
    (1,2),
    (1,5),
    (1,6),
    (1,7),
    (2,3),
    (2,6),
    (2,8),
    (3,4),
    (3,8),
    (3,9),
    (4,5),
    (4,9),
    (4,10),
    (5,7),
    (5,10),
    (6,7),
    (6,8),
    (6,11),
    (7,10),
    (7,11),
    (8,9),
    (8,11),
    (9,10),
    (9,11),
    (10,11)
  ]

edges' :: [(V3 Double, V3 Double)]
edges' = map (both (vertices'' !!)) edges

redges' :: [(V3 Double, V3 Double)]
redges' = map (both (rvertices'' !!)) edges
