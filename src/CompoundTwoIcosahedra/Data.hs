module CompoundTwoIcosahedra.Data
  where
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (Vector3 (..))
import           Linear (V3 (..))
import           Utils.Triplets

vertices0 :: [(Double,Double,Double)]
vertices0 = [ 
    (a, b, 0),
    (-a, b, 0),
    (0, a, -b),
    (b, 0, -a),
    (0, a, b),
    (-b, 0, -a),
    (b, 0, a),
    (-b, 0, a),
    (0, -a, -b),
    (0, -a, b),
    (-a, -b, 0),
    (a, -b, 0) ]
    where
        b = (sqrt 5 - 1) / 2
        a = 1 - b

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
  (0,6),
  (1,2),
  (1,4),
  (1,5),
  (1,7),
  (2,3),
  (2,5),
  (2,8),
  (3,6),
  (3,8),
  (3,11),
  (4,6),
  (4,7),
  (4,9),
  (5,7),
  (5,8),
  (5,10),
  (6,9),
  (6,11),
  (7,9),
  (7,10),
  (8,10),
  (8,11),
  (9,10),
  (9,11),
  (10,11)
  ]

edges' :: [(V3 Double, V3 Double)]
edges' = map (both (vertices'' !!)) edges

redges' :: [(V3 Double, V3 Double)]
redges' = map (both (rvertices'' !!)) edges
