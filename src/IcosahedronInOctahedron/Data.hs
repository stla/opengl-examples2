module IcosahedronInOctahedron.Data
  where
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (Vector3 (..))
import           Linear (V3 (..))
import           Utils.Triplets

overtices :: [(Double,Double,Double)]
overtices = [
    ( 0,  1,  0)
  , ( 1,  0,  0)
  , ( 0,  0, -1)
  , (-1,  0,  0)
  , ( 0,  0,  1)
  , ( 0, -1,  0)
  ]

ivertices :: [(Double,Double,Double)]
ivertices = [ 
    ( a,  b,  0),
    (-a,  b,  0),
    ( 0,  a, -b),
    ( b,  0, -a),
    ( 0,  a,  b),
    (-b,  0, -a),
    ( b,  0,  a),
    (-b,  0,  a),
    ( 0, -a, -b),
    ( 0, -a,  b),
    (-a, -b,  0),
    ( a, -b,  0) ]
    where
        b = (sqrt 5 - 1) / 2
        a = 1 - b

ivertices' :: [Vector3 Double]
ivertices' = map tripletToVector3 ivertices

ivertices'' :: [V3 Double]
ivertices'' = map tripletToV3 ivertices

overtices' :: [Vector3 Double]
overtices' = map tripletToVector3 overtices

overtices'' :: [V3 Double]
overtices'' = map tripletToV3 overtices

iedges :: [(Int,Int)]
iedges = [
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

oedges :: [(Int,Int)]
oedges = [
  (0,1),
  (0,2),
  (0,3),
  (0,4),
  (1,2),
  (1,4),
  (1,5),
  (2,3),
  (2,5),
  (3,4),
  (3,5),
  (4,5)
  ]

iedges' :: [(V3 Double, V3 Double)]
iedges' = map (both (ivertices'' !!)) iedges

oedges' :: [(V3 Double, V3 Double)]
oedges' = map (both (overtices'' !!)) oedges

ifacets :: [(Int,Int,Int)]
ifacets = 
  [ ( 0, 1, 2)
  , ( 1, 0, 4)
  , ( 1, 5, 2)
  , ( 0, 2, 3)
  , ( 0, 3, 6)
  , ( 0, 6, 4)
  , ( 1, 4, 7)
  , ( 1, 7, 5)
  , ( 2, 5, 8)
  , ( 2, 8, 3)
  , ( 3, 8,11)
  , ( 3,11, 6)
  , ( 4, 6, 9)
  , ( 4, 9, 7)
  , ( 5, 7,10)
  , ( 5,10, 8)
  , ( 6,11, 9)
  , ( 7, 9,10)
  , ( 8,10,11)
  , ( 9,11,10) ]