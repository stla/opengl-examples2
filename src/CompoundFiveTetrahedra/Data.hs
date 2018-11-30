module CompoundFiveTetrahedra.Data
  where
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (Vertex3 (..), Vector3 (..))
import           Linear (V3 (..))

vertices0 :: [(Double,Double,Double)]
vertices0 = [ 
    (a, a, a),
    (a, a, -a),
    (a, -a, a),
    (-a, -a, a),
    (-a, a, -a),
    (-a, a, a),
    (0, b, -c),
    (0, -b, -c),
    (0, -b, c),
    (c, 0, -b),
    (-c, 0, -b),
    (-c, 0, b),
    (b, c, 0),
    (b, -c, 0),
    (-b, -c, 0),
    (-b, c, 0),
    (0, b, c),
    (a, -a, -a),
    (c, 0, b),
    (-a, -a, -a) ]
    where
        phi = (1 + sqrt 5) / 2
        a = 1 / sqrt 3
        b = a / phi
        c = a * phi
    
vertices :: [Vector3 Double]
vertices = map toVect3 vertices0
  where
    toVect3 (x,y,z) = Vector3 x y z 

vertices' :: [Vertex3 Double]
vertices' = map toVx3 vertices0
  where
    toVx3 (x,y,z) = Vertex3 x y z 

vertices'' :: [V3 Double]
vertices'' = map toV3 vertices0
  where
    toV3 (x,y,z) = V3 x y z 

tetra1Idxs,tetra2Idxs,tetra3Idxs,tetra4Idxs,tetra5Idxs :: (Int,Int,Int,Int)
tetra1Idxs = (16,13, 1,10)
tetra2Idxs = (17, 0, 3, 4)
tetra3Idxs = (18, 5,14, 6)
tetra4Idxs = ( 2,12,11, 7)
tetra5Idxs = (19,15, 9,15)

tetraEdges :: (Int,Int,Int,Int) -> [(Int,Int)]
tetraEdges (i,j,k,l) = [(i,j),(i,k),(i,l),(j,k),(j,l),(k,l)]

edges1,edges2,edges3,edges4,edges5 :: [(V3 Double, V3 Double)]
edges1 = map (both (vertices'' !!)) (tetraEdges tetra1Idxs)
edges2 = map (both (vertices'' !!)) (tetraEdges tetra2Idxs)
edges3 = map (both (vertices'' !!)) (tetraEdges tetra3Idxs)
edges4 = map (both (vertices'' !!)) (tetraEdges tetra4Idxs)
edges5 = map (both (vertices'' !!)) (tetraEdges tetra5Idxs)

vertices1,vertices2,vertices3,vertices4,vertices5 :: [Vector3 Double]
vertices1 = let (i,j,k,l) = tetra1Idxs in 
    map (vertices !!) [i,j,k,l]
vertices2 = let (i,j,k,l) = tetra2Idxs in 
    map (vertices !!) [i,j,k,l]
vertices3 = let (i,j,k,l) = tetra3Idxs in 
    map (vertices !!) [i,j,k,l]
vertices4 = let (i,j,k,l) = tetra4Idxs in 
    map (vertices !!) [i,j,k,l]
vertices5 = let (i,j,k,l) = tetra5Idxs in 
            map (vertices !!) [i,j,k,l]
