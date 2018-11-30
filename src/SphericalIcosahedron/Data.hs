module SphericalIcosahedron.Data where
import Graphics.Rendering.OpenGL.GL
import Icosahedron.Data
import Utils.SphericalTriangle
import Utils.Triplets

n :: Int
n = 5

type Point = (Double,Double,Double)
type NPoint = (Vertex3 Double, Normal3 Double)
type NTriangle = (NPoint,NPoint,NPoint)

triangles :: [[(Point,Point,Point)]] 
triangles = 
    map (stMesh n) (map (\(i,j,k) -> (vertices!!i,vertices!!j,vertices!!k))
                    facets)

ntriangles :: [[NTriangle]]
ntriangles = 
    map (map (\(u,v,w) -> ((tripletToVertex3 u, tripletToNormal3 u),
                           (tripletToVertex3 v, tripletToNormal3 v),
                           (tripletToVertex3 w, tripletToNormal3 w)))) triangles