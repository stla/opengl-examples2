module Cubinder.Data where
import Data.Maybe (isJust, fromJust)

n1,n2 :: Int
n1 = 30
n2 = 4

vertices :: [(Double,Double,Double,Double)]
vertices = map flatten [(u,v) | u <- poly1, v <- poly2]
  where
  toDbl :: Int -> Double
  toDbl = fromIntegral
  poly1 = [(cos(i*2*pi / toDbl n1),sin(i*2*pi / toDbl n1)) | 
           i <- map toDbl [0 .. n1-1]]
  poly2 = [(cos(i*2*pi / toDbl n2),sin(i*2*pi / toDbl n2)) | 
           i <- map toDbl [0 .. n2-1]]
  flatten :: ((Double,Double),(Double,Double)) -> (Double,Double,Double,Double)
  flatten ((u1,u2),(v1,v2)) = (u1,u2,v1,v2)

edges_ij :: (Int,Int) -> [(Int,Int)]
edges_ij (i,j) = map fromJust $ filter isJust [e1, e2, e3, e4]
  where
    toInt :: (Int,Int) -> Int
    toInt (k1,k2) = k1*n2 + k2
    e = (i, j)
    c1 = (i, (j-1) `mod` n2)
    e1 = if e<c1 then Just (toInt e, toInt c1) else Nothing
    c2 = (i, (j+1) `mod` n2)
    e2 = if e<c2 then Just (toInt e, toInt c2) else Nothing
    c3 = ((i-1) `mod` n1, j)
    e3 = if e<c3 then Just (toInt e, toInt c3) else Nothing
    c4 = ((i+1) `mod` n1, j)
    e4 = if e<c4 then Just (toInt e, toInt c4) else Nothing

edges :: [(Int, Int)]
edges = concatMap edges_ij [(i,j) | i <- [0 .. n1-1], j <- [0 .. n2-1]]
