module CompoundFiveOctahedra.Data
  where
import           Data.List                    (nub)
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (Vector3 (..))
import           Linear (V3 (..))
import           Math.Combinat.Permutations   (permuteList, isEvenPermutation, 
                                               permutations)
import           Utils.Triplets

signs :: (Eq a, Num a) => [a] -> [[a]]
signs [] = [[]]
signs (x : xs)
  = let ps = signs xs
    in nub $ map (x :) ps ++ map ((-x) :) ps

phi :: Double
phi = (1+sqrt 5)/2

vertices :: [(Double,Double,Double)]
vertices = map toTriplet $ 
           concatMap 
           (\coord -> zipWith permuteList perms (replicate 3 coord))
           (signs [0,0,phi]) ++
           concatMap 
           (\coord -> zipWith permuteList perms (replicate 3 coord))
           (signs [1/2,phi/2,phi*phi/2])  
    where
    perms = filter isEvenPermutation (permutations 3)
    toTriplet l = (l!!0,l!!1,l!!2)

vertices1,vertices2,vertices3,vertices4,vertices5 :: [Vector3 Double]
vertices1 = map (tripletToVector3 . (vertices !!)) [0,1,2,3,4,5]
vertices2 = map (tripletToVector3 . (vertices !!)) [6,10,14,23,25,27]
vertices3 = map (tripletToVector3 . (vertices !!)) [7,11,12,21,26,28]
vertices4 = map (tripletToVector3 . (vertices !!)) [8,9,13,22,24,29]
vertices5 = map (tripletToVector3 . (vertices !!)) [15,16,17,18,19,20]

edges :: [[(Int,Int)]]
edges = [ 
  [ ( 0 , 1 )
  , ( 0 , 2 )
  , ( 0 , 4 )
  , ( 0 , 5 )
  , ( 1 , 2 )
  , ( 1 , 3 )
  , ( 1 , 5 )
  , ( 2 , 3 )
  , ( 2 , 4 )
  , ( 3 , 4 )
  , ( 3 , 5 )
  , ( 4 , 5 )
  ]
  , [ 
    ( 6 , 10 )
  , ( 6 , 14 )
  , ( 6 , 23 )
  , ( 6 , 25 )
  , ( 10 , 14 )
  , ( 10 , 23 )
  , ( 10 , 27 )
  , ( 14 , 25 )
  , ( 14 , 27 )
  , ( 23 , 25 )
  , ( 23 , 27 )
  , ( 25 , 27 )
  ]
  , [ 
    ( 7 , 11 )
  , ( 7 , 12 )
  , ( 7 , 21 )
  , ( 7 , 26 )
  , ( 11 , 12 )
  , ( 11 , 21 )
  , ( 11 , 28 )
  , ( 12 , 26 )
  , ( 12 , 28 )
  , ( 21 , 26 )
  , ( 21 , 28 )
  , ( 26 , 28 )
  ]
  , [ 
    ( 8 , 9 )
  , ( 8 , 13 )
  , ( 8 , 22 )
  , ( 8 , 24 )
  , ( 9 , 13 )
  , ( 9 , 22 )
  , ( 9 , 29 )
  , ( 13 , 24 )
  , ( 13 , 29 )
  , ( 22 , 24 )
  , ( 22 , 29 )
  , ( 24 , 29 )
  ]
  , [ 
    ( 15 , 16 )
  , ( 15 , 17 )
  , ( 15 , 19 )
  , ( 15 , 20 )
  , ( 16 , 17 )
  , ( 16 , 18 )
  , ( 16 , 20 )
  , ( 17 , 18 )
  , ( 17 , 19 )
  , ( 18 , 19 )
  , ( 18 , 20 )
  , ( 19 , 20 )
  ]
  ]

edges' :: [[(V3 Double, V3 Double)]]
edges' = map (map (both (tripletToV3 . (vertices !!)))) edges