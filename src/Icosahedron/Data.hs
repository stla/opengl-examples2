module Icosahedron.Data where

vertices :: [(Double,Double,Double)]
vertices = [
    ( 0,  a,  b)
  , ( 0,  a, -b)
  , ( 0, -a,  b)
  , ( 0, -a, -b)
  , ( a,  b,  0)
  , ( a, -b,  0)
  , (-a,  b,  0)
  , (-a, -b,  0)
  , ( b,  0,  a)
  , (-b,  0,  a)
  , ( b,  0, -a)
  , (-b,  0, -a)  
  ]
  where
    phi = (sqrt 5 -1)/2
    r = sqrt(2 - phi)
    a = phi / r
    b = 1 / r

facets :: [(Int,Int,Int)]
facets = [
    ( 0,  2,  8)
  , ( 0,  8,  4)
  , ( 0,  4,  6)
  , ( 0,  6,  9)
  , ( 0,  9,  2)
  , ( 3, 11,  1)
  , ( 3,  1, 10)
  , ( 3, 10,  5)
  , ( 3,  5,  7)
  , ( 3,  7, 11)
  , ( 8,  2,  5)
  , ( 4,  8, 10)
  , ( 6,  4,  1)
  , ( 9,  6, 11)
  , ( 2,  9,  7)
  , ( 1, 11,  6)
  , (10,  1,  4)
  , ( 5, 10,  8)
  , ( 7,  5,  2)
  , (11,  7,  9)
  ]