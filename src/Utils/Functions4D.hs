module Utils.Functions4D where

stereog :: Floating a => a -> (a,a,a,a) -> (a,a,a)
stereog r (x1,x2,x3,x4) = (x1 / (r-x4), x2 / (r-x4), x3 / (r-x4))

mstereog :: Floating a => a -> (a,a,a,a) -> (a,a,a)
mstereog r (x1,x2,x3,x4) = 
  (ad*x1, ad*x2, ad*x3) 
  where
    ad = acos(x4/sqrt r)/pi/sqrt(r-x4*x4)

rightIsoclinic :: Floating a => a -> a -> a -> (a,a,a,a) -> (a,a,a,a) 
rightIsoclinic theta phi alpha (x0,x1,x2,x3) =
  ( q0*x0 - q1*x1 - q2*x2 - q3*x3
  , q1*x0 + q0*x1 + q3*x2 - q2*x3
  , q2*x0 - q3*x1 + q0*x2 + q1*x3
  , q3*x0 + q2*x1 - q1*x2 + q0*x3 )
  where
    q0 = cos alpha
    q1 = sin theta * cos phi * sin alpha
    q2 = sin theta * sin phi * sin alpha
    q3 = cos theta * sin alpha
