module Utils.SphericalTriangle where 
import           Utils.Triplets

splitTriangle :: Floating a => ((a,a,a),(a,a,a),(a,a,a)) 
              -> [((a,a,a),(a,a,a),(a,a,a))]
splitTriangle (u, v, w) = [tr1, tr2, tr3, tr4]
  where
    muv = normalize $ middle u v
    muw = normalize $ middle u w
    mvw = normalize $ middle v w
    tr1 = (u, muv, muw)
    tr2 = (v, mvw, muv)
    tr3 = (w, muw, mvw)
    tr4 = (muv, mvw, muw)

stMesh :: Floating a => Int -> ((a,a,a),(a,a,a),(a,a,a))
        -> [((a,a,a),(a,a,a),(a,a,a))]
stMesh n v1v2v3 = iterate (concatMap splitTriangle) [v1v2v3] !! n


