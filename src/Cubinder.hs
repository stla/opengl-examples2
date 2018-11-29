module Cubinder
  where
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (when, forM_)
import           Cubinder.Data
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.Tuple.Extra                  (both)
import           Data.Vector                       ((!))
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Linear                            (V3 (..))
import           System.Directory                  (doesDirectoryExist)
import           System.IO.Unsafe                  (unsafePerformIO)
import           Text.Printf
import           Utils.ConeMesh

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextAlpha     :: IORef Double
    , contextZoom      :: IORef GLdouble
    }

white,black,whitesmoke :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
whitesmoke = Color4 0.96 0.96 0.96    1

toVector3 :: Floating a => (a,a,a) -> Vector3 a
toVector3 (x,y,z) = Vector3 x y z

toVx3 :: Floating a => (a,a,a) -> Vertex3 a
toVx3 (x,y,z) = Vertex3 x y z

toV3 :: Floating a => (a,a,a) -> V3 a
toV3 (x,y,z) = V3 x y z

toN3 :: Floating a => (a,a,a) -> Normal3 a
toN3 (x,y,z) = Normal3 x y z

stereog :: Double -> (Double,Double,Double,Double) -> (Double,Double,Double)
stereog r (x1,x2,x3,x4) = (x1 / (r-x4), x2 / (r-x4), x3 / (r-x4))

rightIsoclinic :: Double -> Double -> Double 
               -> (Double,Double,Double,Double) -> (Double,Double,Double,Double) 
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

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  alpha <- get (contextAlpha context)
  let points  = map (rightIsoclinic 0 0 (alpha * pi / 180)) vertices
      ppoints = map (stereog (sqrt 1.25)) points
      vectors = map toVector3 ppoints
      edges'  = map (both (toV3 . (!!) ppoints)) edges
      meshesAndMatrices = 
          map (\(pt,pt') -> coneMesh pt pt' 0.08 0.08 3 30) edges'
  loadIdentity
  (_, size) <- get viewport
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  forM_ vectors $ \vec -> preservingMatrix $ do
    translate vec
    materialDiffuse Front $= whitesmoke
    renderObject Solid $ Sphere' 0.1 15 15
  forM_ meshesAndMatrices $ \meshAndMatrix -> 
    preservingMatrix $ do
      m <- newMatrix RowMajor (snd meshAndMatrix) :: IO (GLmatrix Double)
      multMatrix m
      forM_ ((snd . fst) meshAndMatrix) $ \(i,j,k,l) ->
        renderPrimitive Quads $ do
          materialDiffuse Front $= whitesmoke
          drawQuad i j k l ((fst . fst) meshAndMatrix)
  swapBuffers
  where
    drawQuad i j k l verticesAndNormals = do 
        normal ni
        vertex vi
        normal nj
        vertex vj
        normal nk
        vertex vk
        normal nl
        vertex vl
        where
          (vi',ni') = verticesAndNormals ! i
          (vj',nj') = verticesAndNormals ! j
          (vk',nk') = verticesAndNormals ! k
          (vl',nl') = verticesAndNormals ! l
          vi = toVx3 vi'
          ni = toN3 ni'
          vj = toVx3 vj'
          nj = toN3 nj'
          vk = toVx3 vk'
          nk = toN3 nk'
          vl = toVx3 vl'
          nl = toN3 nl'
      
resize :: GLdouble -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (realToFrac w / realToFrac h) 1.0 100.0
  lookAt (Vertex3 0 0 (6.3+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Bool -- animation
         -> IORef Bool -- save
         -> IORef Int -- delay
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim save delay char _ = do
  case char of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+0.1)
    'l' -> zoom $~! subtract 0.1
    'a' -> anim $~! not
    's' -> save $~! not
    'o' -> delay $~! (+10000)
    'p' -> delay $~! (\d -> if d==0 then 0 else d-10000)
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle :: IORef Bool -> IORef Bool -> IORef Int -> IORef Int -> IORef Double 
     -> IdleCallback
idle anim save delay snapshots alpha = do
  an <- get anim
  snapshot <- get snapshots
  s <- get save
  when an $ do
    d <- get delay
    when (s && ppmExists && snapshot < 360) $ do
      let ppm = printf "ppm/cubinder%04d.ppm" snapshot
      (>>=) capturePPM (B.writeFile ppm)
      print snapshot
      snapshots $~! (+1)
    alpha $~! (+1)
    _ <- threadDelay d
    postRedisplay Nothing
  return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Twenty cones"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialSpecular Front $= white
  materialShininess Front $= 50
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 100 1
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  alpha <- newIORef 0.0
  anim <- newIORef False
  save <- newIORef False
  delay <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextAlpha = alpha,
                                      contextZoom = zoom}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= 
    Just (keyboard rot1 rot2 rot3 zoom anim save delay)
  snapshot <- newIORef 0
  idleCallback $= Just (idle anim save delay snapshot alpha)
  putStrLn "*** Cubinder ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop