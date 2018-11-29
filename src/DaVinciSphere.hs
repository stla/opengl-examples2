module DaVinciSphere
  where
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (when, forM_)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.Vector                       (Vector, (!))
import           DaVinciSphere.Data
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           System.Directory                  (doesDirectoryExist)
import           System.IO.Unsafe                  (unsafePerformIO)
import           Text.Printf
import           Utils.Colors.Color                (infinityOfColors)
import           Utils.ConeMesh

type Mesh = (Vector ((Double,Double,Double),(Double,Double,Double)), 
             [(Int,Int,Int,Int)])

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextAlpha     :: IORef GLfloat
    , contextZoom      :: IORef GLdouble
    }

white,black,whitesmoke :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
whitesmoke = Color4 0.96 0.96 0.96    1

toVx3 :: Floating a => (a,a,a) -> Vertex3 a
toVx3 (x,y,z) = Vertex3 x y z

toN3 :: Floating a => (a,a,a) -> Normal3 a
toN3 (x,y,z) = Normal3 x y z

triangleNormal :: Floating a => (Vertex3 a, Vertex3 a, Vertex3 a) -> Normal3 a
triangleNormal (Vertex3 x1 x2 x3, Vertex3 y1 y2 y3, Vertex3 z1 z2 z3) =
  Normal3 (a/norm) (b/norm) (c/norm)
  where
    (a, b, c) = crossProd (y1-x1, y2-x2, y3-x3) (z1-x1, z2-x2, z3-x3)
    crossProd (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
    norm = sqrt (a*a + b*b + c*c)

meshesAndMatrices :: [(Mesh, [Double])]
meshesAndMatrices = 
    map (\(pt,pt') -> coneMesh pt pt' 0.08 0.08 3 30) edges

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  alpha <- get (contextAlpha context)
  loadIdentity
  (_, size) <- get viewport
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  rotate alpha $ Vector3 1 1 1
  forM_ (zip triangles (infinityOfColors 0 0.2)) $ \(face,col) -> 
    renderPrimitive Triangles $ do
      materialDiffuse Front $= col
      drawTriangle face
  forM_ (zip quads (infinityOfColors (length triangles) 0.2)) $ \(face,col) -> 
    renderPrimitive Quads $ do
      materialDiffuse Front $= col
      drawQuad face 
  forM_ vertices $ \vec -> preservingMatrix $ do
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
          drawQuad' i j k l ((fst . fst) meshAndMatrix)
  swapBuffers
  where
    drawTriangle (v1,v2,v3) = do
      normal $ triangleNormal (v1, v3, v2)
      vertex v1
      vertex v2
      vertex v3
    drawQuad (v1,v2,v3,v4) = do
      normal $ triangleNormal (v1, v3, v2)
      vertex v1
      vertex v2
      vertex v3
      vertex v4
    drawQuad' i j k l verticesAndNormals = do 
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

idle :: IORef Bool -> IORef Bool -> IORef Int -> IORef Int -> IORef GLfloat
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
  _ <- createWindow "da Vinci's 72-sided sphere"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer, WithAlphaComponent]
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  clearColor $= Color4 0 0 0 0
--   materialEmission Front $= black
  materialSpecular Front $= white
  materialShininess Front $= 50
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 100 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
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