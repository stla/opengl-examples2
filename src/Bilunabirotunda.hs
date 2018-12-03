module Bilunabirotunda
  where
import           Bilunabirotunda.Data
import           Control.Concurrent                  (threadDelay)
import           Control.Monad                       (when, forM_)
import qualified Data.ByteString                  as B
import           Data.IORef
import           Data.Tuple.Extra                    (both)
import           Data.Vector                         ((!))
import           Graphics.Rendering.OpenGL.Capture   (capturePPM)
import           Graphics.Rendering.OpenGL.GL hiding (normalize)
import           Graphics.UI.GLUT             hiding (normalize)
import           System.Directory                    (doesDirectoryExist)
import           System.IO.Unsafe                    (unsafePerformIO)
import           Text.Printf
import           Utils.ConeMesh
import           Utils.Functions4D
import           Utils.Triplets               hiding (triangleNormal)

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextAlpha     :: IORef Double
    , contextZoom      :: IORef GLdouble
    }

white,black,whitesmoke,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
whitesmoke = Color4 0.96 0.96 0.96    1
red        = Color4    1    0    0  0.5

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  alpha <- get (contextAlpha context)
  let points  = map (rightIsoclinic (pi/4) (pi/4) (pi + alpha * pi / 180)) vertices
      ppoints = map (mstereog 8) points
      vectors = map tripletToVector3 ppoints
      edges'  = map (both (tripletToV3 . (!!) ppoints)) edges
      meshesAndMatrices = 
        map (\(pt,pt') -> coneMesh pt pt' 0.02 0.02 3 30) edges'
  loadIdentity
  (_, size) <- get viewport
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  forM_ vectors $ \vec -> preservingMatrix $ do
    translate vec
    materialDiffuse Front $= whitesmoke
    renderObject Solid $ Sphere' 0.04 15 15
  forM_ meshesAndMatrices $ \meshAndMatrix -> 
    preservingMatrix $ do
      m <- newMatrix RowMajor (snd meshAndMatrix) :: IO (GLmatrix Double)
      multMatrix m
      forM_ ((snd . fst) meshAndMatrix) $ \(i,j,k,l) ->
        renderPrimitive Quads $ do
          materialDiffuse Front $= whitesmoke
          drawQuad i j k l ((fst . fst) meshAndMatrix)
  forM_ tetrahedra $ \(i,j,k,l) -> 
    renderPrimitive Triangles $ do
      let vi = tripletToVertex3 $ ppoints !! i 
          vj = tripletToVertex3 $ ppoints !! j 
          vk = tripletToVertex3 $ ppoints !! k 
          vl = tripletToVertex3 $ ppoints !! l 
          triangleNormal (Vertex3 x1 x2 x3, Vertex3 y1 y2 y3, Vertex3 z1 z2 z3) =
            tripletToNormal3 (normalize $ 
              crossProd (y1-x1, y2-x2, y3-x3) (z1-x1, z2-x2, z3-x3))          
      materialDiffuse FrontAndBack $= red
      normal (triangleNormal (vi,vj,vk))
      vertex vi
      vertex vj
      vertex vk
      normal (triangleNormal (vi,vj,vl))
      vertex vi
      vertex vj
      vertex vl
      normal (triangleNormal (vi,vk,vl))
      vertex vi
      vertex vk
      vertex vl
      normal (triangleNormal (vj,vk,vl))
      vertex vj
      vertex vk
      vertex vl
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
          vi = tripletToVertex3 vi'
          ni = tripletToNormal3 ni'
          vj = tripletToVertex3 vj'
          nj = tripletToNormal3 nj'
          vk = tripletToVertex3 vk'
          nk = tripletToNormal3 nk'
          vl = tripletToVertex3 vl'
          nl = tripletToNormal3 nl'
                  
resize :: GLdouble -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (realToFrac w / realToFrac h) 1.0 100.0
  lookAt (Vertex3 0 0 (1.8+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
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
      let ppm = printf "ppm/bilunabirotunda%04d.ppm" snapshot
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
  _ <- createWindow "Bilunabirotunda pseudopyramid"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  clearColor $= black
  materialSpecular Front $= white
  materialShininess Front $= 50
  lighting $= Enabled
  lightModelTwoSide $= Enabled
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
  putStrLn "*** Bilunabirotunda pseudopyramid ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop