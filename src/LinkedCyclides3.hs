module LinkedCyclides3
  (main)
  where
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           LinkedCyclides.Data3
import           System.Directory                  (doesDirectoryExist)
import           System.IO.Unsafe
import           Text.Printf

n_u,n_v :: Int
n_u = 250
n_v = 250

type NPoint = (Vertex3 GLfloat, Normal3 GLfloat, Color4 GLfloat)
type NTriangle = (NPoint, NPoint, NPoint)
type PairedTriangles = (NTriangle,NTriangle)

cyclide :: String -> Float -> Float -> [PairedTriangles]
cyclide = allTriangles (n_u,n_v)

data Context = Context
  {
    contextRot1      :: IORef GLfloat
  , contextRot2      :: IORef GLfloat
  , contextRot3      :: IORef GLfloat
  , contextAlpha     :: IORef GLfloat
  , contextZoom      :: IORef GLdouble
  , contextTriangles :: IORef ([PairedTriangles],[PairedTriangles],[PairedTriangles])
  }

white,discord :: Color4 GLfloat
white      = Color4    1    1    1 1
discord    = Color4 0.21 0.22 0.25 1

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  alpha <- get (contextAlpha context)
  zoom <- get (contextZoom context)
  (triangles1,triangles2,triangles3) <- get (contextTriangles context)
  let (lowerTriangles1,upperTriangles1) = unzip triangles1
      (lowerTriangles2,upperTriangles2) = unzip triangles2
      (lowerTriangles3,upperTriangles3) = unzip triangles3
  loadIdentity
  (_, size) <- get viewport
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  rotate alpha $ Vector3 1 1 1
  renderPrimitive Triangles $ 
    mapM_ drawTriangle lowerTriangles1
  renderPrimitive Triangles $ 
    mapM_ drawTriangle upperTriangles1
  renderPrimitive Triangles $ 
    mapM_ drawTriangle lowerTriangles2
  renderPrimitive Triangles $ 
    mapM_ drawTriangle upperTriangles2
  renderPrimitive Triangles $ 
    mapM_ drawTriangle lowerTriangles3
  renderPrimitive Triangles $ 
    mapM_ drawTriangle upperTriangles3
  swapBuffers
  where
    drawTriangle ((v1,n1,col1),(v2,n2,col2),(v3,n3,col3)) = do
      materialDiffuse Front $= col1
      normal n1
      vertex v1
      materialDiffuse Front $= col2
      normal n2
      vertex v2
      materialDiffuse Front $= col3
      normal n3
      vertex v3

resize :: GLdouble -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (14+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Float -- phi
         -> IORef Bool -- animation
         -> IORef Bool -- save
         -> IORef Int -- delay
         -> IORef ([PairedTriangles],[PairedTriangles],[PairedTriangles])
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom phi anim save delay triangles char _ = do
  case char of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+0.1)
    'l' -> zoom $~! subtract 0.1
    'h' -> do
      phi $~! (+0.2)
      phi' <- get phi
      writeIORef triangles ( cyclide "viridis" 0 phi'
                           , cyclide "plasma'" (2*pi/3) phi'
                           , cyclide "magma" (4*pi/3) phi')
    'n' -> do
      phi $~! (\x -> if x>0.2 then x-0.2 else x)
      phi' <- get phi
      writeIORef triangles ( cyclide "viridis" 0 phi'
                           , cyclide "plasma'" (2*pi/3) phi'
                           , cyclide "magma" (4*pi/3) phi')
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
  a <- get anim
  snapshot <- get snapshots
  s <- get save
  when a $ do
    d <- get delay
    when (s && ppmExists && snapshot < 360) $ do
      let ppm = printf "ppm/linkedcyclides%04d.ppm" snapshot
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
  _ <- createWindow "Linked cyclides"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= discord
  materialSpecular Front $= white
  materialShininess Front $= 50
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  alpha <- newIORef 0.0
  let phi = 1.2
      triangles = ( cyclide "viridis" 0 phi
                  , cyclide "plasma'" (2*pi/3) phi
                  , cyclide "magma" (4*pi/3) phi)
  triangles' <- newIORef triangles
  phi' <- newIORef phi
  anim <- newIORef False
  save <- newIORef False
  delay <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextAlpha = alpha,
                                      contextZoom = zoom,
                                      contextTriangles = triangles'}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= 
    Just (keyboard rot1 rot2 rot3 zoom phi' anim save delay triangles')
  snapshot <- newIORef 0
  idleCallback $= Just (idle anim save delay snapshot alpha)
  putStrLn "*** Linked Cyclides ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease phi: h, n\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop